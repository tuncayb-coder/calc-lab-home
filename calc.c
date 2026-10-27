// Name: Tunjay Bayramzade StudentID: 251ADB234
// GitHub: https://github.com/your-username/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used:
// AI: ChatGPT (OpenAI), conversation history dated 2026-10-27.
// Used for guidance on C parsing, arithmetic, file handling and testing.
// Official C reference: https://en.cppreference.com/w/c
// AI-assisted code must be understood and verified by the student.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>

#define STUDENT_NAME "tunjay"
#define STUDENT_LASTNAME "bayramzade"
#define STUDENT_ID "251ADB234"

typedef struct {
    int is_float;
    long long integer;
    double real;
} Value;

typedef struct {
    const char *text;
    size_t length;
    size_t pos;
    size_t line_start;
    size_t error;
} Parser;

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} Buffer;

/* Positions are counted from the beginning of the original file.
   Keeping line_start lets the parser report the correct file position. */
static size_t file_position(const Parser *p, size_t local_pos) {
    return p->line_start + local_pos + 1;
}

/* Grow buffers gradually instead of copying the whole file repeatedly. */
static int reserve_buffer(Buffer *b, size_t needed) {
    if (needed <= b->capacity) {
        return 1;
    }

    size_t capacity = b->capacity ? b->capacity : 256;

    while (capacity < needed) {
        if (capacity > (size_t)-1 / 2) {
            capacity = needed;
            break;
        }
        capacity *= 2;
    }

    char *temp = realloc(b->data, capacity);
    if (temp == NULL) {
        return 0;
    }

    b->data = temp;
    b->capacity = capacity;
    return 1;
}

static int append_text(Buffer *b, const char *text, size_t length) {
    if (length > (size_t)-1 - b->length - 1) {
        return 0;
    }

    if (!reserve_buffer(b, b->length + length + 1)) {
        return 0;
    }

    memcpy(b->data + b->length, text, length);
    b->length += length;
    b->data[b->length] = '\0';

    return 1;
}

static int append_result(Buffer *b, Value value) {
    char result[128];
    int length;

    if (value.is_float) {
        length = snprintf(result, sizeof result, "%.15g\n", value.real);
    } else {
        length = snprintf(result, sizeof result, "%lld\n", value.integer);
    }

    if (length < 0 || (size_t)length >= sizeof result) {
        return 0;
    }

    return append_text(b, result, (size_t)length);
}

static void skip_spaces(Parser *p) {
    while (p->pos < p->length &&
           isspace((unsigned char)p->text[p->pos])) {
        p->pos++;
    }
}

static Value integer_value(long long number) {
    Value v;
    v.is_float = 0;
    v.integer = number;
    v.real = (double)number;
    return v;
}

static Value float_value(double number) {
    Value v;
    v.is_float = 1;
    v.integer = 0;
    v.real = number;
    return v;
}

static Value parse_expression(Parser *p);
static Value parse_term(Parser *p);
static Value parse_power(Parser *p);
static Value parse_primary(Parser *p);

/* Numbers must start with a digit. A decimal point must have digits
   on both sides, so .5 and 5. are rejected. */
static Value parse_primary(Parser *p) {
    skip_spaces(p);

    if (p->pos >= p->length) {
        p->error = file_position(p, p->length);
        return integer_value(0);
    }

    char c = p->text[p->pos];

    if (c == '(') {
        p->pos++;
        Value value = parse_expression(p);

        if (p->error != 0) {
            return integer_value(0);
        }

        skip_spaces(p);

        if (p->pos >= p->length) {
            p->error = file_position(p, p->length);
            return integer_value(0);
        }

        if (p->text[p->pos] != ')') {
            p->error = file_position(p, p->pos);
            return integer_value(0);
        }

        p->pos++;
        return value;
    }

    if (!isdigit((unsigned char)c)) {
        p->error = file_position(p, p->pos);
        return integer_value(0);
    }

    size_t start = p->pos;
    long long integer = 0;
    double real = 0.0;

    while (p->pos < p->length &&
           isdigit((unsigned char)p->text[p->pos])) {
        int digit = p->text[p->pos] - '0';

        integer = integer * 10 + digit;
        real = real * 10.0 + digit;

        p->pos++;
    }

    if (p->pos < p->length && p->text[p->pos] == '.') {
        size_t dot = p->pos;
        p->pos++;

        if (p->pos >= p->length ||
            !isdigit((unsigned char)p->text[p->pos])) {
            p->error = file_position(p, dot);
            return integer_value(0);
        }

        double place = 0.1;

        while (p->pos < p->length &&
               isdigit((unsigned char)p->text[p->pos])) {
            real += (p->text[p->pos] - '0') * place;
            place *= 0.1;
            p->pos++;
        }

        if (!isfinite(real)) {
            p->error = file_position(p, start);
            return integer_value(0);
        }

        return float_value(real);
    }

    return integer_value(integer);
}

/* Integer powers stay integers. Repeated squaring avoids unnecessary
   work when the exponent is large. */
static long long integer_power(long long base, long long exponent) {
    long long result = 1;

    while (exponent > 0) {
        if (exponent % 2 != 0) {
            result *= base;
        }

        exponent /= 2;

        if (exponent > 0) {
            base *= base;
        }
    }

    return result;
}

/* Calling parse_power() for the exponent makes ** right-associative:
   2 ** 3 ** 2 is parsed as 2 ** (3 ** 2). */
static Value parse_power(Parser *p) {
    Value left = parse_primary(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    skip_spaces(p);

    if (p->pos + 1 >= p->length ||
        p->text[p->pos] != '*' ||
        p->text[p->pos + 1] != '*') {
        return left;
    }

    size_t operator_pos = p->pos;
    p->pos += 2;

    skip_spaces(p);
    size_t exponent_pos = p->pos;

    Value right = parse_power(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    /* Negative integer exponents are invalid for integer powers. */
    if (!left.is_float && !right.is_float && right.integer < 0) {
        p->error = file_position(p, exponent_pos);
        return integer_value(0);
    }

    if (!left.is_float && !right.is_float) {
        return integer_value(integer_power(left.integer, right.integer));
    }

    double result = pow(left.is_float ? left.real : (double)left.integer,
                        right.is_float ? right.real : (double)right.integer);

    if (!isfinite(result)) {
        p->error = file_position(p, operator_pos);
        return integer_value(0);
    }

    return float_value(result);
}

/* Multiplication and division have higher precedence than addition.
   Division by zero is reported at the beginning of its right operand. */
static Value parse_term(Parser *p) {
    Value left = parse_power(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    while (1) {
        skip_spaces(p);

        if (p->pos >= p->length) {
            break;
        }

        char op = p->text[p->pos];

        if (op != '*' && op != '/') {
            break;
        }

        size_t operator_pos = p->pos;
        p->pos++;

        skip_spaces(p);
        size_t right_pos = p->pos;

        Value right = parse_power(p);

        if (p->error != 0) {
            return integer_value(0);
        }

        if (op == '/' &&
            ((right.is_float && right.real == 0.0) ||
             (!right.is_float && right.integer == 0))) {
            p->error = file_position(p, right_pos);
            return integer_value(0);
        }

        if (!left.is_float && !right.is_float) {
            if (op == '*') {
                left = integer_value(left.integer * right.integer);
            } else {
                left = integer_value(left.integer / right.integer);
            }
        } else {
            double a = left.is_float ? left.real : (double)left.integer;
            double b = right.is_float ? right.real : (double)right.integer;

            double result = (op == '*') ? a * b : a / b;

            if (!isfinite(result)) {
                p->error = file_position(p, operator_pos);
                return integer_value(0);
            }

            left = float_value(result);
        }
    }

    return left;
}

/* Addition and subtraction have the lowest precedence. */
static Value parse_expression(Parser *p) {
    Value left = parse_term(p);

    if (p->error != 0) {
        return integer_value(0);
    }

    while (1) {
        skip_spaces(p);

        if (p->pos >= p->length) {
            break;
        }

        char op = p->text[p->pos];

        if (op != '+' && op != '-') {
            break;
        }

        p->pos++;

        Value right = parse_term(p);

        if (p->error != 0) {
            return integer_value(0);
        }

        if (!left.is_float && !right.is_float) {
            if (op == '+') {
                left = integer_value(left.integer + right.integer);
            } else {
                left = integer_value(left.integer - right.integer);
            }
        } else {
            double a = left.is_float ? left.real : (double)left.integer;
            double b = right.is_float ? right.real : (double)right.integer;

            double result = (op == '+') ? a + b : a - b;

            if (!isfinite(result)) {
                p->error = file_position(p, p->pos - 1);
                return integer_value(0);
            }

            left = float_value(result);
        }
    }

    return left;
}

/* A line is parsed completely before its result is accepted.
   This prevents valid prefixes from hiding invalid trailing characters. */
static int evaluate_line(const char *text, size_t length,
                         size_t line_start, size_t eof_position,
                         Value *result, size_t *error) {
    Parser p = {text, length, 0, line_start, 0};

    skip_spaces(&p);

    if (p.pos == p.length) {
        return 0;
    }

    if (text[p.pos] == '#') {
        return 0;
    }

    *result = parse_expression(&p);

    if (p.error != 0) {
        *error = p.error;
        return -1;
    }

    skip_spaces(&p);

    if (p.pos < p.length) {
        p.error = file_position(&p, p.pos);
        *error = p.error;
        return -1;
    }

    /* A non-terminated final line uses file length + 1 for missing tokens. */
    (void)eof_position;
    return 1;
}

/* Read large files in chunks so the input does not need a fixed size. */
static char *read_file(const char *filename, size_t *length) {
    FILE *fp = fopen(filename, "rb");

    if (fp == NULL) {
        return NULL;
    }

    Buffer b = {NULL, 0, 0};
    char chunk[8192];

    while (1) {
        size_t n = fread(chunk, 1, sizeof chunk, fp);

        if (n > 0 && !append_text(&b, chunk, n)) {
            free(b.data);
            fclose(fp);
            return NULL;
        }

        if (n < sizeof chunk) {
            if (ferror(fp)) {
                free(b.data);
                fclose(fp);
                return NULL;
            }
            break;
        }
    }

    fclose(fp);

    if (b.data == NULL) {
        b.data = malloc(1);

        if (b.data == NULL) {
            return NULL;
        }

        b.data[0] = '\0';
    }

    *length = b.length;
    return b.data;
}

static int evaluate_file(const char *filename, Buffer *output) {
    size_t length = 0;
    char *data = read_file(filename, &length);

    if (data == NULL) {
        fprintf(stderr, "Cannot read input file: %s\n", filename);
        return -1;
    }

    size_t start = 0;
    size_t error = 0;

    while (start < length) {
        size_t end = start;

        while (end < length && data[end] != '\n') {
            end++;
        }

        size_t line_length = end - start;
        Value value;

        int status = evaluate_line(data + start, line_length,
                                   start, length + 1, &value, &error);

        if (status < 0) {
            break;
        }

        if (status > 0 && !append_result(output, value)) {
            free(data);
            return -1;
        }

        if (end == length) {
            start = length;
        } else {
            start = end + 1;
        }
    }

    free(data);

    if (error != 0) {
        char result[64];
        int n = snprintf(result, sizeof result, "ERROR:%zu\n", error);

        if (n < 0 || !append_text(output, result, (size_t)n)) {
            return -1;
        }
    }

    return 0;
}

static char *copy_string(const char *s) {
    size_t n = strlen(s) + 1;
    char *copy = malloc(n);

    if (copy != NULL) {
        memcpy(copy, s, n);
    }

    return copy;
}

static const char *base_name(const char *path) {
    const char *slash = strrchr(path, '/');
    return slash == NULL ? path : slash + 1;
}

static char *input_base(const char *path, int is_directory) {
    char *base = copy_string(base_name(path));

    if (base == NULL) {
        return NULL;
    }

    size_t length = strlen(base);

    if (!is_directory && length >= 4 &&
        strcmp(base + length - 4, ".txt") == 0) {
        base[length - 4] = '\0';
    }

    return base;
}

static char *join_path(const char *directory, const char *filename) {
    size_t a = strlen(directory);
    size_t b = strlen(filename);

    int slash_needed = a > 0 && directory[a - 1] != '/';

    if (a > (size_t)-1 - b - (size_t)slash_needed - 1) {
        return NULL;
    }

    char *path = malloc(a + b + (size_t)slash_needed + 1);

    if (path == NULL) {
        return NULL;
    }

    snprintf(path, a + b + (size_t)slash_needed + 1,
             "%s%s%s", directory, slash_needed ? "/" : "", filename);

    return path;
}

static int make_directory(const char *path) {
    struct stat st;

    if (stat(path, &st) == 0) {
        if (S_ISDIR(st.st_mode)) {
            return 0;
        }

        fprintf(stderr, "Output path is not a directory: %s\n", path);
        return -1;
    }

    if (mkdir(path, 0777) != 0) {
        fprintf(stderr, "Cannot create output directory: %s\n", path);
        return -1;
    }

    return 0;
}

static int write_output(const char *path, const Buffer *output) {
    FILE *fp = fopen(path, "wb");

    if (fp == NULL) {
        fprintf(stderr, "Cannot write output file: %s\n", path);
        return -1;
    }

    int failed = 0;

    if (output->length > 0 &&
        fwrite(output->data, 1, output->length, fp) != output->length) {
        failed = 1;
    }

    if (fclose(fp) != 0) {
        failed = 1;
    }

    if (failed) {
        fprintf(stderr, "Error writing output file: %s\n", path);
        return -1;
    }

    return 0;
}

static int process_one_file(const char *input, const char *outdir) {
    char *base = input_base(input, 0);

    if (base == NULL) {
        return -1;
    }

    int needed = snprintf(NULL, 0, "%s_%s_%s_%s.txt",
                          base, STUDENT_NAME,
                          STUDENT_LASTNAME, STUDENT_ID);

    if (needed < 0) {
        free(base);
        return -1;
    }

    char *output_name = malloc((size_t)needed + 1);

    if (output_name == NULL) {
        free(base);
        return -1;
    }

    snprintf(output_name, (size_t)needed + 1, "%s_%s_%s_%s.txt",
             base, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

    char *output_path = join_path(outdir, output_name);
    free(output_name);
    free(base);

    if (output_path == NULL) {
        return -1;
    }

    Buffer output = {NULL, 0, 0};
    int status = evaluate_file(input, &output);

    if (status == 0) {
        status = write_output(output_path, &output);
    }

    free(output.data);
    free(output_path);

    return status;
}

static int has_txt_extension(const char *name) {
    size_t n = strlen(name);
    return n >= 4 && strcmp(name + n - 4, ".txt") == 0;
}

int main(int argc, char *argv[]) {
    const char *input = NULL;
    const char *directory = NULL;
    const char *out_option = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 ||
            strcmp(argv[i], "--dir") == 0) {
            if (directory != NULL || i + 1 >= argc) {
                fprintf(stderr, "Invalid directory option.\n");
                return EXIT_FAILURE;
            }

            directory = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 ||
                   strcmp(argv[i], "--output-dir") == 0) {
            if (out_option != NULL || i + 1 >= argc) {
                fprintf(stderr, "Invalid output option.\n");
                return EXIT_FAILURE;
            }

            out_option = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return EXIT_FAILURE;
        } else {
            if (input != NULL) {
                fprintf(stderr, "Too many input arguments.\n");
                return EXIT_FAILURE;
            }

            input = argv[i];
        }
    }

    if ((directory == NULL && input == NULL) ||
        (directory != NULL && input != NULL)) {
        fprintf(stderr,
                "Usage: calc [-d DIR] [-o OUTDIR] [input.txt]\n");
        return EXIT_FAILURE;
    }

    struct stat st;

    if (directory != NULL) {
        if (stat(directory, &st) != 0 || !S_ISDIR(st.st_mode)) {
            fprintf(stderr, "Input directory not found: %s\n", directory);
            return EXIT_FAILURE;
        }
    } else {
        if (stat(input, &st) != 0 || !S_ISREG(st.st_mode)) {
            fprintf(stderr, "Input file not found: %s\n", input);
            return EXIT_FAILURE;
        }
    }

    char *default_out = NULL;
    const char *outdir = out_option;

    if (outdir == NULL) {
        char *base = input_base(directory != NULL ? directory : input,
                                directory != NULL);

        if (base == NULL) {
            return EXIT_FAILURE;
        }

        int needed = snprintf(NULL, 0, "%s_%s_%s",
                              base, STUDENT_NAME, STUDENT_ID);

        if (needed < 0) {
            free(base);
            return EXIT_FAILURE;
        }

        default_out = malloc((size_t)needed + 1);

        if (default_out == NULL) {
            free(base);
            return EXIT_FAILURE;
        }

        snprintf(default_out, (size_t)needed + 1, "%s_%s_%s",
                 base, STUDENT_NAME, STUDENT_ID);

        free(base);
        outdir = default_out;
    }

    if (make_directory(outdir) != 0) {
        free(default_out);
        return EXIT_FAILURE;
    }

    int status = 0;

    if (directory == NULL) {
        status = process_one_file(input, outdir);
    } else {
        DIR *dir = opendir(directory);

        if (dir == NULL) {
            fprintf(stderr, "Cannot open input directory: %s\n", directory);
            free(default_out);
            return EXIT_FAILURE;
        }

        struct dirent *entry;

        while ((entry = readdir(dir)) != NULL) {
            if (!has_txt_extension(entry->d_name)) {
                continue;
            }

            char *path = join_path(directory, entry->d_name);

            if (path == NULL) {
                status = -1;
                break;
            }

            struct stat file_st;

            if (stat(path, &file_st) == 0 && S_ISREG(file_st.st_mode)) {
                if (process_one_file(path, outdir) != 0) {
                    status = -1;
                }
            }

            free(path);

            if (status != 0) {
                break;
            }
        }

        closedir(dir);
    }

    free(default_out);

    return status == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}