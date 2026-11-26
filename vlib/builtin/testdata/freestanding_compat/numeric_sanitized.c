#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <errno.h>
#include <fenv.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
extern long v_bare_strtol(const char *, char **, int);
extern unsigned long v_bare_strtoul(const char *, char **, int);
extern float v_bare_strtof(const char *, char **);
extern long double v_bare_strtold(const char *, char **);
extern int v_bare_initstate_r(unsigned int, char *, size_t, struct random_data *);
extern int v_bare_random_r(struct random_data *, int32_t *);
extern int v_bare_setstate_r(char *, struct random_data *);
extern int v_bare_srandom_r(unsigned int, struct random_data *);
extern double v_bare_strtod(const char *, char **);
extern int v_bare_sscanf(const char *, const char *, ...);
extern int v_bare___isoc23_sscanf(const char *, const char *, ...);
extern intmax_t v_bare___isoc23_strtoimax(const char *, char **, int);
extern uintmax_t v_bare___isoc23_strtoumax(const char *, char **, int);
extern long v_bare___isoc23_strtol(const char *, char **, int);
extern char *v_bare_setlocale(int, const char *);
extern size_t v_bare_mbrtowc(wchar_t *, const char *, size_t, mbstate_t *);
extern size_t v_bare_wcrtomb(char *, wchar_t, mbstate_t *);
extern double v_bare_frexp(double, int *);
extern double v_bare_modf(double, double *);
#define CHECK(x)                                                                                   \
    do {                                                                                           \
        if (!(x)) {                                                                                \
            fprintf(stderr, "failed %d: %s\n", __LINE__, #x);                                      \
            return 1;                                                                              \
        }                                                                                          \
    } while (0)
int main(void) {
    char *end;
    errno = 0;
    CHECK(v_bare_strtol("-9223372036854775808!", &end, 10) == LONG_MIN && *end == '!' && !errno);
    CHECK(v_bare_strtol("9223372036854775808!", &end, 10) == LONG_MAX && *end == '!' &&
          errno == ERANGE);
    CHECK(v_bare_strtol("0x!", &end, 0) == 0 && *end == 'x');
    CHECK(v_bare_strtol("0b101!", &end, 0) == 0 && *end == 'b');
    CHECK(v_bare___isoc23_strtol("0b101!", &end, 0) == 5 && *end == '!');
    CHECK(v_bare___isoc23_strtol("-0B101!", &end, 2) == -5 && *end == '!');
    CHECK(v_bare___isoc23_strtoimax("-0b101!", &end, 0) == -5 && *end == '!');
    CHECK(v_bare___isoc23_strtoumax("0b101!", &end, 0) == 5 && *end == '!');
    int x = -1, n = -1;
    CHECK(v_bare___isoc23_sscanf("0b101!", "%i%n", &x, &n) == 1 && x == 5 && n == 5);
    char b[5];
    double d;
    CHECK(v_bare_sscanf("12 1.25 abcd!", "%d %lf %4[a-z]%n", &x, &d, b, &n) == 3 && x == 12 &&
          d == 1.25 && !strcmp(b, "abcd") && n == 12);
    const char *cases[] = {"",        " ",
                           ".",       "1e",
                           "1e+",     "0x1p",
                           "infinX",  "nan(foo",
                           "0x.8p-4", "1.23456789012345678901234567890123456789",
                           "1e-5000", "1e5000",
                           "-0",      "4.9406564584124654e-324"};
    for (size_t i = 0; i < sizeof cases / sizeof *cases; ++i) {
        const char *s = cases[i];
        char *a, *b;
        double ref = strtod(s, &a), got = v_bare_strtod(s, &b);
        if (memcmp(&ref, &got, 8) || a - s != b - s) {
            fprintf(stderr, "float '%s' expected %.17g @%ld got %.17g @%ld\n", s, ref, a - s, got,
                    b - s);
            return 1;
        }
    }
    uint64_t random = 12345;
    for (int i = 0; i < 100000; ++i) {
        random = random * 6364136223846793005ULL + 1;
        char text[128];
        snprintf(text, sizeof text, "%llu.%018llue%d!", (unsigned long long)random,
                 (unsigned long long)(random >> 6), (int)(random % 660) - 330);
        char *a, *b;
        double ref = strtod(text, &a), got = v_bare_strtod(text, &b);
        CHECK(!memcmp(&ref, &got, 8) && a - text == b - text);
    }
    const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
    for (int base = 2; base <= 36; ++base)
        for (int sample = 0; sample < 1000; ++sample) {
            random = random * 6364136223846793005ULL + 1;
            uint64_t number = random;
            char text[96], *at = text + sizeof text;
            *--at = 0;
            *--at = '!';
            do {
                *--at = digits[number % base];
                number /= base;
            } while (number);
            if (sample & 1)
                *--at = '-';
            char *a, *b;
            errno = 0;
            long expected = strtol(at, &a, base);
            int error = errno;
            errno = 0;
            long actual = v_bare_strtol(at, &b, base);
            CHECK(expected == actual && a - at == b - at && (error == ERANGE) == (errno == ERANGE));
            errno = 0;
            unsigned long uexpected = strtoul(at, &a, base);
            error = errno;
            errno = 0;
            unsigned long uactual = v_bare_strtoul(at, &b, base);
            CHECK(uexpected == uactual && a - at == b - at &&
                  (error == ERANGE) == (errno == ERANGE));
        }
    const int modes[] = {FE_TONEAREST, FE_DOWNWARD, FE_UPWARD, FE_TOWARDZERO};
    for (size_t mode = 0; mode < 4; ++mode) {
        CHECK(fesetround(modes[mode]) == 0);
        for (int sample = 0; sample < 1000; ++sample) {
            random = random * 6364136223846793005ULL + 1;
            char text[128], *a, *b;
            snprintf(text, sizeof text, "%llu.%018llue%d!", (unsigned long long)random,
                     (unsigned long long)(random >> 7), (int)(random % 90) - 45);
            float fref = strtof(text, &a), fgot = v_bare_strtof(text, &b);
            CHECK(!memcmp(&fref, &fgot, sizeof fref) && a - text == b - text);
            double dref = strtod(text, &a), dgot = v_bare_strtod(text, &b);
            CHECK(!memcmp(&dref, &dgot, sizeof dref) && a - text == b - text);
            long double lref = strtold(text, &a), lgot = v_bare_strtold(text, &b);
            CHECK(lref == lgot && a - text == b - text);
        }
    }
    for (size_t mode = 0; mode < 4; ++mode) {
        CHECK(fesetround(modes[mode]) == 0);
        for (int sample = 0; sample < 100000; ++sample) {
            random = random * 6364136223846793005ULL + 1;
            double value;
            memcpy(&value, &random, sizeof value);
            int left_exp = 0, right_exp = 0;
            double left = frexp(value, &left_exp), right = v_bare_frexp(value, &right_exp);
            CHECK((isnan(left) && isnan(right)) ||
                  (!memcmp(&left, &right, sizeof left) && left_exp == right_exp));
            double left_int, right_int;
            left = modf(value, &left_int);
            right = v_bare_modf(value, &right_int);
            CHECK((isnan(left) && isnan(right) && isnan(left_int) && isnan(right_int)) ||
                  (!memcmp(&left, &right, sizeof left) && !memcmp(&left_int, &right_int, sizeof left_int)));
        }
    }
    CHECK(fesetround(FE_TONEAREST) == 0);
    for (size_t size = 8; size <= 256; size = size == 8 ? 32 : size * 2) {
        int32_t first[64], second[64];
        struct random_data native = {0}, bare = {0};
        CHECK(initstate_r(0xfedcba98u, (char *)first, size, &native) == 0);
        CHECK(v_bare_initstate_r(0xfedcba98u, (char *)second, size, &bare) == 0);
        for (int sample = 0; sample < 1000; ++sample) {
            int32_t expected, actual;
            CHECK(random_r(&native, &expected) == 0 && v_bare_random_r(&bare, &actual) == 0);
            CHECK(expected == actual);
        }
        CHECK(srandom_r(123, &native) == 0 && v_bare_srandom_r(123, &bare) == 0);
        int32_t expected, actual;
        CHECK(random_r(&native, &expected) == 0 && v_bare_random_r(&bare, &actual) == 0);
        CHECK(expected == actual);
    }
    CHECK(v_bare_setlocale(LC_ALL, "C.UTF-8"));
    mbstate_t st = {0};
    wchar_t wc = 0;
    CHECK(v_bare_mbrtowc(&wc, "\xf0\x9f", 2, &st) == (size_t)-2);
    CHECK(v_bare_mbrtowc(&wc, "\x98\x80", 2, &st) == 2 && wc == 0x1f600);
    CHECK(v_bare_mbrtowc(&wc, "\xed\xa0\x80", 3, &st) == (size_t)-1 && errno == EILSEQ);
    char utf[8];
    CHECK(v_bare_wcrtomb(utf, 0x1f600, &st) == 4 && !memcmp(utf, "\xf0\x9f\x98\x80", 4));
    puts("numeric and UTF-8 oracle checks passed");
}
