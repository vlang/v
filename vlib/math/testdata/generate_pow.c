/* Copyright (c) 2026 the V contributors. MIT license, see the root LICENSE.
 * Regenerate: cc generate_pow.c -lmpfr -lgmp -o /tmp/pow_oracle
 *            /tmp/pow_oracle > pow_bits.tsv
 * MPFR is only needed to regenerate the fixture. Every rounded binary64
 * result is checked at both 512 and 1024 bits of precision.
 */
#include <mpfr.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static uint64_t bits(double x) {
    uint64_t result;
    memcpy(&result, &x, sizeof result);
    return result;
}
static double number(uint64_t value) {
    double result;
    memcpy(&result, &value, sizeof result);
    return result;
}
static void row(double x, double y) {
    mpfr_t a, b, result;
    uint64_t expected = 0;
    for (int precision = 512; precision <= 1024; precision *= 2) {
        mpfr_inits2(precision, a, b, result, (mpfr_ptr)0);
        mpfr_set_d(a, x, MPFR_RNDN);
        mpfr_set_d(b, y, MPFR_RNDN);
        mpfr_pow(result, a, b, MPFR_RNDN);
        const uint64_t actual = bits(mpfr_get_d(result, MPFR_RNDN));
        if (precision == 512) expected = actual;
        else if (actual != expected) {
            fprintf(stderr, "unstable oracle: (%a,%a)\n", x, y);
            exit(1);
        }
        mpfr_clears(a, b, result, (mpfr_ptr)0);
    }
    printf("%016llx\t%016llx\t%016llx\n", (unsigned long long)bits(x),
           (unsigned long long)bits(y), (unsigned long long)expected);
}
static uint64_t state = UINT64_C(0x706f775f706f7274);
static double sample(void) {
    state = state * UINT64_C(6364136223846793005) + UINT64_C(1442695040888963407);
    return (state >> 11) * (1.0 / 9007199254740992.0);
}
int main(void) {
    puts("# MPFR512/1024-bit oracle: x, y, pow(x,y), binary64 hex bits");
    const double bases[] = {0, -0.0, INFINITY, -INFINITY, NAN, 1, -1, 2, -2};
    const double exponents[] = {0, -0.0, 1, -1, 2, -2, 3, -3, 0.5, -0.5,
        INFINITY, -INFINITY, NAN, 9007199254740991.0, -9007199254740991.0,
        9007199254740992.0, -9007199254740992.0,
        9223372036854775808.0, -9223372036854775808.0, 1e100, -1e100};
    for (size_t i = 0; i < sizeof bases / sizeof bases[0]; i++)
        for (size_t j = 0; j < sizeof exponents / sizeof exponents[0]; j++)
            row(bases[i], exponents[j]);
    const double pairs[][2] = {
        {1.0000000000000002, 1e15}, {1.0000000000000002, 1e15 + 0.5},
        {1.0000000000000002, 0x1p52 - 0.5},
        {0.9999999999999999, 1e15 + 0.5},
        {0.9999999999999999, 0x1p52 - 0.5},
        {0.9999999999999999, 1e18}, {0.9999999999999999, -1e18},
        {1.0001, 1e6}, {1.0001, 1e6 + 0.25}, {1.0001, -1e6 - 0.25},
        {1.0001, 9007199254740991.0}, {1.0001, -9007199254740991.0},
        {1e103, -3}, {1e-103, 3}, {1e-103, -3},
        {1e-200, 1.25}, {1e-200, 1.75}, {1e-100, 3.2},
        {2, -1074}, {2, -1075}, {2, -1074.5}, {2, -1073.5},
        {2, 1023.9999999999999}, {0x1p-1074, 0.75},
        {0x1.fffffffffffffp1023, 0.75}
    };
    for (size_t i = 0; i < sizeof pairs / sizeof pairs[0]; i++)
        row(pairs[i][0], pairs[i][1]);
    for (int i = 0; i < 256; i++) row(0.125 + sample() * 7.875, (sample() - 0.5) * 600);
    for (int i = 0; i < 128; i++) row(1 + (sample() - 0.5) * 2e-6, (sample() - 0.5) * 2e8);
    /* Fractional powers across the full normal input exponent range. */
    for (int i = 0; i < 256; i++) {
        const uint64_t exponent = (uint64_t)(sample() * 2046) + 1;
        const uint64_t fraction = (uint64_t)(sample() * 4503599627370496.0);
        row(number((exponent << 52) | fraction), (sample() - 0.5) * 4);
    }
    mpfr_free_cache();
    return 0;
}
