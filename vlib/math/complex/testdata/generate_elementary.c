/* Copyright (c) 2026 the V contributors. MIT license, see the root LICENSE.
 * Regenerate: cc generate_elementary.c -lmpc -lmpfr -lgmp -o /tmp/complex_oracle
 *            /tmp/complex_oracle > elementary.tsv
 * MPC is needed only to regenerate the checked-in fixture, never by V tests.
 * Every expected binary64 component must agree at 512 and 1024 bit precision.
 * Run from this directory; elementary_regressions.tsv supplies host-libm
 * regression inputs whose expected results are calculated only by MPC.
 */
#include <mpc.h>
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
static double number(uint64_t x) {
    double result;
    memcpy(&result, &x, sizeof result);
    return result;
}
static void operation(mpc_t result, mpc_t z, mpc_t p, int op) {
    switch (op) {
    case 0: mpc_exp(result, z, MPC_RNDNN); break;
    case 1: mpc_log(result, z, MPC_RNDNN); break;
    case 2: mpc_sin(result, z, MPC_RNDNN); break;
    case 3: mpc_cos(result, z, MPC_RNDNN); break;
    case 4: mpc_tan(result, z, MPC_RNDNN); break;
    case 5: mpc_asin(result, z, MPC_RNDNN); break;
    case 6: mpc_acos(result, z, MPC_RNDNN); break;
    case 7: mpc_atan(result, z, MPC_RNDNN); break;
    case 8: mpc_sqrt(result, z, MPC_RNDNN); break;
    case 9: mpc_asinh(result, z, MPC_RNDNN); break;
    case 10: mpc_acosh(result, z, MPC_RNDNN); break;
    case 11: mpc_atanh(result, z, MPC_RNDNN); break;
    case 12: mpc_sinh(result, z, MPC_RNDNN); break;
    case 13: mpc_cosh(result, z, MPC_RNDNN); break;
    case 14: mpc_tanh(result, z, MPC_RNDNN); break;
    default: mpc_pow(result, z, p, MPC_RNDNN); break;
    }
}
static void row(double re, double im, double pr, double pi, int op) {
    mpc_t z, p, result;
    uint64_t expected[2] = {0, 0};
    for (int precision = 512; precision <= 1024; precision *= 2) {
        mpc_init2(z, precision);
        mpc_init2(p, precision);
        mpc_init2(result, precision);
        mpc_set_d_d(z, re, im, MPC_RNDNN);
        mpc_set_d_d(p, pr, pi, MPC_RNDNN);
        operation(result, z, p, op);
        const uint64_t r = bits(mpfr_get_d(mpc_realref(result), MPFR_RNDN));
        const uint64_t i = bits(mpfr_get_d(mpc_imagref(result), MPFR_RNDN));
        if (precision == 512) {
            expected[0] = r;
            expected[1] = i;
        } else if (expected[0] != r || expected[1] != i) {
            fprintf(stderr, "unstable oracle: op %d (%a,%a)\n", op, re, im);
            exit(1);
        }
        mpc_clear(z);
        mpc_clear(p);
        mpc_clear(result);
    }
    printf("%d\t%016llx\t%016llx\t%016llx\t%016llx\t%016llx\t%016llx\n", op,
           (unsigned long long)bits(re), (unsigned long long)bits(im),
           (unsigned long long)bits(pr), (unsigned long long)bits(pi),
           (unsigned long long)expected[0], (unsigned long long)expected[1]);
}
static uint64_t state = UINT64_C(0x76696c655f6d6174);
static double sample(void) {
    state = state * UINT64_C(6364136223846793005) + UINT64_C(1442695040888963407);
    return (state >> 11) * (1.0 / 9007199254740992.0);
}
int main(void) {
    puts("# MPC 512/1024-bit round-to-nearest, binary64 inputs and outputs as hex bits");
    puts("# op re im power_re power_im result_re result_im (tab separated)");
    const double pairs[][2] = {
        {0, 0}, {-0.0, -0.0}, {1, 0}, {1, -0.0}, {-1, 0}, {-1, -0.0},
        {1, 1}, {-1, -1}, {0.9999999999999999, 0x1p-100},
        {1.0000000000000002, -0x1p-100}, {0x1p-1074, 0x1p-1074},
        {0x1p-1022, 0x1p-1074}, {0x1p-100, -0x1p-1074},
        {0x1p-1074, -0x1p-100}, {0x1p1023, 0x1p1023},
        {0x1.fffffffffffffp1023, 0x1.fffffffffffffp1023},
        {-0x1.fffffffffffffp1023, 0x1p-1074},
        {710, 0x1p-1074}, {1454, 0x1p-1074}, {1, 1000}, {1000, 1},
        {4503599627370496.0, 1}, {1, 4503599627370496.0},
        {0.6, 0.8}, {0.9999999999999999, 0x1p-26},
        {1.2254234266273745, 15.261375963319086}
    };
    for (size_t n = 0; n < sizeof pairs / sizeof pairs[0]; n++) {
        for (int op = 0; op < 15; op++)
            row(pairs[n][0], pairs[n][1], 0, 0, op);
        if (pairs[n][0] != 0 || pairs[n][1] != 0) {
            row(pairs[n][0], pairs[n][1], 0.5, 0, 15);
            row(pairs[n][0], pairs[n][1], 0.25, -0.125, 15);
        }
    }
    for (int n = 0; n < 64; n++) {
        const double re = (sample() - 0.5) * 40;
        const double im = (sample() - 0.5) * 40;
        const double pr = (sample() - 0.5) * 4;
        const double pi = (sample() - 0.5) * 4;
        for (int op = 0; op < 16; op++) row(re, im, pr, pi, op);
    }
    /* Independent exponent scales and near-unit cancellation in log. */
    for (int n = 0; n < 32; n++) {
        const uint64_t eb = (uint64_t)(sample() * 2046) + 1;
        const uint64_t ec = (uint64_t)(sample() * 2046) + 1;
        const double re = number((eb << 52) | (bits(sample()) & UINT64_C(0xfffffffffffff)));
        const double im = number((ec << 52) | (bits(sample()) & UINT64_C(0xfffffffffffff)));
        for (int op = 1; op <= 11; op++) {
            if (op >= 2 && op <= 4) continue;
            row(re, im, 0, 0, op);
        }
    }
    FILE *regressions = fopen("elementary_regressions.tsv", "r");
    if (!regressions) {
        perror("elementary_regressions.tsv");
        return 1;
    }
    char line[256];
    while (fgets(line, sizeof line, regressions)) {
        if (line[0] == '#') continue;
        int op;
        unsigned long long re, im;
        if (sscanf(line, "%d %llx %llx", &op, &re, &im) != 3 || op < 0 || op >= 15) {
            fprintf(stderr, "invalid regression input: %s", line);
            fclose(regressions);
            return 1;
        }
        row(number((uint64_t)re), number((uint64_t)im), 0, 0, op);
    }
    if (ferror(regressions)) {
        perror("reading elementary_regressions.tsv");
        fclose(regressions);
        return 1;
    }
    fclose(regressions);
    mpfr_free_cache();
    return 0;
}
