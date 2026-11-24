/* Copyright (c) 2026 the V contributors. MIT license, see the root LICENSE. */
#include <complex.h>
#include <string.h>

/* C specifies the representation of double complex as two doubles, real
 * then imaginary. memcpy preserves signed zero and infinities without the
 * arithmetic in re + I*im, which can manufacture NaNs. */
static double complex v_complex_parts(double re, double im) {
    double complex z;
    const double parts[2] = {re, im};
    memcpy(&z, parts, sizeof z);
    return z;
}

static void v_complex_reference(int op, double re, double im,
                                double pr, double pi, double *rr, double *ri) {
    const double complex z = v_complex_parts(re, im);
    double complex result;
    switch (op) {
    case 0: result = cexp(z); break;
    case 1: result = clog(z); break;
    case 2: result = csin(z); break;
    case 3: result = ccos(z); break;
    case 4: result = ctan(z); break;
    case 5: result = casin(z); break;
    case 6: result = cacos(z); break;
    case 7: result = catan(z); break;
    case 8: result = csqrt(z); break;
    case 9: result = casinh(z); break;
    case 10: result = cacosh(z); break;
    case 11: result = catanh(z); break;
    case 12: result = csinh(z); break;
    case 13: result = ccosh(z); break;
    case 14: result = ctanh(z); break;
    default: result = cpow(z, v_complex_parts(pr, pi)); break;
    }
    *rr = creal(result);
    *ri = cimag(result);
}
