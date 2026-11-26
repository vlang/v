/* Hardware floating-point scaling follows musl's normal-factor method; COPYRIGHT.musl. */
#include <errno.h>
#include <fenv.h>
#include <float.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

double scalbn(double x, int n) {
    if (!isfinite(x) || x == 0)
        return x + x;
    if (n > 1023) {
        x *= 0x1p1023;
        n -= 1023;
        if (n > 1023) {
            x *= 0x1p1023;
            n -= 1023;
            if (n > 1023)
                n = 1023;
        }
    } else if (n < -1022) {
        x *= 0x1p-1022 * 0x1p53;
        n += 1022 - 53;
        if (n < -1022) {
            x *= 0x1p-1022 * 0x1p53;
            n += 1022 - 53;
            if (n < -1022)
                n = -1022;
        }
    }
    union {
        uint64_t bits;
        double value;
    } factor = {.bits = (uint64_t)(1023 + n) << 52};
    double result = x * factor.value;
    if (!isfinite(result) || result == 0)
        errno = ERANGE;
    return result;
}
#if LDBL_MANT_DIG == 113
/* AArch64 compiler runtimes can multiply binary128 using integer instructions.
 * Adjust the exponent directly and round discarded bits using the caller's FP mode. */
static __uint128_t scale_binary128(__uint128_t bits, int n) {
    const __uint128_t unit = (__uint128_t)1 << 112;
    const __uint128_t infinity = (__uint128_t)0x7fff << 112;
    __uint128_t sign = bits & ((__uint128_t)1 << 127);
    __uint128_t fraction = bits & (unit - 1);
    int exponent = (int)((bits >> 112) & 0x7fff);
    if (exponent == 0x7fff) {
        if (fraction && !(fraction & (unit >> 1))) {
            feraiseexcept(FE_INVALID);
            return bits | (unit >> 1);
        }
        return bits;
    }
    __uint128_t significand = unit | fraction;
    if (!exponent) {
        if (!fraction) return bits;
        uint64_t high = (uint64_t)(fraction >> 64);
        int shift = high ? __builtin_clzll(high) - 15
                         : __builtin_clzll((uint64_t)fraction) + 49;
        significand = fraction << shift;
        exponent = 1 - shift;
    }
    int64_t scaled = (int64_t)exponent + n;
    if (scaled > 0 && scaled < 0x7fff)
        return sign | ((__uint128_t)scaled << 112) | (significand & (unit - 1));

    int mode = fegetround();
    int away = sign ? mode == FE_DOWNWARD : mode == FE_UPWARD;
    if (scaled >= 0x7fff) {
        feraiseexcept(FE_OVERFLOW | FE_INEXACT);
        if (away || mode == FE_TONEAREST) {
            errno = ERANGE;
            return sign | infinity;
        }
        return sign | (infinity - 1);
    }
    /* At 114 bits the entire significand is strictly below the midpoint. */
    int drop = scaled < -112 ? 114 : (int)(1 - scaled);
    __uint128_t kept = significand >> drop;
    __uint128_t lost = significand & (((__uint128_t)1 << drop) - 1);
    if (lost) {
        __uint128_t half = (__uint128_t)1 << (drop - 1);
        if (away || (mode == FE_TONEAREST && (lost > half || (lost == half && (kept & 1)))))
            ++kept;
        feraiseexcept(FE_UNDERFLOW | FE_INEXACT);
        if (!kept) errno = ERANGE;
    }
    return sign | kept;
}
#endif

long double scalbnl(long double x, int n) {
#if LDBL_MANT_DIG == 53
    return scalbn(x, n);
#elif LDBL_MANT_DIG == 113
    __uint128_t bits;
    memcpy(&bits, &x, sizeof bits);
    bits = scale_binary128(bits, n);
    memcpy(&x, &bits, sizeof x);
    return x;
#else
    if (!isfinite(x) || x == 0)
        return x + x;
    if (n > 16383) {
        x *= 0x1p16383L;
        n -= 16383;
        if (n > 16383) {
            x *= 0x1p16383L;
            n -= 16383;
            if (n > 16383)
                n = 16383;
        }
    } else if (n < -16382) {
        x *= 0x1p-16382L * 0x1p113L;
        n += 16382 - 113;
        if (n < -16382) {
            x *= 0x1p-16382L * 0x1p113L;
            n += 16382 - 113;
            if (n < -16382)
                n = -16382;
        }
    }
    long double factor = 1;
    unsigned char bytes[sizeof factor];
    memcpy(bytes, &factor, sizeof factor);
#if LDBL_MANT_DIG == 64
    uint16_t exponent = (uint16_t)(16383 + n);
    memcpy(bytes + 8, &exponent, 2);
#else
#error unsupported long double representation
#endif
    memcpy(&factor, bytes, sizeof factor);
    long double result = x * factor;
    if (!isfinite(result) || result == 0)
        errno = ERANGE;
    return result;
#endif
}
/* Exact binary remainder: scaling is by powers of two, so subtraction never
 * rounds away a quotient bit. At most one iteration per exponent bit is used. */
long double fmodl(long double x, long double y) {
    if (isnan(x) || isnan(y))
        return x + y;
    if (y == 0 || isinf(x)) {
        errno = EDOM;
        volatile long double zero = 0;
        return zero / zero;
    }
    int negative = signbit(x);
    x = fabsl(x);
    y = fabsl(y);
    if (x < y)
        return negative ? -x : x;
    if (x == y)
        return copysignl(0, negative ? -1 : 1);
    int shift = 0;
    long double scaled = y;
    /* Doubling is exact up to this bound, even when x is subnormal. */
    while (scaled <= LDBL_MAX * 0.5L && scaled * 2 <= x) {
        scaled *= 2;
        ++shift;
    }
    for (;;) {
        if (x >= scaled)
            x -= scaled;
        if (!shift--)
            break;
        scaled *= 0.5L;
    }
    return copysignl(x, negative ? -1 : 1);
}

/* musl's public math macros call these entry points for native classification.
 * Inspect representations rather than comparing NaNs or disturbing FP flags. */
int __fpclassify(double x) {
    uint64_t bits;
    memcpy(&bits, &x, sizeof bits);
    unsigned int exponent = (unsigned int)(bits >> 52) & 0x7ff;
    uint64_t fraction = bits & UINT64_C(0xfffffffffffff);
    return exponent == 0x7ff ? (fraction ? FP_NAN : FP_INFINITE)
           : !exponent       ? (fraction ? FP_SUBNORMAL : FP_ZERO)
                             : FP_NORMAL;
}
int __fpclassifyf(float x) {
    uint32_t bits;
    memcpy(&bits, &x, sizeof bits);
    unsigned int exponent = (bits >> 23) & 255, fraction = bits & 0x7fffff;
    return exponent == 255 ? (fraction ? FP_NAN : FP_INFINITE)
           : !exponent     ? (fraction ? FP_SUBNORMAL : FP_ZERO)
                           : FP_NORMAL;
}
int __fpclassifyl(long double x) {
#if LDBL_MANT_DIG == 53
    return __fpclassify(x);
#elif LDBL_MANT_DIG == 64
    unsigned char bytes[sizeof x];
    uint64_t mantissa;
    uint16_t sign_exponent;
    memcpy(bytes, &x, sizeof x);
    memcpy(&mantissa, bytes, 8);
    memcpy(&sign_exponent, bytes + 8, 2);
    unsigned int exponent = sign_exponent & 0x7fff;
    if (exponent == 0x7fff)
        return mantissa == UINT64_C(0x8000000000000000) ? FP_INFINITE : FP_NAN;
    if (!exponent)
        return mantissa ? FP_SUBNORMAL : FP_ZERO;
    return mantissa >> 63 ? FP_NORMAL : FP_NAN;
#elif LDBL_MANT_DIG == 113
    uint64_t words[2];
    memcpy(words, &x, sizeof words);
    unsigned int exponent = (unsigned int)(words[1] >> 48) & 0x7fff;
    int fraction = words[0] || (words[1] & UINT64_C(0xffffffffffff));
    return exponent == 0x7fff ? (fraction ? FP_NAN : FP_INFINITE)
           : !exponent        ? (fraction ? FP_SUBNORMAL : FP_ZERO)
                              : FP_NORMAL;
#endif
}
int __signbit(double x) {
    uint64_t bits;
    memcpy(&bits, &x, sizeof bits);
    return (int)(bits >> 63);
}
int __signbitf(float x) {
    uint32_t bits;
    memcpy(&bits, &x, sizeof bits);
    return (int)(bits >> 31);
}
int __signbitl(long double x) {
    unsigned char bytes[sizeof x];
    memcpy(bytes, &x, sizeof x);
#if LDBL_MANT_DIG == 64
    return bytes[9] >> 7;
#else
    return bytes[sizeof x - 1] >> 7;
#endif
}

double modf(double value, double *integer) {
    union { double value; uint64_t bits; } u = {value};
    int e = (int)((u.bits >> 52) & 0x7ff) - 1023;
    union { uint64_t bits; double value; } zero = {u.bits & UINT64_C(0x8000000000000000)};
    if (e < 0) { *integer = zero.value; return value; }
    if (e >= 52) {
        if (e == 1024 && (u.bits & UINT64_C(0xfffffffffffff))) {
            /* Native arithmetic quiets an sNaN and raises FE_INVALID once. */
            *integer = value + value;
            return *integer;
        }
        *integer = value;
        return zero.value;
    }
    uint64_t mask = UINT64_C(0xfffffffffffff) >> e;
    if (!(u.bits & mask)) { *integer = value; return zero.value; }
    u.bits &= ~mask;
    *integer = u.value;
    return value - u.value;
}
