#if defined(_MSC_VER)
#define V_INT128_STRUCT_ATTR __declspec(align(16))
#else
#define V_INT128_STRUCT_ATTR __attribute__((aligned(16)))
#endif

#if defined(__SIZEOF_INT128__) && !defined(V_INT128_PORTABLE)

typedef unsigned __int128 u128;
typedef __int128 i128;

#define V_INT128_NATIVE 1

/* The native type is two's complement, so every bit pattern is a value and the
 * conversions below are plain casts. */
static inline u128 __v_u128_zero(void) {
	return (u128)0;
}

static inline bool __v_u128_is_zero(u128 a) {
	return a == 0;
}

static inline bool __v_i128_is_zero(i128 a) {
	return a == 0;
}

static inline u128 __v_u128_from_u64(u64 x) {
	return (u128)x;
}

static inline u128 __v_u128_from_i64(i64 x) {
	return (u128)x;
}

static inline i128 __v_i128_from_u64(u64 x) {
	return (i128)x;
}

static inline i128 __v_i128_from_i64(i64 x) {
	return (i128)x;
}

static inline u128 __v_u128_from_i128(i128 x) {
	return (u128)x;
}

static inline i128 __v_i128_from_u128(u128 x) {
	return (i128)x;
}

static inline u64 __v_u128_to_u64(u128 x) {
	return (u64)x;
}

static inline i64 __v_u128_to_i64(u128 x) {
	return (i64)x;
}

static inline u64 __v_i128_to_u64(i128 x) {
	return (u64)x;
}

static inline i64 __v_i128_to_i64(i128 x) {
	return (i64)x;
}

static inline double __v_u128_to_f64(u128 a) {
	return (double)a;
}

static inline double __v_i128_to_f64(i128 a) {
	return (double)a;
}

static inline u128 __v_u128_from_f64(double d) {
	return (u128)d;
}

static inline i128 __v_i128_from_f64(double d) {
	return (i128)d;
}

static inline u128 __v_u128_add(u128 a, u128 b) {
	return a + b;
}

static inline u128 __v_u128_sub(u128 a, u128 b) {
	return a - b;
}

static inline u128 __v_u128_mul(u128 a, u128 b) {
	return a * b;
}

static inline u128 __v_u128_div(u128 a, u128 b) {
	return a / b;
}

static inline u128 __v_u128_rem(u128 a, u128 b) {
	return a % b;
}

static inline u128 __v_u128_neg(u128 a) {
	return (u128)0 - a;
}

static inline u128 __v_u128_and(u128 a, u128 b) {
	return a & b;
}

static inline u128 __v_u128_or(u128 a, u128 b) {
	return a | b;
}

static inline u128 __v_u128_xor(u128 a, u128 b) {
	return a ^ b;
}

static inline u128 __v_u128_not(u128 a) {
	return ~a;
}

static inline u128 __v_u128_shl(u128 a, u64 n) {
	return n >= 128 ? (u128)0 : a << n;
}

static inline u128 __v_u128_shr(u128 a, u64 n) {
	return n >= 128 ? (u128)0 : a >> n;
}

static inline i128 __v_i128_add(i128 a, i128 b) {
	return a + b;
}

static inline i128 __v_i128_sub(i128 a, i128 b) {
	return a - b;
}

static inline i128 __v_i128_mul(i128 a, i128 b) {
	return a * b;
}

/* min_i128 / -1 overflows a signed division in C and raises SIGFPE on x86, so
 * that one case is computed as a negation; every other pair divides directly.
 * The portable path answers the same way through its long division. */
static inline i128 __v_i128_div(i128 a, i128 b) {
	if (b == -1) {
		return (i128)((u128)0 - (u128)a);
	}
	return a / b;
}

static inline i128 __v_i128_rem(i128 a, i128 b) {
	if (b == -1) {
		return (i128)0;
	}
	return a % b;
}

static inline i128 __v_i128_and(i128 a, i128 b) {
	return a & b;
}

static inline i128 __v_i128_or(i128 a, i128 b) {
	return a | b;
}

static inline i128 __v_i128_xor(i128 a, i128 b) {
	return a ^ b;
}

static inline i128 __v_i128_not(i128 a) {
	return ~a;
}

/* Negating the minimum value has no representable result, so the negation is
 * computed in unsigned arithmetic and only then reinterpreted. That wraps to
 * the minimum again, the same answer the portable path gives. */
static inline i128 __v_i128_neg(i128 a) {
	return (i128)((u128)0 - (u128)a);
}

static inline i128 __v_i128_shl(i128 a, u64 n) {
	return n >= 128 ? (i128)0 : a << n;
}

static inline i128 __v_i128_shr(i128 a, u64 n) {
	if (n >= 128) {
		return a < 0 ? (i128)-1 : (i128)0;
	}
	return a >> n;
}

static inline bool __v_u128_eq(u128 a, u128 b) {
	return a == b;
}

static inline bool __v_u128_ne(u128 a, u128 b) {
	return a != b;
}

static inline bool __v_u128_lt(u128 a, u128 b) {
	return a < b;
}

static inline bool __v_u128_gt(u128 a, u128 b) {
	return a > b;
}

static inline bool __v_u128_le(u128 a, u128 b) {
	return a <= b;
}

static inline bool __v_u128_ge(u128 a, u128 b) {
	return a >= b;
}

static inline bool __v_i128_eq(i128 a, i128 b) {
	return a == b;
}

static inline bool __v_i128_ne(i128 a, i128 b) {
	return a != b;
}

static inline bool __v_i128_lt(i128 a, i128 b) {
	return a < b;
}

static inline bool __v_i128_gt(i128 a, i128 b) {
	return a > b;
}

static inline bool __v_i128_le(i128 a, i128 b) {
	return a <= b;
}

static inline bool __v_i128_ge(i128 a, i128 b) {
	return a >= b;
}

#else

typedef struct v_int128_s {
	u64 lo;
	u64 hi;
} V_INT128_STRUCT_ATTR v_int128_t;

/* The portable representation is the same C struct for both signednesses: the
 * bits are two's complement either way, and V keeps signedness in its own type
 * table, so there is nothing for C to distinguish. */
typedef v_int128_t u128;
typedef v_int128_t i128;

static inline u128 __v_u128_make(u64 hi, u64 lo) {
	u128 r;
	r.lo = lo;
	r.hi = hi;
	return r;
}

static inline u128 __v_u128_zero(void) {
	return __v_u128_make(0, 0);
}

static inline u128 __v_u128_from_u64(u64 x) {
	return __v_u128_make(0, x);
}

/* A negative i64 sign-extends, because the struct holds a two's complement
 * pattern and the value keeps its sign when it widens. */
static inline u128 __v_u128_from_i64(i64 x) {
	return __v_u128_make(x < 0 ? ~(u64)0 : 0, (u64)x);
}

static inline i128 __v_i128_from_u64(u64 x) {
	return __v_u128_make(0, x);
}

static inline i128 __v_i128_from_i64(i64 x) {
	return __v_u128_make(x < 0 ? ~(u64)0 : 0, (u64)x);
}

static inline u128 __v_u128_from_i128(i128 x) {
	return x;
}

static inline i128 __v_i128_from_u128(u128 x) {
	return x;
}

static inline u64 __v_u128_to_u64(u128 x) {
	return x.lo;
}

static inline i64 __v_u128_to_i64(u128 x) {
	return (i64)x.lo;
}

static inline u64 __v_i128_to_u64(i128 x) {
	return x.lo;
}

static inline i64 __v_i128_to_i64(i128 x) {
	return (i64)x.lo;
}

/* 2^64 as a double, the weight of the high limb. */
#define V_INT128_TWO64 18446744073709551616.0

static inline u128 __v_u128_add(u128 a, u128 b) {
	u128 r;
	r.lo = a.lo + b.lo;
	r.hi = a.hi + b.hi + (r.lo < a.lo ? 1 : 0);
	return r;
}

static inline u128 __v_u128_sub(u128 a, u128 b) {
	u128 r;
	r.lo = a.lo - b.lo;
	r.hi = a.hi - b.hi - (a.lo < b.lo ? 1 : 0);
	return r;
}

static inline u128 __v_u128_neg(u128 a) {
	return __v_u128_sub(__v_u128_zero(), a);
}

/* The magnitude of a signed value as an unsigned one. */
static inline u128 __v_u128_abs_of(i128 a) {
	if ((a.hi >> 63) != 0) {
		return __v_u128_neg(a);
	}
	return a;
}

static inline bool __v_u128_is_zero(u128 a) {
	return a.lo == 0 && a.hi == 0;
}

static inline bool __v_i128_is_zero(i128 a) {
	return a.lo == 0 && a.hi == 0;
}

/* The low 64 bits come from the 32-bit limb products; the high 64 bits add the
 * cross products, which is everything a 128-bit result keeps. */
static inline u128 __v_u128_mul(u128 a, u128 b) {
	u64 a0 = a.lo & 0xFFFFFFFF;
	u64 a1 = a.lo >> 32;
	u64 b0 = b.lo & 0xFFFFFFFF;
	u64 b1 = b.lo >> 32;
	u64 c0 = a0 * b0;
	u64 c1 = a0 * b1 + (c0 >> 32);
	u64 c2 = a1 * b0 + (c1 & 0xFFFFFFFF);
	u128 r;
	r.lo = (c2 << 32) | (c0 & 0xFFFFFFFF);
	r.hi = a1 * b1 + (c1 >> 32) + (c2 >> 32) + a.lo * b.hi + a.hi * b.lo;
	return r;
}

static inline bool __v_u128_eq(u128 a, u128 b) {
	return a.lo == b.lo && a.hi == b.hi;
}

static inline bool __v_u128_ne(u128 a, u128 b) {
	return a.lo != b.lo || a.hi != b.hi;
}

static inline bool __v_u128_lt(u128 a, u128 b) {
	return a.hi < b.hi || (a.hi == b.hi && a.lo < b.lo);
}

static inline bool __v_u128_gt(u128 a, u128 b) {
	return a.hi > b.hi || (a.hi == b.hi && a.lo > b.lo);
}

static inline bool __v_u128_le(u128 a, u128 b) {
	return a.hi < b.hi || (a.hi == b.hi && a.lo <= b.lo);
}

static inline bool __v_u128_ge(u128 a, u128 b) {
	return a.hi > b.hi || (a.hi == b.hi && a.lo >= b.lo);
}

static inline u128 __v_u128_and(u128 a, u128 b) {
	return __v_u128_make(a.hi & b.hi, a.lo & b.lo);
}

static inline u128 __v_u128_or(u128 a, u128 b) {
	return __v_u128_make(a.hi | b.hi, a.lo | b.lo);
}

static inline u128 __v_u128_xor(u128 a, u128 b) {
	return __v_u128_make(a.hi ^ b.hi, a.lo ^ b.lo);
}

static inline u128 __v_u128_not(u128 a) {
	return __v_u128_make(~a.hi, ~a.lo);
}

static inline u128 __v_u128_shl(u128 a, u64 n) {
	if (n >= 128) {
		return __v_u128_zero();
	}
	if (n == 0) {
		return a;
	}
	if (n >= 64) {
		return __v_u128_make(a.lo << (n - 64), 0);
	}
	return __v_u128_make((a.hi << n) | (a.lo >> (64 - n)), a.lo << n);
}

static inline u128 __v_u128_shr(u128 a, u64 n) {
	if (n >= 128) {
		return __v_u128_zero();
	}
	if (n == 0) {
		return a;
	}
	if (n >= 64) {
		return __v_u128_make(0, a.hi >> (n - 64));
	}
	return __v_u128_make(a.hi >> n, (a.lo >> n) | (a.hi << (64 - n)));
}

static inline i128 __v_i128_shr(i128 a, u64 n) {
	u64 sign = (a.hi >> 63) != 0 ? ~(u64)0 : 0;
	if (n >= 128) {
		return __v_u128_make(sign, sign);
	}
	if (n == 0) {
		return a;
	}
	if (n == 64) {
		return __v_u128_make(sign, a.hi);
	}
	if (n > 64) {
		return __v_u128_make(sign, (a.hi >> (n - 64)) | (sign << (128 - n)));
	}
	return __v_u128_make((a.hi >> n) | (sign << (64 - n)), (a.lo >> n) | (a.hi << (64 - n)));
}

/* Binary long division, one quotient bit per pass. */
static inline void __v_u128_divmod(u128 a, u128 b, u128 *q, u128 *r) {
	u128 qq = __v_u128_zero();
	u128 rr = __v_u128_zero();
	int i;
	for (i = 127; i >= 0; i--) {
		u64 bit;
		if (i >= 64) {
			bit = (a.hi >> (i - 64)) & 1;
		} else {
			bit = (a.lo >> i) & 1;
		}
		rr.hi = (rr.hi << 1) | (rr.lo >> 63);
		rr.lo = (rr.lo << 1) | bit;
		if (__v_u128_ge(rr, b)) {
			rr = __v_u128_sub(rr, b);
			if (i >= 64) {
				qq.hi |= ((u64)1 << (i - 64));
			} else {
				qq.lo |= ((u64)1 << i);
			}
		}
	}
	*q = qq;
	*r = rr;
}

static inline u128 __v_u128_div(u128 a, u128 b) {
	u128 q;
	u128 r;
	__v_u128_divmod(a, b, &q, &r);
	return q;
}

static inline u128 __v_u128_rem(u128 a, u128 b) {
	u128 q;
	u128 r;
	__v_u128_divmod(a, b, &q, &r);
	return r;
}

static inline double __v_u128_to_f64(u128 a) {
	return (double)a.hi * V_INT128_TWO64 + (double)a.lo;
}

static inline double __v_i128_to_f64(i128 a) {
	double v = (double)__v_u128_abs_of(a).hi * V_INT128_TWO64 + (double)__v_u128_abs_of(a).lo;
	return (a.hi >> 63) != 0 ? -v : v;
}

static inline u128 __v_u128_from_f64(double d) {
	u64 hi;
	double rest;
	if (d <= 0) {
		return __v_u128_zero();
	}
	hi = (u64)(d / V_INT128_TWO64);
	rest = d - (double)hi * V_INT128_TWO64;
	return __v_u128_make(hi, (u64)rest);
}

static inline i128 __v_i128_from_f64(double d) {
	u128 bits;
	double mag = d < 0 ? -d : d;
	u64 hi;
	double rest;
	if (mag <= 0) {
		return __v_i128_from_u64(0);
	}
	hi = (u64)(mag / V_INT128_TWO64);
	rest = mag - (double)hi * V_INT128_TWO64;
	bits = __v_u128_make(hi, (u64)rest);
	if (d < 0) {
		bits = __v_u128_neg(bits);
	}
	return bits;
}

static inline i128 __v_i128_add(i128 a, i128 b) {
	return __v_u128_add(a, b);
}

static inline i128 __v_i128_sub(i128 a, i128 b) {
	return __v_u128_sub(a, b);
}

/* Two's complement multiplication keeps its low 128 bits whatever the signs
 * are, so the unsigned product is the signed result. */
static inline i128 __v_i128_mul(i128 a, i128 b) {
	return __v_u128_mul(a, b);
}

static inline i128 __v_i128_and(i128 a, i128 b) {
	return __v_u128_and(a, b);
}

static inline i128 __v_i128_or(i128 a, i128 b) {
	return __v_u128_or(a, b);
}

static inline i128 __v_i128_xor(i128 a, i128 b) {
	return __v_u128_xor(a, b);
}

static inline i128 __v_i128_not(i128 a) {
	return __v_u128_not(a);
}

static inline i128 __v_i128_shl(i128 a, u64 n) {
	return __v_u128_shl(a, n);
}

static inline i128 __v_i128_neg(i128 a) {
	return __v_u128_neg(a);
}

static inline bool __v_i128_lt(i128 a, i128 b) {
	bool an = (a.hi >> 63) != 0;
	bool bn = (b.hi >> 63) != 0;
	if (an != bn) {
		return an;
	}
	return __v_u128_lt(a, b);
}

static inline bool __v_i128_eq(i128 a, i128 b) {
	return __v_u128_eq(a, b);
}

static inline bool __v_i128_ne(i128 a, i128 b) {
	return __v_u128_ne(a, b);
}

static inline bool __v_i128_gt(i128 a, i128 b) {
	return __v_i128_lt(b, a);
}

static inline bool __v_i128_le(i128 a, i128 b) {
	return !__v_i128_lt(b, a);
}

static inline bool __v_i128_ge(i128 a, i128 b) {
	return !__v_i128_lt(a, b);
}

static inline i128 __v_i128_div(i128 a, i128 b) {
	bool neg = ((a.hi ^ b.hi) >> 63) != 0;
	u128 q;
	u128 r;
	__v_u128_divmod(__v_u128_abs_of(a), __v_u128_abs_of(b), &q, &r);
	return neg ? __v_u128_neg(q) : q;
}

/* The remainder takes the sign of the dividend, which is what C and V both
 * specify. */
static inline i128 __v_i128_rem(i128 a, i128 b) {
	bool neg = (a.hi >> 63) != 0;
	u128 q;
	u128 r;
	__v_u128_divmod(__v_u128_abs_of(a), __v_u128_abs_of(b), &q, &r);
	return neg ? __v_u128_neg(r) : r;
}

#endif
