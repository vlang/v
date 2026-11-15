/* Compile the emitted helper block without host or Windows SDK headers. */
struct int128_layout {
	char first;
	u128 value;
	char last;
};
_Static_assert(sizeof(u128) == 16 && sizeof(i128) == 16, "128-bit size");
_Static_assert(__alignof__(u128) == 16 && __alignof__(i128) == 16, "128-bit alignment");
_Static_assert(__builtin_offsetof(struct int128_layout, value) == 16, "aligned field");
_Static_assert(__builtin_offsetof(struct int128_layout, last) == 32, "following field");
_Static_assert(sizeof(struct int128_layout) == 48, "containing struct size");

/* Each row is dividend, divisor, quotient, remainder, as high/low pairs.
 * Volatile inputs keep the operations live even in optimized builds. */
static const volatile u64 unsigned_cases[][8] = {
	{0x1000000000ULL, 12345, 1, 0, 0, 0x1000000000ULL, 0, 12345},
	{0xffffffffffffffffULL, 0xffffffffffffffffULL, 0, 10,
		0x1999999999999999ULL, 0x9999999999999999ULL, 0, 5},
	{0x8000000000000000ULL, 5, 0x8000000000000000ULL, 2, 0, 1, 0, 3},
	{0x7fffffffffffffffULL, 0xffffffffffffffffULL, 0x8000000000000000ULL, 0,
		0, 0, 0x7fffffffffffffffULL, 0xffffffffffffffffULL},
	{0, 0, 0, 7, 0, 0, 0, 0}
};
static const volatile u64 signed_cases[][8] = {
	{0xffffffefffffffffULL, 0xffffffffffffcfc7ULL, 1, 0,
		0xffffffffffffffffULL, 0xfffffff000000000ULL,
		0xffffffffffffffffULL, 0xffffffffffffcfc7ULL},
	{0x1000000000ULL, 12345, 0xffffffffffffffffULL, 0,
		0xffffffffffffffffULL, 0xfffffff000000000ULL, 0, 12345},
	{0xffffffefffffffffULL, 0xffffffffffffcfc7ULL, 0xffffffffffffffffULL, 0,
		0, 0x1000000000ULL, 0xffffffffffffffffULL, 0xffffffffffffcfc7ULL},
	{0x8000000000000000ULL, 0, 0xffffffffffffffffULL, 0xffffffffffffffffULL,
		0x8000000000000000ULL, 0, 0, 0},
	{0x8000000000000000ULL, 0, 0, 7,
		0xedb6db6db6db6db6ULL, 0xdb6db6db6db6db6eULL,
		0xffffffffffffffffULL, 0xfffffffffffffffeULL}
};

static int same_text(const char *a, const char *b) {
	while (*a && *a == *b) {
		a++;
		b++;
	}
	return *a == *b;
}

static int check_int128(void) {
	unsigned int n;
	for (n = 0; n < sizeof(unsigned_cases) / sizeof(unsigned_cases[0]); n++) {
		const volatile u64 *row = unsigned_cases[n];
		u128 a = __v_u128_make(row[0], row[1]);
		u128 b = __v_u128_make(row[2], row[3]);
		u128 q = __v_u128_div(a, b);
		u128 r = __v_u128_rem(a, b);
		if (!__v_u128_eq(q, __v_u128_make(row[4], row[5]))) return 1;
		if (!__v_u128_eq(r, __v_u128_make(row[6], row[7]))) return 2;
		if (!__v_u128_eq(__v_u128_add(__v_u128_mul(q, b), r), a)) return 3;
	}
	for (n = 0; n < sizeof(signed_cases) / sizeof(signed_cases[0]); n++) {
		const volatile u64 *row = signed_cases[n];
		i128 a = __v_i128_from_u128(__v_u128_make(row[0], row[1]));
		i128 b = __v_i128_from_u128(__v_u128_make(row[2], row[3]));
		i128 q = __v_i128_div(a, b);
		i128 r = __v_i128_rem(a, b);
		if (!__v_i128_eq(q, __v_i128_from_u128(__v_u128_make(row[4], row[5])))) return 4;
		if (!__v_i128_eq(r, __v_i128_from_u128(__v_u128_make(row[6], row[7])))) return 5;
	}
	if (!same_text(__v_u128_str(__v_u128_make(~(u64)0, ~(u64)0)),
		"340282366920938463463374607431768211455")) return 6;
	if (!same_text(__v_i128_str(__v_i128_from_u128(__v_u128_make((u64)1 << 63, 0))),
		"-170141183460469231731687303715884105728")) return 7;
	return 0;
}

int main(void) {
	return check_int128();
}

/* A PE entry point lets the link check run without a Windows CRT or SDK. */
int entry(void) {
	return check_int128();
}
