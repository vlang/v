/* Decimal text for the assert and panic printers.
 *
 * This comes after the main helper block, so it can call the arithmetic helpers
 * whatever representation the build picked. The buffer is static and the text is
 * written back-to-front, which keeps a failing path free of allocation. */
static inline const char* __v_u128_str(u128 v) {
	static char buf[64];
	char* p = buf + sizeof(buf) - 1;
	*p = '\0';
	while (!__v_u128_is_zero(v)) {
		u64 digit = __v_u128_to_u64(__v_u128_rem(v, __v_u128_from_u64(10)));
		*(--p) = (char)('0' + (char)digit);
		v = __v_u128_div(v, __v_u128_from_u64(10));
	}
	if (*p == '\0') {
		*(--p) = '0';
	}
	return p;
}

/* Signed decimal. The magnitude comes from the unsigned negation, which is also
 * the only way to print the minimum value. */
static inline const char* __v_i128_str(i128 v) {
	static char buf[64];
	char* p = buf + sizeof(buf) - 1;
	*p = '\0';
	bool neg = __v_i128_lt(v, __v_i128_from_i64(0));
	u128 mag = neg ? __v_u128_neg(__v_u128_from_i128(v)) : __v_u128_from_i128(v);
	while (!__v_u128_is_zero(mag)) {
		u64 digit = __v_u128_to_u64(__v_u128_rem(mag, __v_u128_from_u64(10)));
		*(--p) = (char)('0' + (char)digit);
		mag = __v_u128_div(mag, __v_u128_from_u64(10));
	}
	if (*p == '\0') {
		*(--p) = '0';
	}
	if (neg) {
		*(--p) = '-';
	}
	return p;
}
