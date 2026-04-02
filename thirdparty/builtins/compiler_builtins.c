// Compiler runtime builtins needed for cross-compilation from macOS to Linux.
// When cross-compiling with clang, 128-bit integer operations (e.g. in mbedtls bignum.c)
// generate calls to __udivti3/__umodti3 which are normally provided by libgcc or compiler-rt.
// Since the linuxroot sysroot doesn't include these libraries, we provide minimal
// implementations here.

#ifdef __SIZEOF_INT128__

typedef unsigned __int128 tu_int;
typedef unsigned long long du_int;

typedef union {
    tu_int all;
    struct {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
        du_int low;
        du_int high;
#else
        du_int high;
        du_int low;
#endif
    } s;
} utwords;

static inline unsigned __clzdi2(du_int a) {
    unsigned n = 0;
    if (a == 0) return 64;
    if ((a & 0xFFFFFFFF00000000ULL) == 0) { n += 32; a <<= 32; }
    if ((a & 0xFFFF000000000000ULL) == 0) { n += 16; a <<= 16; }
    if ((a & 0xFF00000000000000ULL) == 0) { n +=  8; a <<=  8; }
    if ((a & 0xF000000000000000ULL) == 0) { n +=  4; a <<=  4; }
    if ((a & 0xC000000000000000ULL) == 0) { n +=  2; a <<=  2; }
    if ((a & 0x8000000000000000ULL) == 0) { n +=  1; }
    return n;
}

tu_int __udivmodti4(tu_int a, tu_int b, tu_int *rem) {
    const unsigned n_udword_bits = sizeof(du_int) * 8;
    const unsigned n_utword_bits = sizeof(tu_int) * 8;
    utwords n, d, q, r;
    unsigned sr;
    n.all = a;
    d.all = b;

    // Special cases
    if (n.s.high == 0) {
        if (d.s.high == 0) {
            if (rem) *rem = n.s.low % d.s.low;
            return n.s.low / d.s.low;
        }
        if (rem) *rem = n.s.low;
        return 0;
    }
    // n.s.high != 0
    if (d.s.low == 0) {
        if (d.s.high == 0) {
            // Division by zero (undefined behavior, just return 0)
            if (rem) *rem = 0;
            return 0;
        }
        if (n.s.low == 0) {
            if (rem) {
                r.s.high = n.s.high % d.s.high;
                r.s.low = 0;
                *rem = r.all;
            }
            return n.s.high / d.s.high;
        }
        // n.s.high != 0 && n.s.low != 0 && d.s.low == 0 && d.s.high != 0
        if ((d.s.high & (d.s.high - 1)) == 0) {
            // d is a power of 2
            if (rem) {
                r.s.low = n.s.low;
                r.s.high = n.s.high & (d.s.high - 1);
                *rem = r.all;
            }
            return n.s.high >> __clzdi2(d.s.high);
        }
        sr = __clzdi2(d.s.high) - __clzdi2(n.s.high);
        if (sr > n_udword_bits - 2) {
            if (rem) *rem = n.all;
            return 0;
        }
        ++sr;
        // 1 <= sr <= n_udword_bits - 1
        q.s.low = 0;
        q.s.high = n.s.low << (n_udword_bits - sr);
        r.s.high = n.s.high >> sr;
        r.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
        goto shift_loop;
    }
    // d.s.low != 0
    if (d.s.high == 0) {
        if ((d.s.low & (d.s.low - 1)) == 0) {
            if (rem) *rem = n.s.low & (d.s.low - 1);
            if (d.s.low == 1) return n.all;
            sr = __builtin_ctzll(d.s.low);
            q.s.high = n.s.high >> sr;
            q.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
            return q.all;
        }
        sr = 1 + n_udword_bits + __clzdi2(d.s.low) - __clzdi2(n.s.high);
        if (sr == n_udword_bits) {
            q.s.low = 0;
            q.s.high = 0;
            r.s.high = 0;
            r.s.low = n.s.high;
        } else if (sr < n_udword_bits) {
            q.s.low = 0;
            q.s.high = n.s.low << (n_udword_bits - sr);
            r.s.high = n.s.high >> sr;
            r.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
        } else {
            q.s.low = n.s.low << (n_utword_bits - sr);
            q.s.high = (n.s.high << (n_utword_bits - sr)) | (n.s.low >> (sr - n_udword_bits));
            r.s.high = 0;
            r.s.low = n.s.high >> (sr - n_udword_bits);
        }
        goto shift_loop;
    }
    // d.s.high != 0 && d.s.low != 0
    sr = __clzdi2(d.s.high) - __clzdi2(n.s.high);
    if (sr > n_udword_bits - 1) {
        if (rem) *rem = n.all;
        return 0;
    }
    ++sr;
    q.s.low = 0;
    if (sr == n_udword_bits) {
        q.s.high = 0;
        r.s.high = 0;
        r.s.low = n.s.high;
    } else {
        r.s.high = n.s.high >> sr;
        r.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
        q.s.high = n.s.low << (n_udword_bits - sr);
    }

shift_loop:
    // The main shift-and-subtract loop
    for (; sr > 0; --sr) {
        r.s.high = (r.s.high << 1) | (r.s.low >> (n_udword_bits - 1));
        r.s.low = (r.s.low << 1) | (q.s.high >> (n_udword_bits - 1));
        q.s.high = (q.s.high << 1) | (q.s.low >> (n_udword_bits - 1));
        q.s.low = (q.s.low << 1);
        // Compute (r - d) and conditionally subtract
        const long long s = (long long)(d.all - r.all - 1) >> (n_utword_bits - 1);
        q.s.low |= s & 1;
        r.all -= d.all & s;
    }
    if (rem) *rem = r.all;
    return q.all;
}

tu_int __udivti3(tu_int a, tu_int b) {
    return __udivmodti4(a, b, 0);
}

tu_int __umodti3(tu_int a, tu_int b) {
    tu_int r;
    __udivmodti4(a, b, &r);
    return r;
}

#endif // __SIZEOF_INT128__
