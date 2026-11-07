module builtin

// freestanding_math.c.v: the libc math names (pow/fabs/floor) that the native
// SSA float formatter, and on the BSD and Solaris bare runtimes the C backend's
// float formatter, reach for; under -freestanding those resolve through these
// pure V exports. No dlmalloc dependency here, so any bf.v additions can call
// into this file. linux_bare/exports_d_freestanding.v carries the Linux C
// backend's set, and a freestanding macOS program still links libSystem.

$if freestanding && ((native && linux) || (!i386 && (freebsd || netbsd || dragonfly || solaris))) {
	@[export: 'fabs']
	@[markused]
	fn bf_fabs(x f64) f64 {
		return if x < 0 { -x } else { x }
	}

	@[export: 'floor']
	@[markused]
	fn bf_floor(x f64) f64 {
		// Zero of either sign, NaN, the infinities and every |x| >= 2^52 are their
		// own floor; the rest fit i64, whose conversion truncates toward zero.
		if x == 0 || x != x || x <= -4503599627370496.0 || x >= 4503599627370496.0 {
			return x
		}
		t := f64(i64(x))
		return if t > x { t - 1 } else { t }
	}

	// bf_pow takes the integer exponents the float formatter needs by exact
	// repeated squaring, and the rest through exp and ln.
	@[export: 'pow']
	@[markused]
	fn bf_pow(x f64, y f64) f64 {
		if y == 0 || x == 1 {
			return 1
		}
		if x != x || y != y {
			return f64(0) / 0
		}
		if x == 0 {
			return if y < 0 { f64(1) / 0 } else { 0 }
		}
		y_is_int := bf_floor(y) == y
		if x < 0 && !y_is_int {
			return f64(0) / 0
		}
		if y_is_int && y >= -2147483647 && y <= 2147483647 {
			mut acc := 1.0
			mut base := x
			mut exp := int(y)
			if exp < 0 {
				exp = -exp
				base = 1 / base
			}
			for exp > 0 {
				if exp % 2 == 1 {
					acc *= base
				}
				base *= base
				exp = exp / 2
			}
			return acc
		}
		if x < 0 {
			// an integer exponent past the int range: its parity sets the sign
			r := bf_exp(y * bf_ln(-x))
			return if bf_floor(y / 2) * 2 == y { r } else { -r }
		}
		return bf_exp(y * bf_ln(x))
	}

	// bf_ln is the natural logarithm through the atanh series on [0.5, 2).
	fn bf_ln(x f64) f64 {
		if x == 0 {
			return f64(-1) / 0
		}
		if x < 0 || x != x {
			return f64(0) / 0
		}
		if x > 1.7976931348623157e308 {
			return x
		}
		mut k := x
		mut scale := 0
		for k >= 2 {
			k = k / 2
			scale++
		}
		for k < 0.5 {
			k = k * 2
			scale--
		}
		m := (k - 1) / (k + 1)
		m2 := m * m
		mut acc := m
		mut term := m
		for n in 1 .. 24 {
			term *= m2
			acc += term / f64(2 * n + 1)
		}
		return 2 * acc + f64(scale) * 0.6931471805599453
	}

	// bf_exp is e^x as 2^k * e^r, with k = trunc(x / ln 2) and a Taylor series
	// for e^r.
	fn bf_exp(x f64) f64 {
		if x != x {
			return x
		}
		if x < -745 {
			return 0
		}
		if x > 709.782712893384 {
			return f64(1) / 0
		}
		k := int(x / 0.6931471805599453)
		r := x - f64(k) * 0.6931471805599453
		mut term := 1.0
		mut acc := 1.0
		for n in 1 .. 18 {
			term *= r / f64(n)
			acc += term
		}
		mut power := 1.0
		mut kk := k
		for kk > 0 {
			power *= 2
			kk--
		}
		for kk < 0 {
			power = power / 2
			kk++
		}
		return acc * power
	}
}

$if freestanding && native && linux {
	// posix_memalign returns `size` bytes at an address that is a multiple of
	// `alignment`, a power of two and a multiple of the pointer size. Alignments up to
	// the plain allocator's own alignment come from it; larger ones take the aligned path.
	@[export: 'posix_memalign']
	@[markused]
	fn bf_posix_memalign(memptr &voidptr, alignment uint, size uint) int {
		if alignment == 0 || alignment & (alignment - 1) != 0 || alignment % uint(sizeof(voidptr)) != 0 {
			return 22 // EINVAL
		}
		n := if size == 0 { u64(1) } else { u64(size) }
		p := if alignment <= 16 {
			unsafe { __malloc(uint(n)) }
		} else {
			unsafe { bmem_aligned_alloc(n, u64(alignment)) }
		}
		if p == unsafe { nil } {
			return 12 // ENOMEM normalized to linux numbers by callers
		}
		unsafe {
			*memptr = p
		}
		return 0
	}

	// SSA's C.errno lowering resolves __errno_location; keep a freestanding
	// errno cell that bf wrappers store into.
	__global bf_last_errno_cell = int(0)

	@[export: '__errno_location']
	@[markused]
	fn bf_errno_location() &i32 {
		return &i32(voidptr(&bf_last_errno_cell))
	}
}
