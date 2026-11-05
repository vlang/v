module builtin

// freestanding_math.c.v: the SSA native float formatter reaches for the libc
// math names (pow/fabs/floor) at C level; under -freestanding those resolve
// through these pure V exports on both the C backend and the SSA one. No
// dlmalloc dependency here, so any bf.v additions can call into this file.

$if freestanding && native && linux {

	// Links for the SSA/x64 pipeline: the native float formatter reaches for
	// these libc math names; drive them with pure V under -freestanding.

	@[export: 'fabs']
	@[markused]
	fn bf_fabs(x f64) f64 {
		return if x < 0 { -x } else { x }
	}

	@[export: 'floor']
	@[markused]
	fn bf_floor(x f64) f64 {
		mut r := x
		if r < 0 {
			mut t := -r
			if t > 0 && t != f64(i64(t)) {
				t++
			}
			r = -f64(i64(t))
			return r
		}
		return f64(i64(r))
	}

	// bf_pow covers the formatting-needed cases: integer exponents fast path,
	// exp/log fallback for fractional exponents.
	@[export: 'pow']
	@[markused]
	fn bf_pow(x f64, y f64) f64 {
		if x == 0 {
			return 0
		}
		if x < 0 && y != f64(int(y)) { // NaN for fractional exponents of negatives
			return f64(-1) / 0
		}
		iy := int(y)
		if f64(iy) == y {
			mut acc := 1.0
			mut base := x
			mut exp := iy
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
		// exp/log fallback via series approximation (freestanding has no libm)
		return bf_exp(y * bf_ln(x))
	}

	fn bf_ln(x f64) f64 {
		// atanh series on (0,2]
		if x == 0 {
			return f64(-1) / 0
		}
		if x < 0 {
			return f64(0) / 0
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
		mut m := (k - 1) / (k + 1)
		mut m2 := m * m
		mut acc := m
		mut term := m
		for n in 1 .. 24 {
			term *= m2
			acc += term / f64(2 * n + 1)
		}
		// reduce: ln(2)=0.6931471805599453
		return 2 * acc + f64(scale) * 0.6931471805599453
	}

	fn bf_exp(x f64) f64 {
		// e^x = 2^k * e^r with k = round(x/ln2), r |< ln2/2
		if x < -745 {
			return 0
		}
		k := int(x / 0.6931471805599453)
		mut r := x - f64(k) * 0.6931471805599453
		mut term := 1.0
		mut acc := 1.0
		for n in 1 .. 18 {
			term *= r / f64(n)
			acc += term
		}
		// scale by 2^k
		mut power := 1.0
		mut kk := k
		if kk > 0 {
			for kk > 0 {
				power *= 2
				kk--
			}
		} else {
			for kk < 0 {
				power = power / 2
				kk++
			}
		}
		return acc * power
	}

	// posix_memalign resolves through our mmap allocator's aligned path.
	@[export: 'posix_memalign']
	@[markused]
	fn bf_posix_memalign(memptr &voidptr, alignment uint, size uint) int {
		// hardcoded-mmap blocks are 0x1000 aligned; bigger alignments ride the
		// same padding the mexican allocator uses
		mut pad := uint(0)
		if alignment > 4096 {
			pad = alignment
		}
		p := unsafe { __malloc(uint(size + pad)) }
		if p == unsafe { nil } {
			return 12 // ENOMEM normalized to linux numbers by callers
		}
		unsafe {
			addr := uint(p)
			if alignment > 1 && addr & (alignment - 1) != 0 {
				addr = (addr + alignment) & ~(alignment - 1)
			}
			*memptr = voidptr(addr)
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
