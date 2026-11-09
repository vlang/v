module builtin

// exports_d_freestanding.v: the libc-named symbols the freestanding runtime
// provides to the link. A libc-linked `-syscalls` build must not carry them: the
// program would override libc's errno accessor and math functions.
$if freestanding && !i386 {
	@[export: '__errno_location']
	@[markused]
	fn bf_errno_export() &i32 {
		return &i32(voidptr(&bf_last_errno_cell))
	}

	@[export: 'fabs']
	@[markused]
	fn bf_fabs(x f64) f64 {
		return if x < 0 { -x } else { x }
	}

	@[export: 'floor']
	@[markused]
	fn bf_floor(x f64) f64 {
		if x < 0 {
			t := -x
			if t != f64(i64(t)) {
				return -f64(i64(t) + 1)
			}
			return -f64(i64(t))
		}
		return f64(i64(x))
	}

	@[export: 'pow']
	@[markused]
	fn bf_pow(x f64, y f64) f64 {
		if x == 0 {
			return 0
		}
		if x < 0 && y != f64(int(y)) {
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
		return bf_exp(y * bf_ln(x))
	}
}
