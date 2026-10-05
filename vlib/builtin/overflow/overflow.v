module overflow

// Functions for integer arithmetic overflow.
fn C.__builtin_add_overflow(x any, y any, z voidptr) bool
fn C.__builtin_sub_overflow(x any, y any, z voidptr) bool
fn C.__builtin_mul_overflow(x any, y any, z voidptr) bool

// add_i8 computes `x` + `y` for i8 values, and panic if overflow occurs
@[inline]
fn add_i8(x i8, y i8) i8 {
	mut res := i8(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res64 := i64(x) + i64(y)
		res = i8(res64)
		is_overflow = res64 > max_i8 || res64 < min_i8
	}
	if is_overflow {
		panic('attempt to add with overflow(i8(${x}) + i8(${y}))')
	}
	return res
}

// add_u8 computes `x` + `y` for u8 values, and panic if overflow occurs
@[inline]
fn add_u8(x u8, y u8) u8 {
	mut res := u8(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res = x + y
		is_overflow = res < x
	}
	if is_overflow {
		panic('attempt to add with overflow(u8(${x}) + u8(${y}))')
	}
	return res
}

// sub_i8 computes `x` - `y` for i8 values, and panic if overflow occurs
@[inline]
fn sub_i8(x i8, y i8) i8 {
	mut res := i8(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res64 := i64(x) - i64(y)
		res = i8(res64)
		is_overflow = res64 > max_i8 || res64 < min_i8
	}
	if is_overflow {
		panic('attempt to sub with overflow(i8(${x}) - i8(${y}))')
	}
	return res
}

// sub_u8 computes `x` - `y` for u8 values, and panic if overflow occurs
@[inline]
fn sub_u8(x u8, y u8) u8 {
	mut res := u8(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res = x - y
		is_overflow = x < y
	}
	if is_overflow {
		panic('attempt to sub with overflow(u8(${x}) - u8(${y}))')
	}
	return res
}

// mul_i8 computes `x` * `y` for i8 values, and panic if overflow occurs
@[inline]
fn mul_i8(x i8, y i8) i8 {
	mut res := i8(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res64 := i64(x) * i64(y)
		res = i8(res64)
		is_overflow = res64 > max_i8 || res64 < min_i8
	}
	if is_overflow {
		panic('attempt to mul with overflow(i8(${x}) * i8(${y}))')
	}
	return res
}

// mul_u8 computes `x` * `y` for u8 values, and panic if overflow occurs
@[inline]
fn mul_u8(x u8, y u8) u8 {
	mut res := u8(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res64 := u64(x) * u64(y)
		res = u8(res64)
		is_overflow = res64 > max_u8
	}
	if is_overflow {
		panic('attempt to mul with overflow(u8(${x}) * u8(${y}))')
	}
	return res
}

// add_i16 computes `x` + `y` for i16 values, and panic if overflow occurs
@[inline]
fn add_i16(x i16, y i16) i16 {
	mut res := i16(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res64 := i64(x) + i64(y)
		res = i16(res64)
		is_overflow = res64 > max_i16 || res64 < min_i16
	}
	if is_overflow {
		panic('attempt to add with overflow(i16(${x}) + i16(${y}))')
	}
	return res
}

// add_u16 computes `x` + `y` for u16 values, and panic if overflow occurs
@[inline]
fn add_u16(x u16, y u16) u16 {
	mut res := u16(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res = x + y
		is_overflow = res < x
	}
	if is_overflow {
		panic('attempt to add with overflow(u16(${x}) + u16(${y}))')
	}
	return res
}

// sub_i16 computes `x` - `y` for i16 values, and panic if overflow occurs
@[inline]
fn sub_i16(x i16, y i16) i16 {
	mut res := i16(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res64 := i64(x) - i64(y)
		res = i16(res64)
		is_overflow = res64 > max_i16 || res64 < min_i16
	}
	if is_overflow {
		panic('attempt to sub with overflow(i16(${x}) - i16(${y}))')
	}
	return res
}

// sub_u16 computes `x` - `y` for u16 values, and panic if overflow occurs
@[inline]
fn sub_u16(x u16, y u16) u16 {
	mut res := u16(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res = x - y
		is_overflow = x < y
	}
	if is_overflow {
		panic('attempt to sub with overflow(u16(${x}) - u16(${y}))')
	}
	return res
}

// mul_i16 computes `x` * `y` for i16 values, and panic if overflow occurs
@[inline]
fn mul_i16(x i16, y i16) i16 {
	mut res := i16(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res64 := i64(x) * i64(y)
		res = i16(res64)
		is_overflow = res64 > max_i16 || res64 < min_i16
	}
	if is_overflow {
		panic('attempt to mul with overflow(i16(${x}) * i16(${y}))')
	}
	return res
}

// mul_u16 computes `x` * `y` for u16 values, and panic if overflow occurs
@[inline]
fn mul_u16(x u16, y u16) u16 {
	mut res := u16(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res64 := u64(x) * u64(y)
		res = u16(res64)
		is_overflow = res64 > max_u16
	}
	if is_overflow {
		panic('attempt to mul with overflow(u16(${x}) * u16(${y}))')
	}
	return res
}

// add_i32 computes `x` + `y` for i32 values, and panic if overflow occurs
@[inline]
fn add_i32(x i32, y i32) i32 {
	mut res := i32(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res64 := i64(x) + i64(y)
		res = i32(res64)
		is_overflow = res64 > max_i32 || res64 < min_i32
	}
	if is_overflow {
		panic('attempt to add with overflow(i32(${x}) + i32(${y}))')
	}
	return res
}

// add_u32 computes `x` + `y` for u32 values, and panic if overflow occurs
@[inline]
fn add_u32(x u32, y u32) u32 {
	mut res := u32(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res = x + y
		is_overflow = res < x
	}
	if is_overflow {
		panic('attempt to add with overflow(u32(${x}) + u32(${y}))')
	}
	return res
}

// sub_i32 computes `x` - `y` for i32 values, and panic if overflow occurs
@[inline]
fn sub_i32(x i32, y i32) i32 {
	mut res := i32(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res64 := i64(x) - i64(y)
		res = i32(res64)
		is_overflow = res64 > max_i32 || res64 < min_i32
	}
	if is_overflow {
		panic('attempt to sub with overflow(i32(${x}) - i32(${y}))')
	}
	return res
}

// sub_u32 computes `x` - `y` for u32 values, and panic if overflow occurs
@[inline]
fn sub_u32(x u32, y u32) u32 {
	mut res := u32(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res = x - y
		is_overflow = x < y
	}
	if is_overflow {
		panic('attempt to sub with overflow(u32(${x}) - u32(${y}))')
	}
	return res
}

// mul_i32 computes `x` * `y` for i32 values, and panic if overflow occurs
@[inline]
fn mul_i32(x i32, y i32) i32 {
	mut res := i32(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res64 := i64(x) * i64(y)
		res = i32(res64)
		is_overflow = res64 > max_i32 || res64 < min_i32
	}
	if is_overflow {
		panic('attempt to mul with overflow(i32(${x}) * i32(${y}))')
	}
	return res
}

// mul_u32 computes `x` * `y` for u32 values, and panic if overflow occurs
@[inline]
fn mul_u32(x u32, y u32) u32 {
	mut res := u32(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res64 := u64(x) * u64(y)
		res = u32(res64)
		is_overflow = res64 > max_u32
	}
	if is_overflow {
		panic('attempt to mul with overflow(u32(${x}) * u32(${y}))')
	}
	return res
}

// add_i64 computes `x` + `y` for i64 values, and panic if overflow occurs
@[inline]
fn add_i64(x i64, y i64) i64 {
	mut res := i64(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res = x + y
		is_overflow = (x > 0 && y > 0 && res < 0) || (x < 0 && y < 0 && res > 0)
	}
	if is_overflow {
		panic('attempt to add with overflow(i64(${x}) + i64(${y}))')
	}
	return res
}

// add_u64 computes `x` + `y` for u64 values, and panic if overflow occurs
@[inline]
fn add_u64(x u64, y u64) u64 {
	mut res := u64(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_add_overflow(x, y, &res)
	} $else {
		res = x + y
		is_overflow = res < x
	}
	if is_overflow {
		panic('attempt to add with overflow(u64(${x}) + u64(${y}))')
	}
	return res
}

// sub_i64 computes `x` - `y` for i64 values, and panic if overflow occurs
@[inline]
fn sub_i64(x i64, y i64) i64 {
	mut res := i64(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res = x - y
		is_overflow = (x >= 0 && y < 0 && res < 0) || (x < 0 && y > 0 && res > 0)
	}
	if is_overflow {
		panic('attempt to sub with overflow(i64(${x}) - i64(${y}))')
	}
	return res
}

// sub_u64 computes `x` - `y` for u64 values, and panic if overflow occurs
@[inline]
fn sub_u64(x u64, y u64) u64 {
	mut res := u64(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_sub_overflow(x, y, &res)
	} $else {
		res = x - y
		is_overflow = x < y
	}
	if is_overflow {
		panic('attempt to sub with overflow(u64(${x}) - u64(${y}))')
	}
	return res
}

// mul_i64 computes `x` * `y` for i64 values, and panic if overflow occurs
@[inline]
fn mul_i64(x i64, y i64) i64 {
	mut res := i64(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res = x * y
		if x == 0 || y == 0 {
			is_overflow = false
		} else if x == min_i64 {
			is_overflow = y != 1
		} else if y == min_i64 {
			is_overflow = x != 1
		} else {
			if x > 0 {
				if y > 0 {
					is_overflow = x > max_i64 / y
				} else {
					is_overflow = y < min_i64 / x
				}
			} else if x < 0 {
				if y > 0 {
					is_overflow = x < min_i64 / y
				} else {
					is_overflow = y < max_i64 / x
				}
			}
		}
	}
	if is_overflow {
		panic('attempt to mul with overflow(i64(${x}) * i64(${y}))')
	}
	return res
}

// mul_u64 computes `x` * `y` for u64 values, and panic if overflow occurs
@[inline]
fn mul_u64(x u64, y u64) u64 {
	mut res := u64(0)
	mut is_overflow := false
	$if gcc || clang {
		is_overflow = C.__builtin_mul_overflow(x, y, &res)
	} $else {
		res = x * y
		is_overflow = y != 0 && x > max_u64 / y
	}
	if is_overflow {
		panic('attempt to mul with overflow(u64(${x}) * u64(${y}))')
	}
	return res
}

// neg_<T>, div_<T>, mod_<T>, shl_<T> and shr_<T> back the `-check-overflow` checks for unary
// negation, division, modulo and shift counts. The cast_overflow_* functions report the
// lossy integer casts that `-check-casts` detects.

// neg_i8 computes -`x` for i8 values, and panic if overflow occurs
@[inline]
fn neg_i8(x i8) i8 {
	if x == min_i8 {
		panic('attempt to neg with overflow(-i8(${x}))')
	}
	return -x
}

// neg_i16 computes -`x` for i16 values, and panic if overflow occurs
@[inline]
fn neg_i16(x i16) i16 {
	if x == min_i16 {
		panic('attempt to neg with overflow(-i16(${x}))')
	}
	return -x
}

// neg_i32 computes -`x` for i32 values, and panic if overflow occurs
@[inline]
fn neg_i32(x i32) i32 {
	if x == min_i32 {
		panic('attempt to neg with overflow(-i32(${x}))')
	}
	return -x
}

// neg_i64 computes -`x` for i64 values, and panic if overflow occurs
@[inline]
fn neg_i64(x i64) i64 {
	if x == min_i64 {
		panic('attempt to neg with overflow(-i64(${x}))')
	}
	return -x
}

// div_i8 computes `x` / `y` for i8 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn div_i8(x i8, y i8) i8 {
	if y == 0 {
		panic('division by zero')
	}
	if y == -1 && x == min_i8 {
		panic('attempt to div with overflow(i8(${x}) / i8(${y}))')
	}
	return x / y
}

// mod_i8 computes `x` % `y` for i8 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn mod_i8(x i8, y i8) i8 {
	if y == 0 {
		panic('modulo by zero')
	}
	if y == -1 && x == min_i8 {
		panic('attempt to mod with overflow(i8(${x}) % i8(${y}))')
	}
	return x % y
}

// div_i16 computes `x` / `y` for i16 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn div_i16(x i16, y i16) i16 {
	if y == 0 {
		panic('division by zero')
	}
	if y == -1 && x == min_i16 {
		panic('attempt to div with overflow(i16(${x}) / i16(${y}))')
	}
	return x / y
}

// mod_i16 computes `x` % `y` for i16 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn mod_i16(x i16, y i16) i16 {
	if y == 0 {
		panic('modulo by zero')
	}
	if y == -1 && x == min_i16 {
		panic('attempt to mod with overflow(i16(${x}) % i16(${y}))')
	}
	return x % y
}

// div_i32 computes `x` / `y` for i32 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn div_i32(x i32, y i32) i32 {
	if y == 0 {
		panic('division by zero')
	}
	if y == -1 && x == min_i32 {
		panic('attempt to div with overflow(i32(${x}) / i32(${y}))')
	}
	return x / y
}

// mod_i32 computes `x` % `y` for i32 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn mod_i32(x i32, y i32) i32 {
	if y == 0 {
		panic('modulo by zero')
	}
	if y == -1 && x == min_i32 {
		panic('attempt to mod with overflow(i32(${x}) % i32(${y}))')
	}
	return x % y
}

// div_i64 computes `x` / `y` for i64 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn div_i64(x i64, y i64) i64 {
	if y == 0 {
		panic('division by zero')
	}
	if y == -1 && x == min_i64 {
		panic('attempt to div with overflow(i64(${x}) / i64(${y}))')
	}
	return x / y
}

// mod_i64 computes `x` % `y` for i64 values, and panic on a zero divisor or if overflow occurs
@[inline]
fn mod_i64(x i64, y i64) i64 {
	if y == 0 {
		panic('modulo by zero')
	}
	if y == -1 && x == min_i64 {
		panic('attempt to mod with overflow(i64(${x}) % i64(${y}))')
	}
	return x % y
}

// shl_i8 computes `x` << `n` for i8 values, and panic if `n` is negative or at least 8
@[inline]
fn shl_i8(x i8, n i64) i8 {
	if n < 0 || n >= 8 {
		panic('attempt to shl with overflow(i8(${x}) << ${n})')
	}
	return x << n
}

// shr_i8 computes `x` >> `n` for i8 values, and panic if `n` is negative or at least 8
@[inline]
fn shr_i8(x i8, n i64) i8 {
	if n < 0 || n >= 8 {
		panic('attempt to shr with overflow(i8(${x}) >> ${n})')
	}
	return x >> n
}

// shl_u8 computes `x` << `n` for u8 values, and panic if `n` is negative or at least 8
@[inline]
fn shl_u8(x u8, n i64) u8 {
	if n < 0 || n >= 8 {
		panic('attempt to shl with overflow(u8(${x}) << ${n})')
	}
	return x << n
}

// shr_u8 computes `x` >> `n` for u8 values, and panic if `n` is negative or at least 8
@[inline]
fn shr_u8(x u8, n i64) u8 {
	if n < 0 || n >= 8 {
		panic('attempt to shr with overflow(u8(${x}) >> ${n})')
	}
	return x >> n
}

// shl_i16 computes `x` << `n` for i16 values, and panic if `n` is negative or at least 16
@[inline]
fn shl_i16(x i16, n i64) i16 {
	if n < 0 || n >= 16 {
		panic('attempt to shl with overflow(i16(${x}) << ${n})')
	}
	return x << n
}

// shr_i16 computes `x` >> `n` for i16 values, and panic if `n` is negative or at least 16
@[inline]
fn shr_i16(x i16, n i64) i16 {
	if n < 0 || n >= 16 {
		panic('attempt to shr with overflow(i16(${x}) >> ${n})')
	}
	return x >> n
}

// shl_u16 computes `x` << `n` for u16 values, and panic if `n` is negative or at least 16
@[inline]
fn shl_u16(x u16, n i64) u16 {
	if n < 0 || n >= 16 {
		panic('attempt to shl with overflow(u16(${x}) << ${n})')
	}
	return x << n
}

// shr_u16 computes `x` >> `n` for u16 values, and panic if `n` is negative or at least 16
@[inline]
fn shr_u16(x u16, n i64) u16 {
	if n < 0 || n >= 16 {
		panic('attempt to shr with overflow(u16(${x}) >> ${n})')
	}
	return x >> n
}

// shl_i32 computes `x` << `n` for i32 values, and panic if `n` is negative or at least 32
@[inline]
fn shl_i32(x i32, n i64) i32 {
	if n < 0 || n >= 32 {
		panic('attempt to shl with overflow(i32(${x}) << ${n})')
	}
	return x << n
}

// shr_i32 computes `x` >> `n` for i32 values, and panic if `n` is negative or at least 32
@[inline]
fn shr_i32(x i32, n i64) i32 {
	if n < 0 || n >= 32 {
		panic('attempt to shr with overflow(i32(${x}) >> ${n})')
	}
	return x >> n
}

// shl_u32 computes `x` << `n` for u32 values, and panic if `n` is negative or at least 32
@[inline]
fn shl_u32(x u32, n i64) u32 {
	if n < 0 || n >= 32 {
		panic('attempt to shl with overflow(u32(${x}) << ${n})')
	}
	return x << n
}

// shr_u32 computes `x` >> `n` for u32 values, and panic if `n` is negative or at least 32
@[inline]
fn shr_u32(x u32, n i64) u32 {
	if n < 0 || n >= 32 {
		panic('attempt to shr with overflow(u32(${x}) >> ${n})')
	}
	return x >> n
}

// shl_i64 computes `x` << `n` for i64 values, and panic if `n` is negative or at least 64
@[inline]
fn shl_i64(x i64, n i64) i64 {
	if n < 0 || n >= 64 {
		panic('attempt to shl with overflow(i64(${x}) << ${n})')
	}
	return x << n
}

// shr_i64 computes `x` >> `n` for i64 values, and panic if `n` is negative or at least 64
@[inline]
fn shr_i64(x i64, n i64) i64 {
	if n < 0 || n >= 64 {
		panic('attempt to shr with overflow(i64(${x}) >> ${n})')
	}
	return x >> n
}

// shl_u64 computes `x` << `n` for u64 values, and panic if `n` is negative or at least 64
@[inline]
fn shl_u64(x u64, n i64) u64 {
	if n < 0 || n >= 64 {
		panic('attempt to shl with overflow(u64(${x}) << ${n})')
	}
	return x << n
}

// shr_u64 computes `x` >> `n` for u64 values, and panic if `n` is negative or at least 64
@[inline]
fn shr_u64(x u64, n i64) u64 {
	if n < 0 || n >= 64 {
		panic('attempt to shr with overflow(u64(${x}) >> ${n})')
	}
	return x >> n
}

// cast_overflow_signed panics, because the value `x` of the signed integer type `src` does not fit in `dst`
@[noinline]
fn cast_overflow_signed(x i64, src string, dst string) {
	panic('attempt to cast with overflow(${dst}(${src}(${x})))')
}

// cast_overflow_unsigned panics, because the value `x` of the unsigned integer type `src` does not fit in `dst`
@[noinline]
fn cast_overflow_unsigned(x u64, src string, dst string) {
	panic('attempt to cast with overflow(${dst}(${src}(${x})))')
}

/////////////////////////////////////////////////////////////////////////////////////////////
// These are here to prevent cgen errors for `./v -check-overflow vlib/math/vec/vec4_test.v`
// TODO: check for correctness and add tests. Improve markused so it keeps them, when they are used.
@[inline; markused]
fn add_int(x int, y int) int {
	return add_i32(x, y)
}

@[inline; markused]
fn sub_int(x int, y int) int {
	return sub_i32(x, y)
}

@[inline; markused]
fn mul_int(x int, y int) int {
	return mul_i32(x, y)
}

@[inline; markused]
fn add_f32(x f32, y f32) f32 {
	return x + y
}

@[inline; markused]
fn sub_f32(x f32, y f32) f32 {
	return x - y
}

@[inline; markused]
fn mul_f32(x f32, y f32) f32 {
	return x * y
}

@[inline; markused]
fn add_f64(x f64, y f64) f64 {
	return x + y
}

@[inline; markused]
fn sub_f64(x f64, y f64) f64 {
	return x - y
}

@[inline; markused]
fn mul_f64(x f64, y f64) f64 {
	return x * y
}
