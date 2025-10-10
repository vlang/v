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
