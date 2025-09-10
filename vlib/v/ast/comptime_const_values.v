module ast

pub type ComptTimeConstValue = EmptyExpr
	| f32
	| f64
	| i16
	| i32
	| i64
	| i8
	| rune
	| string
	| u16
	| u32
	| u64
	| u8
	| voidptr

//| int

// i8 tries to return a `ComptTimeConstValue` as `i8` type.
pub fn (val ComptTimeConstValue) i8() ?i8 {
	x := val.i64()?
	if x > -129 && x < 128 {
		return i8(x)
	}
	return none
}

// i16 tries to return a `ComptTimeConstValue` as `i16` type.
pub fn (val ComptTimeConstValue) i16() ?i16 {
	x := val.i64()?
	if x > -32769 && x < 32768 {
		return i16(x)
	}
	return none
}

// int tries to return a `ComptTimeConstValue` as `int` type.
pub fn (val ComptTimeConstValue) int() ?int {
	x := val.i64()?
	if x > -2147483649 && x < 2147483648 {
		return int(x)
	}
	return none
}

// i32 tries to return a `ComptTimeConstValue` as `i32` type.
pub fn (val ComptTimeConstValue) i32() ?i32 {
	x := val.i64()?
	if x > -2147483649 && x < 2147483648 {
		return i32(x)
	}
	return none
}

// voidptr tries to return a `ComptTimeConstValue` as `voidptr` type.
pub fn (val ComptTimeConstValue) voidptr() ?voidptr {
	match val {
		i8, i16, i32, i64 { return voidptr(i64(val)) }
		u8, u16, u32, u64 { return voidptr(u64(val)) }
		rune { return voidptr(u64(val)) }
		voidptr { return val }
		string, EmptyExpr, f32, f64 {}
	}
	return none
}

// i64 tries to return a `ComptTimeConstValue` as i64 type.
pub fn (val ComptTimeConstValue) i64() ?i64 {
	match val {
		i8 {
			return i64(val)
		}
		i16 {
			return i64(val)
		}
		i32 {
			return i64(val)
		}
		i64 {
			return i64(val)
		}
		u8 {
			return i64(val)
		}
		u16 {
			return i64(val)
		}
		u32 {
			return i64(val)
		}
		u64 {
			if val <= 9223372036854775807 {
				return i64(val)
			}
		}
		//
		f32 {
			if -9223372036854775808.0 <= val && val <= 9223372036854775807.0 {
				return i64(val)
			}
		}
		f64 {
			if -9223372036854775808.0 <= val && val <= 9223372036854775807.0 {
				return i64(val)
			}
		}
		//
		string {
			return val.i64()
		}
		rune {
			return int(val)
		}
		voidptr {
			return i64(val)
		}
		EmptyExpr {}
	}
	return none
}

// u8 tries to return a `ComptTimeConstValue` as `u8` type.
pub fn (val ComptTimeConstValue) u8() ?u8 {
	x := val.u64()?
	if x < 256 {
		return u8(x)
	}
	return none
}

// u16 tries to return a `ComptTimeConstValue` as `u16` type.
pub fn (val ComptTimeConstValue) u16() ?u16 {
	x := val.u64()?
	if x < 65536 {
		return u16(x)
	}
	return none
}

// u32 tries to return a `ComptTimeConstValue` as `u32` type.
pub fn (val ComptTimeConstValue) u32() ?u32 {
	x := val.u64()?
	if x < 4294967296 {
		return u32(x)
	}
	return none
}

// u64 tries to return a `ComptTimeConstValue` as `u64` type.
pub fn (val ComptTimeConstValue) u64() ?u64 {
	match val {
		i8 {
			if val >= 0 {
				return u64(val)
			}
		}
		i16 {
			if val >= 0 {
				return u64(val)
			}
		}
		i32 {
			if val >= 0 {
				return u64(val)
			}
		}
		i64 {
			if val >= 0 {
				return u64(val)
			}
		}
		// int {
		// if val >= 0 {
		// return u64(val)
		//}
		//}
		u8 {
			return u64(val)
		}
		u16 {
			return u64(val)
		}
		u32 {
			return u64(val)
		}
		u64 {
			return val
		}
		f32 {
			if val <= 18446744073709551615.0 {
				return u64(val)
			}
		}
		f64 {
			if val <= 18446744073709551615.0 {
				return u64(val)
			}
		}
		string {
			return val.u64()
		}
		voidptr {
			return u64(val)
		}
		rune {}
		EmptyExpr {}
	}
	return none
}

// f32 tries to return a `ComptTimeConstValue` as `f32` type.
pub fn (val ComptTimeConstValue) f32() ?f32 {
	x := val.f64()?
	return f32(x)
}

// f64 tries to return a `ComptTimeConstValue` as `f64` type.
pub fn (val ComptTimeConstValue) f64() ?f64 {
	match val {
		i8 {
			return f64(val)
		}
		i16 {
			return f64(val)
		}
		i32 {
			return f64(val)
		}
		i64 {
			return f64(val)
		}
		// int {
		// return f64(val)
		//}
		u8 {
			return f64(val)
		}
		u16 {
			return f64(val)
		}
		u32 {
			return f64(val)
		}
		u64 {
			return f64(val)
		}
		f32 {
			return f64(val)
		}
		f64 {
			return val
		}
		string {
			return val.f64()
		}
		voidptr {}
		rune {}
		EmptyExpr {}
	}
	return none
}

// string tries to return a `ComptTimeConstValue` as `string` type.
pub fn (val ComptTimeConstValue) string() ?string {
	match val {
		i8 {
			return val.str()
		}
		i16 {
			return val.str()
		}
		i32 {
			return val.str()
		}
		i64 {
			return val.str()
		}
		// int {
		// return val.str()
		//}
		u8 {
			return val.str()
		}
		u16 {
			return val.str()
		}
		u32 {
			return val.str()
		}
		u64 {
			return val.str()
		}
		f32 {
			return val.str()
		}
		f64 {
			return val.str()
		}
		rune {
			return val.str()
		}
		string {
			return val
		}
		voidptr {
			return ptr_str(val)
		}
		EmptyExpr {}
	}
	return none
}

pub fn (obj ConstField) comptime_expr_value() ?ComptTimeConstValue {
	if obj.comptime_expr_value !is EmptyExpr {
		return obj.comptime_expr_value
	}
	return none
}

pub fn (obj ConstField) is_simple_define_const() bool {
	return match obj.expr {
		CharLiteral, FloatLiteral, IntegerLiteral { true }
		else { false }
	}
}

pub fn (obj ScopeObject) is_simple_define_const() bool {
	if obj is ConstField {
		return obj.is_simple_define_const()
	}
	return false
}
