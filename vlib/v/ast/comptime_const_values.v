module ast

pub type ComptTimeConstValue = EmptyExpr | byte | f64 | i64 | rune | string | u64

pub fn empty_comptime_const_expr() ComptTimeConstValue {
	return EmptyExpr{}
}

pub fn (val ComptTimeConstValue) i64() ?i64 {
	match val {
		byte {
			return i64(val)
		}
		i64 {
			return i64(val)
		}
		f64 {
			if -9223372036854775808.0 <= val && val <= 9223372036854775807.0 {
				return i64(val)
			}
		}
		u64 {
			if val <= 9223372036854775807 {
				return i64(val)
			}
		}
		string {
			return val.i64()
		}
		rune {
			return int(val)
		}
		EmptyExpr {}
	}
	return none
}

pub fn (val ComptTimeConstValue) int() ?int {
	match val {
		u64 {
			if val <= 2147483647 {
				return int(val)
			}
		}
		f64 {
			if -2147483648.0 <= val && val <= 2147483647.0 {
				return int(val)
			}
		}
		i64 {
			if -2147483648 <= val && val <= 2147483647 {
				return int(val)
			}
		}
		byte {
			return int(val)
		}
		string {
			return val.int()
		}
		rune, EmptyExpr {}
	}
	return none
}

pub fn (val ComptTimeConstValue) string() ?string {
	match val {
		u64 {
			return val.str()
		}
		i64 {
			return val.str()
		}
		f64 {
			return val.str()
		}
		byte {
			return val.str()
		}
		rune {
			return val.str()
		}
		string {
			return val
		}
		EmptyExpr {}
	}
	return none
}

pub fn (val ComptTimeConstValue) f64() ?f64 {
	match val {
		i64 {
			return f64(val)
		}
		u64 {
			return f64(val)
		}
		byte {
			return f64(val)
		}
		f64 {
			return val
		}
		string {
			return val.f64()
		}
		rune {}
		EmptyExpr {}
	}
	return none
}

pub fn (val ComptTimeConstValue) u64() ?u64 {
	match val {
		i64 {
			if val >= 0 {
				return u64(val)
			}
		}
		u64 {
			return val
		}
		byte {
			return u64(val)
		}
		f64 {
			if val <= 18446744073709551615.0 {
				return u64(val)
			}
		}
		string {
			return val.u64()
		}
		rune {}
		EmptyExpr {}
	}
	return none
}

pub fn (val ComptTimeConstValue) byte() ?byte {
	match val {
		byte {
			return val
		}
		u64 {
			if val <= 255 {
				return byte(val)
			}
		}
		f64 {
			if 0 <= val && val <= 255 {
				return byte(val)
			}
		}
		i64 {
			if 0 <= val && val <= 255 {
				return byte(val)
			}
		}
		string {
			x := val.int()
			if 0 <= x && x <= 255 {
				return byte(x)
			}
		}
		rune {
			x := u32(val)
			if 0 <= x && x <= 255 {
				return byte(x)
			}
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
