module ast

pub type ComptTimeConstValue = EmptyExpr | byte | f64 | i64 | rune | string

pub fn empty_comptime_const_expr() ComptTimeConstValue {
	return EmptyExpr{}
}

pub fn (val ComptTimeConstValue) i64() ?i64 {
	match val {
		i64, byte {
			return i64(val)
		}
		f64 {
			if -9223372036854775808.0 <= val && val <= 9223372036854775807.0 {
				return i64(val)
			}
			return none
		}
		string {
			return val.i64()
		}
		rune {
			return int(val)
		}
		EmptyExpr {
			return none
		}
	}
	return none
}

pub fn (val ComptTimeConstValue) int() ?int {
	match val {
		f64 {
			if -2147483648.0 <= val && val <= 2147483647.0 {
				return int(val)
			}
			return none
		}
		i64 {
			if -2147483648 <= val && val <= 2147483647 {
				return int(val)
			}
			return none
		}
		byte {
			return int(val)
		}
		string {
			return val.int()
		}
		rune {
			return none
		}
		EmptyExpr {
			return none
		}
	}
	return none
}

pub fn (val ComptTimeConstValue) string() ?string {
	match val {
		i64, f64, byte {
			return val.str()
		}
		string {
			return val
		}
		rune {
			return val.str()
		}
		EmptyExpr {
			return none
		}
	}
	return none
}

pub fn (val ComptTimeConstValue) f64() ?f64 {
	match val {
		f64 {
			return val
		}
		i64, byte {
			return f64(val)
		}
		string {
			return val.f64()
		}
		rune {
			return none
		}
		EmptyExpr {
			return none
		}
	}
	return none
}

pub fn (val ComptTimeConstValue) byte() ?byte {
	match val {
		byte {
			return val
		}
		f64 {
			if 0 <= val && val <= 255 {
				return byte(val)
			}
			return none
		}
		i64 {
			if 0 <= val && val <= 255 {
				return byte(val)
			}
			return none
		}
		string {
			x := val.int()
			if 0 <= x && x <= 255 {
				return byte(x)
			}
			return none
		}
		rune {
			x := u32(val)
			if 0 <= x && x <= 255 {
				return byte(x)
			}
			return none
		}
		EmptyExpr {
			return none
		}
	}
	return none
}
