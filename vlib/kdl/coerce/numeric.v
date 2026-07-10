module coerce

import kdl.document

// as_numeric returns (i64, f64, is_int) for a value, useful for generic numeric handling.
pub fn as_numeric(v document.Value) (i64, f64, bool) {
	match v {
		document.IntVal {
			return v.value, f64(v.value), true
		}
		document.FloatVal {
			return i64(v.value), v.value, false
		}
		document.StringVal {
			if v.value.contains('.') {
				return 0, v.value.f64(), false
			}
			return v.value.i64(), 0, true
		}
		document.BoolVal {
			return if v.value { i64(1) } else { i64(0) }, 0, true
		}
		document.NullVal {
			return i64(0), 0.0, true
		}
	}

	return 0, 0.0, true
}
