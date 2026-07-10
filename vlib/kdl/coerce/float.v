module coerce

import kdl.document

pub fn as_f64(v document.Value) f64 {
	match v {
		document.FloatVal { return v.value }
		document.IntVal { return f64(v.value) }
		document.StringVal { return v.value.f64() }
		document.BoolVal { return if v.value { 1.0 } else { 0.0 } }
		document.NullVal { return 0.0 }
	}

	return 0.0
}

pub fn is_float(v document.Value) bool {
	match v {
		document.FloatVal { return true }
		else { return false }
	}
}
