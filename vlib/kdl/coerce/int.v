module coerce

import kdl.document

pub fn as_int(v document.Value) int {
	match v {
		document.IntVal { return int(v.value) }
		document.FloatVal { return int(v.value) }
		document.StringVal { return v.value.int() }
		document.BoolVal { return if v.value { 1 } else { 0 } }
		document.NullVal { return 0 }
	}

	return 0
}

pub fn as_i64(v document.Value) i64 {
	match v {
		document.IntVal { return v.value }
		document.FloatVal { return i64(v.value) }
		document.StringVal { return v.value.i64() }
		document.BoolVal { return if v.value { i64(1) } else { i64(0) } }
		document.NullVal { return i64(0) }
	}

	return i64(0)
}

pub fn is_int(v document.Value) bool {
	match v {
		document.IntVal { return true }
		else { return false }
	}
}
