module coerce

import kdl.document

pub fn as_string(v document.Value) string {
	match v {
		document.StringVal { return v.value }
		document.IntVal { return v.value.str() }
		document.FloatVal { return v.value.str() }
		document.BoolVal { return if v.value { 'true' } else { 'false' } }
		document.NullVal { return 'null' }
	}

	return ''
}

pub fn is_string(v document.Value) bool {
	match v {
		document.StringVal { return true }
		else { return false }
	}
}
