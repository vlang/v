module coerce

import kdl.document

pub fn as_bool(v document.Value) bool {
	match v {
		document.BoolVal { return v.value }
		document.IntVal { return v.value != 0 }
		document.FloatVal { return v.value != 0.0 }
		document.StringVal { return v.value.to_lower() in ['true', 'yes', 'on', '1'] }
		document.NullVal { return false }
	}

	return false
}

pub fn is_bool(v document.Value) bool {
	match v {
		document.BoolVal { return true }
		else { return false }
	}
}
