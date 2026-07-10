module coerce

import kdl.document

pub fn is_null(v document.Value) bool {
	match v {
		document.NullVal { return true }
		else { return false }
	}
}
