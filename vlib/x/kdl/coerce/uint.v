module coerce

import x.kdl.document

// as_u64 extracts a u64 value from a KDL Value.
pub fn as_u64(v document.Value) u64 {
	match v {
		document.IntVal { return u64(v.value) }
		document.FloatVal { return u64(v.value) }
		document.StringVal { return v.value.u64() }
		document.BoolVal { return if v.value { u64(1) } else { u64(0) } }
		document.NullVal { return u64(0) }
	}

	return u64(0)
}
