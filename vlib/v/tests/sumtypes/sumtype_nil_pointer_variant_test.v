type Namable = string | charptr

fn takes_name(name Namable) bool {
	if name is charptr {
		return name == unsafe { nil }
	}
	return false
}

fn test_sumtype_with_single_pointer_variant_accepts_unsafe_nil() {
	assert takes_name(unsafe { nil })
}
