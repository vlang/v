struct Root {}

struct Holder {
	root &Root = unsafe { nil }
}

const empty_holder = Holder{
	root: &Root(unsafe { nil })
}

fn test_address_of_struct_cast_from_unsafe_nil_is_a_null_pointer() {
	assert isnil(empty_holder.root)
}
