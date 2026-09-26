struct ClosureFieldHolder {
mut:
	callback fn () int = unsafe { nil }
}

fn set_closure_field_callback(ptr voidptr, value int) {
	mut holder := unsafe { &ClosureFieldHolder(ptr) }
	holder.callback = fn [value] () int {
		return value
	}
}

fn test_closure_stored_through_cast_pointer_survives_setter() {
	mut holder := &ClosureFieldHolder{}
	set_closure_field_callback(voidptr(holder), 42)
	assert holder.callback() == 42
}
