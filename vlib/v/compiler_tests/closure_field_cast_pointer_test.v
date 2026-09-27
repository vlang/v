@[heap]
struct ClosureFieldHolder {
mut:
	callback fn () int = unsafe { nil }
}

struct ClosureNestedHolder {
mut:
	holder &ClosureFieldHolder
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

fn set_nested_closure_field_callback(mut holder ClosureFieldHolder, value int) {
	mut wrapper := ClosureNestedHolder{
		holder: holder
	}
	wrapper.holder.callback = fn [value] () int {
		return value
	}
}

fn test_closure_stored_through_nested_pointer_survives_setter() {
	mut holder := &ClosureFieldHolder{}
	set_nested_closure_field_callback(mut holder, 43)
	assert holder.callback() == 43
}

fn test_scope_owned_pointer_field_callback() {
	for i in 0 .. 3 {
		mut holder := &ClosureFieldHolder{}
		holder.callback = fn [i] () int {
			return i
		}
		assert holder.callback() == i
	}
}
