type NativeWord = u64
type NativeHandle = NativeWord
type NativeResource = NativeHandle

struct NativeState {
	handle   NativeHandle
	resource NativeResource
}

struct PrimitiveState {
	value u64
}

fn native_handle() NativeHandle {
	return NativeHandle(13)
}

fn test_nested_alias_values_initialize_struct_fields() {
	handle := NativeHandle(7)
	resource := NativeResource(11)
	state := NativeState{ handle: handle, resource: resource }
	assert u64(state.handle) == 7
	assert u64(state.resource) == 11
	copied := NativeState{ handle: state.handle, resource: state.resource }
	assert u64(copied.handle) == 7
	assert u64(copied.resource) == 11
	from_call := NativeState{ handle: native_handle(), resource: resource }
	assert u64(from_call.handle) == 13
	primitive := PrimitiveState{ value: resource }
	assert primitive.value == 11
}
