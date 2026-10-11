module main

type AddressCallback = fn () int

struct AddressCallbackHolder {
mut:
	callback AddressCallback = address_callback_five
}

struct AddressCallbackPointerHolder {
	pointer voidptr
}

fn address_callback_five() int {
	return 5
}

fn address_callback_seven() int {
	return 7
}

fn copy_address_callback(value AddressCallback) AddressCallback {
	assert voidptr(&value) != voidptr(address_callback_five)
	// Copy the pointer stored in the parameter, rather than bytes of executable code.
	copied := unsafe { &AddressCallback(memdup(&value, int(sizeof(AddressCallback)))) }
	return *copied
}

fn copy_address_generic[T](value T) T {
	// T can be a function type whose storage address differs from its value.
	copied := unsafe { &T(memdup(&value, int(sizeof(T)))) }
	return *copied
}

fn test_local_function_value_address_and_implicit_voidptr_argument() {
	mut value := AddressCallback(address_callback_five)
	assert voidptr(&value) != voidptr(address_callback_five)
	copied := unsafe { &AddressCallback(memdup(&value, int(sizeof(AddressCallback)))) }
	value = address_callback_seven
	assert value() == 7
	assert (*copied)() == 5
}

fn test_parameter_and_generic_function_value_storage_addresses() {
	value := AddressCallback(address_callback_five)
	assert copy_address_callback(value)() == 5
	assert copy_address_generic(value)() == 5
}

fn test_function_field_and_array_element_storage_addresses() {
	mut holder := AddressCallbackHolder{ callback: address_callback_five }
	assert voidptr(&holder.callback) != voidptr(address_callback_five)
	copied := unsafe { &AddressCallback(memdup(&holder.callback, int(sizeof(AddressCallback)))) }
	holder.callback = address_callback_seven
	assert (*copied)() == 5
	values := [AddressCallback(address_callback_five)]
	assert voidptr(&values[0]) != voidptr(address_callback_five)
	element := unsafe { &AddressCallback(memdup(&values[0], int(sizeof(AddressCallback)))) }
	assert (*element)() == 5
}

fn test_named_function_address_remains_the_function_value() {
	assert voidptr(&address_callback_five) == voidptr(address_callback_five)
}

fn test_implicit_voidptr_comparisons_keep_function_value_storage_addresses() {
	value := AddressCallback(address_callback_five)
	stored := AddressCallbackPointerHolder{ pointer: &value }
	assert stored.pointer == &value
	assert &value == stored.pointer
	assert stored.pointer != address_callback_five
	assert address_callback_five != stored.pointer
	holder := AddressCallbackHolder{ callback: address_callback_five }
	field := AddressCallbackPointerHolder{ pointer: &holder.callback }
	assert field.pointer == &holder.callback
	assert &holder.callback == field.pointer
	assert field.pointer != address_callback_five
	values := [AddressCallback(address_callback_five)]
	element := AddressCallbackPointerHolder{ pointer: &values[0] }
	assert element.pointer == &values[0]
	assert &values[0] == element.pointer
	assert element.pointer != address_callback_five
	named := AddressCallbackPointerHolder{ pointer: &address_callback_five }
	assert named.pointer == &address_callback_five
	assert &address_callback_five == named.pointer
}
