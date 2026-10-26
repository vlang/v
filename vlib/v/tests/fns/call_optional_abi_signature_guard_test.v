type OptionalAbiInt = int

struct OptionalAbiCallbacks {
	callback fn (?int) int @[required]
}

fn optional_abi_read(value ?int) int {
	return value or { -1 }
}

fn optional_abi_read_alias(value ?OptionalAbiInt) int {
	return int(value or { -2 })
}

fn optional_abi_read_generic[T](value ?T, fallback T) T {
	return value or { fallback }
}

fn optional_abi_invoke[F](callback F, value ?int) int {
	return callback(value)
}

fn test_optional_abi_signature_guard_preserves_calls_and_function_values() {
	assert optional_abi_read(none) == -1
	assert optional_abi_read(7) == 7
	assert optional_abi_read_alias(none) == -2
	assert optional_abi_read_alias(?int(9)) == 9
	assert optional_abi_read_generic[int](none, 10) == 10
	assert optional_abi_read_generic[string](?string('present'), 'missing') == 'present'
	callback := optional_abi_read
	assert callback(none) == -1
	assert callback(?int(11)) == 11
	callbacks := OptionalAbiCallbacks{ callback: optional_abi_read }
	assert callbacks.callback(none) == -1
	assert callbacks.callback(12) == 12
	assert optional_abi_invoke[fn (?int) int](optional_abi_read, none) == -1
	assert optional_abi_invoke[fn (?int) int](optional_abi_read, ?int(13)) == 13
}
