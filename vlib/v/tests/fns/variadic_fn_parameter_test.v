type VariadicCallback = fn (int, ...string) int

struct VariadicCallbacks {
	callback VariadicCallback @[required]
}

fn variadic_parameter_impl(n int, args ...string) int {
	return n + args.len
}

fn call_variadic_alias_parameter(callback VariadicCallback) {
	assert callback(10) == 10
	assert callback(10, 'a', 'b') == 12
	args := ['a', 'b', 'c']
	assert callback(10, ...args) == 13
}

fn call_variadic_direct_parameter(callback fn (int, ...string) int) {
	assert callback(20) == 20
	assert callback(20, 'a') == 21
}

fn call_forwarded_variadic_parameter(callback VariadicCallback) {
	assert callback(30) == 30
	call_variadic_alias_parameter(callback)
}

fn call_fixed_array_parameter(callback fn (int, []string) int) {
	assert callback(40, ['a', 'b']) == 42
}

fn fixed_array_parameter_impl(n int, args []string) int {
	return n + args.len
}

fn test_variadic_function_parameters_keep_empty_and_nonempty_tails() {
	call_variadic_alias_parameter(variadic_parameter_impl)
	call_variadic_direct_parameter(variadic_parameter_impl)
	call_forwarded_variadic_parameter(variadic_parameter_impl)
	call_fixed_array_parameter(fixed_array_parameter_impl)
	local := VariadicCallback(variadic_parameter_impl)
	assert local(50) == 50
	callbacks := VariadicCallbacks{ callback: variadic_parameter_impl }
	assert callbacks.callback(60) == 60
	alias := callbacks.callback
	assert alias(70) == 70
	indexed := [VariadicCallback(variadic_parameter_impl)]
	assert indexed[0](80) == 80
	assert local(90) == 90
}

interface VariadicValue {
	value() int
}

struct VariadicConcrete {}

fn (value VariadicConcrete) value() int {
	return 100
}

type VariadicInterfaceCallback = fn (VariadicValue, ...string) int

struct VariadicInterfaceCallbacks {
	callback VariadicInterfaceCallback @[required]
}

fn variadic_interface_parameter_impl(value VariadicValue, args ...string) int {
	return value.value() + args.len
}

fn test_variadic_function_fields_with_interface_first_argument() {
	callbacks := VariadicInterfaceCallbacks{ callback: variadic_interface_parameter_impl }
	assert callbacks.callback(VariadicConcrete{}) == 100
	assert callbacks.callback(VariadicConcrete{}, 'x') == 101
	args := ['a', 'b']
	assert callbacks.callback(VariadicConcrete{}, ...args) == 102
	pointer := &callbacks
	assert pointer.callback(VariadicConcrete{}) == 100
}
