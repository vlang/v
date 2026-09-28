import v.tests.generics.variadic_signature_module

struct Param {
	n int
}

fn test_generic_variadic_method_uses_declaring_module_param() {
	gate := variadic_signature_module.Gate[int]{ value: 3 }
	assert gate.choose(variadic_signature_module.Param{ n: 42 }) == 42
}
