enum EmbeddedBindingStatus {
	uninitialized
	lexical
	initialized
}

struct EmbeddedBaseEnvironment {
mut:
	this_binding_status EmbeddedBindingStatus = .uninitialized
}

// The outer struct has a method with the same name as a field it embeds.
@[heap]
struct EmbeddedFunctionEnvironment {
	EmbeddedBaseEnvironment
}

fn (environment &EmbeddedFunctionEnvironment) this_binding_status() EmbeddedBindingStatus {
	return environment.this_binding_status
}

fn (mut environment EmbeddedFunctionEnvironment) bind(status EmbeddedBindingStatus) {
	environment.this_binding_status = status
}

fn (environment &EmbeddedFunctionEnvironment) has_this_binding() bool {
	return environment.this_binding_status != .lexical
}

fn test_promoted_field_named_like_a_method_is_read_as_the_field() {
	mut environment := &EmbeddedFunctionEnvironment{}
	assert environment.has_this_binding()
	assert environment.this_binding_status() == .uninitialized
	environment.bind(.lexical)
	assert !environment.has_this_binding()
	assert environment.this_binding_status == .lexical
	assert environment.this_binding_status() == .lexical
}
