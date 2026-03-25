// Test that array type aliases preserve their alias type when initialized with `{}`,
// so that methods defined on the alias type are accessible.
// See: https://github.com/vlang/v/issues/26767
import strings

fn test_builder_alias_preserves_type_on_init() {
	mut builder := strings.Builder{}
	builder.write_decimal(42)
	assert builder.str() == '42'
}

fn test_builder_new_builder() {
	mut builder := strings.new_builder(10)
	builder.write_decimal(123)
	assert builder.str() == '123'
}
