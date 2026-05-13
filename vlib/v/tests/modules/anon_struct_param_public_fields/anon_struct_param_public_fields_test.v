module main

import anon_struct_param_public_fields.foo

fn test_anon_struct_parameter_fields_are_public() {
	assert foo.bar(name: 'Foo!') == 'Foo!'
}
