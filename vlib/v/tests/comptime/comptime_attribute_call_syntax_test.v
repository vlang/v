// vfmt off
@[deprecated('use NewPositional instead')]
struct OldPositional {}

@[deprecated(msg: 'use NewNamed instead', after: '2999-01-01')]
struct OldNamed {}

@[deprecated('use NewMixed instead', after: '2999-01-02')]
struct OldMixed {}

@[custom(flag: true, count: 2)]
struct CustomNamed {}
// vfmt on

fn test_attribute_call_syntax_positional_and_named_args() {
	mut positional_msg := ''
	$for attr in OldPositional.attributes {
		if attr.name == 'deprecated' {
			positional_msg = attr.arg
		}
	}
	assert positional_msg == 'use NewPositional instead'

	mut named_msg := ''
	mut named_after := ''
	$for attr in OldNamed.attributes {
		if attr.name == 'deprecated' {
			named_msg = attr.arg
		}
		if attr.name == 'deprecated_after' {
			named_after = attr.arg
		}
	}
	assert named_msg == 'use NewNamed instead'
	assert named_after == '2999-01-01'

	mut mixed_msg := ''
	mut mixed_after := ''
	$for attr in OldMixed.attributes {
		if attr.name == 'deprecated' {
			mixed_msg = attr.arg
		}
		if attr.name == 'deprecated_after' {
			mixed_after = attr.arg
		}
	}
	assert mixed_msg == 'use NewMixed instead'
	assert mixed_after == '2999-01-02'
}

fn test_attribute_call_syntax_generic_named_args() {
	mut has_custom := false
	mut custom_flag := ''
	mut custom_count := ''
	$for attr in CustomNamed.attributes {
		if attr.name == 'custom' {
			has_custom = true
		}
		if attr.name == 'custom_flag' {
			custom_flag = attr.arg
		}
		if attr.name == 'custom_count' {
			custom_count = attr.arg
		}
	}
	assert has_custom
	assert custom_flag == 'true'
	assert custom_count == '2'
}
