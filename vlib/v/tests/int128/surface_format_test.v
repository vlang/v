// Format specifiers and map printing for the 128-bit types. Before these, a
// specifier was accepted and then ignored, and a map printed `<map value>` for
// every 128-bit entry.
fn test_base_specifiers() {
	wide := (u128(1) << 100) + u128(255)
	assert '${wide:08x}' == '100000000000000000000000ff'
	assert '${wide:x}' == '100000000000000000000000ff'
	assert '${wide:X}' == '100000000000000000000000FF'
	assert '${wide:o}' == '2000000000000000000000000000000377'
	assert '${wide:08b}'.starts_with('1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001')
	assert u128(255).str_base(16) == 'ff'
	assert u128(255).str_base(8) == '377'
	assert u128(255).str_base(2) == '11111111'
	assert u128(0).str_base(16) == '0'
}

fn test_signed_and_char_specifiers() {
	minus_one := i128(-1)
	assert '${minus_one:x}' == 'ffffffffffffffffffffffffffffffff'
	wide := u128(65)
	assert '${wide:c}' == 'A'
	assert u128(255).char_str() == '\u00ff'
}

fn test_a_wide_value_without_a_specifier_is_unchanged() {
	wide := u128(1) << 100
	assert '${wide}' == '1267650600228229401496703205376'
	assert '${wide:u}' == '1267650600228229401496703205376'
}

fn test_map_values_print_their_whole_value() {
	m := {
		'a': u128(255)
		'b': u128(1) << 100
	}
	text := m.str()
	assert text.contains('255')
	assert text.contains('1267650600228229401496703205376')
	assert !text.contains('<map value>')
	assert '${m}'.contains('1267650600228229401496703205376')
}

fn test_signed_map_values_print_their_sign() {
	m := {
		'neg':   i128(-1)
		'small': i128(7)
	}
	text := m.str()
	assert text.contains('-1')
	assert text.contains('7')
}
