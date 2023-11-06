enum CharacterGroup {
	chars
	alphanumerics
	numeric
	special
}

fn (self CharacterGroup) value() string {
	return match self {
		.chars { 'first' }
		.alphanumerics { 'second' }
		.numeric { 'third' }
		.special { 'fourth' }
	}
}

fn CharacterGroup.values() []CharacterGroup {
	mut res := []CharacterGroup{}
	$for item in CharacterGroup.values {
		res << CharacterGroup(item.value)
	}
	return res
}

fn test_main() {
	values := CharacterGroup.values()
	println('Char group values: ${values}')
	println('For loop over the values')
	for entry in CharacterGroup.values() {
		println('Value: ${entry}  ${entry.value()}')
	}

	assert values == [CharacterGroup.chars, CharacterGroup.alphanumerics, CharacterGroup.numeric,
		CharacterGroup.special]
}
