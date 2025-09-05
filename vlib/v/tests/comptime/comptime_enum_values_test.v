enum CharacterGroup {
	chars
	alphanumerics
	numeric
	special
}

type AnotherCharGroup = CharacterGroup

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

fn test_alias_enum() {
	mut values := []EnumData{}
	$for entry in AnotherCharGroup.values {
		values << entry
	}
	assert values[0].value == int(CharacterGroup.chars)
	assert values[0].name == CharacterGroup.chars.str()

	assert values[1].value == int(CharacterGroup.alphanumerics)
	assert values[1].name == CharacterGroup.alphanumerics.str()

	assert values[2].value == int(CharacterGroup.numeric)
	assert values[2].name == CharacterGroup.numeric.str()

	assert values[3].value == int(CharacterGroup.special)
	assert values[3].name == CharacterGroup.special.str()
}
