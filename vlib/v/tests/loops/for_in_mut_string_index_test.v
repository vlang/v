type IndexedName = string

type IndexedValue = int | string

fn first_byte_of_value(mut value IndexedValue) u8 {
	if mut value is string {
		first := value[0]
		return first
	}
	return 0
}

// A `mut` binding refers to the string it names, so indexing it reads a byte of
// that string (and slicing it yields a string), just as with an immutable one.
fn test_index_of_a_mut_string_binding_is_a_byte() {
	mut words := ['abc', 'xyz']
	mut out := []string{}
	for mut word in words {
		first := word[0]
		assert typeof(first).name == 'u8'
		out << first.ascii_str()
		out << word[1].ascii_str()
		out << word[0..2]
		out << word[1..]
		if word[2] == `c` {
			out << '!'
		}
		word = word.to_upper()
	}
	assert out == ['a', 'b', 'ab', 'bc', '!', 'x', 'y', 'xy', 'yz']
	assert words == ['ABC', 'XYZ']
}

fn test_index_of_a_mut_string_alias_binding_is_a_byte() {
	mut names := [IndexedName('qwe')]
	for mut name in names {
		second := name[1]
		assert typeof(second).name == 'u8'
		assert second.ascii_str() == 'w'
	}
}

fn test_index_of_a_mut_smartcast_string_is_a_byte() {
	mut value := IndexedValue('hello')
	assert first_byte_of_value(mut value) == `h`
}

fn test_index_of_other_mut_bindings_is_unchanged() {
	mut rows := [[1, 2], [3, 4]]
	mut sum := 0
	for mut row in rows {
		second := row[1]
		sum += second
	}
	assert sum == 6
	mut maps := [{
		'k': 5
	}]
	for mut m in maps {
		value := m['k']
		assert value == 5
	}
}
