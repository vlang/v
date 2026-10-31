// A builtin array/map method called on a match-smartcast receiver returns the
// receiver's own collection type, not the untyped builtin `array`/`map`.
type KeyPath = []string | string

type Table = map[string]int | string

fn clone_key_path(path KeyPath) KeyPath {
	return match path {
		string { path }
		[]string { path.clone() }
	}
}

fn reversed_key_path(path KeyPath) KeyPath {
	return match path {
		string { path }
		[]string { KeyPath(path.reverse()) }
	}
}

fn table_keys(t Table) []string {
	return match t {
		map[string]int { t.keys() }
		string { [t] }
	}
}

fn test_match_smartcast_array_clone_returns_variant() {
	mut parts := ['a', 'b']
	copy := clone_key_path(KeyPath(parts))
	parts[0] = 'z'
	assert copy is []string
	if copy is []string {
		assert copy == ['a', 'b']
	}
	text := clone_key_path(KeyPath('s'))
	assert text is string
	reversed := reversed_key_path(KeyPath(['x', 'y']))
	if reversed is []string {
		assert reversed == ['y', 'x']
	} else {
		assert false
	}
	match copy {
		[]string {
			cloned := copy.clone()
			assert typeof(cloned).name == '[]string'
		}
		string {}
	}
}

fn test_match_smartcast_map_keys_returns_typed_array() {
	assert table_keys(Table({
		'k': 1
	})) == ['k']
	assert table_keys(Table('s')) == ['s']
}
