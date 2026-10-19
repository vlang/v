// An int literal first branch defaults to `int`, so a later `int(...)` cast
// branch has the same type.
enum Key {
	backspace
	tab
	space = 40
	other = 99
}

fn key_code(key Key) u32 {
	return u32(match key {
		.backspace { 8 }
		.tab { 9 }
		.space { 32 }
		else { int(key) }
	})
}

fn test_int_literal_branches_with_int_cast_else() {
	assert key_code(.backspace) == 8
	assert key_code(.tab) == 9
	assert key_code(.space) == 32
	assert key_code(.other) == 99
	key := Key.other
	code := match key {
		.backspace { 8 }
		else { int(key) }
	}
	assert typeof(code).name == 'int'
	assert code == 99
	narrow := match key {
		.backspace { 8 }
		else { i32(key) }
	}
	assert typeof(narrow).name == 'i32'
	assert narrow == 99
}
