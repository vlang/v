module types

// The conditions of `$if`s as comptime_ways_constraints reads them (see
// vls_comptime_ways.v).

// cond_shape writes the parse of the condition `text`, each test in brackets.
fn cond_shape(text string) string {
	mut conds := ComptimeConds{}
	root := conds.parse_cond(text) or { return 'none' }
	return conds.shape(root)
}

fn (c &ComptimeConds) shape(id int) string {
	node := c.nodes[id]
	return match node.op {
		`!` { '!${c.shape(node.left)}' }
		`&` { '(${c.shape(node.left)} && ${c.shape(node.right)})' }
		`|` { '(${c.shape(node.left)} || ${c.shape(node.right)})' }
		else { '[${c.tests[node.test]}]' }
	}
}

fn test_a_condition_reads_as_written_and_as_the_parser_keeps_it() {
	assert cond_shape('a is f64 && b is f64 && c is f64') == '(([a is f64] && [b is f64]) && [c is f64])'
	assert cond_shape('(a is f64) && ((b is f64) && (c is f64))') == '([a is f64] && ([b is f64] && [c is f64]))'
	// The parser writes a blank inside the parentheses of a condition it keeps
	// as written, and none after `!`.
	assert cond_shape('( y is f64 || y is f32 ) && y !is f32') == '(([y is f64] || [y is f32]) && [y !is f32])'
	assert cond_shape('!( z is f64 )') == '![z is f64]'
	assert cond_shape('!(z is f64)') == '![z is f64]'
	assert cond_shape('!!(z is f64)') == '!![z is f64]'
}

fn test_and_binds_tighter_than_or() {
	assert cond_shape('a is f64 || b is f64 && c is f64') == '([a is f64] || ([b is f64] && [c is f64]))'
	assert cond_shape('(a is f64 || b is f64) && c is f64') == '(([a is f64] || [b is f64]) && [c is f64])'
}

fn test_a_test_keeps_its_list_its_calls_and_its_strings() {
	assert cond_shape('T in[f32, f64] || T !in [i8, u8]') == '([T in[f32, f64]] || [T !in [i8, u8]])'
	assert cond_shape('T in [Box[int], map[string]int] && T !is ?int') == '([T in [Box[int], map[string]int]] && [T !is ?int])'
	assert cond_shape("sizeof(B) == 8 && T.name == 'a && (b'") == "([sizeof(B) == 8] && [T.name == 'a && (b'])"
	assert cond_shape('T is \$float && linux') == '([T is \$float] && [linux])'
}

fn test_a_test_written_twice_is_one_test() {
	mut conds := ComptimeConds{}
	first := conds.parse_cond('(a is f64 && b is f64) || a is f64') or { panic('none') }
	second := conds.parse_cond('!(b is f64)') or { panic('none') }
	assert conds.tests == ['a is f64', 'b is f64']
	assert conds.shape(first) == '(([a is f64] && [b is f64]) || [a is f64])'
	assert conds.shape(second) == '![b is f64]'
}

fn test_a_condition_that_does_not_read_whole_is_none() {
	for text in ['', '   ', 'a is f64 &&', '&& a is f64', '(a is f64', 'a is f64)', '()', 'a in [f32',
		"a is f64 && b == 'x", '!', 'a is f64 || || b is f64'] {
		assert cond_shape(text) == 'none', text
	}
}

fn test_a_condition_holds_as_its_tests_do() {
	mut conds := ComptimeConds{}
	root := conds.parse_cond('!(a || b) && c') or { panic('none') }
	assert conds.tests == ['a', 'b', 'c']
	for bits in 0 .. 8 {
		values := [bits & 1 != 0, bits & 2 != 0, bits & 4 != 0]
		assert conds.holds(root, values) == (!(values[0] || values[1]) && values[2]), bits.str()
	}
}

fn test_the_negation_of_a_test_on_an_interface_is_its_positive_test() {
	assert comptime_positive_test('T !is User', 'T')? == 'T is User'
	assert comptime_positive_test('T !in[User, Admin]', 'T')? == 'T in [User, Admin]'
	assert comptime_positive_test('T !in [User]', 'T')? == 'T in [User]'
	for test in ['T is User', 'T in [User]', 'Tx !is User', 'T !isnt', 'x !is User'] {
		assert comptime_positive_test(test, 'T') == none, test
	}
}
