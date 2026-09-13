module types

fn test_reading_or_writing_a_name_is_a_reference() {
	assert code_references_ident('println(x)', 'x', true)
	assert code_references_ident('x = 1', 'x', true)
	assert code_references_ident('x++', 'x', true)
	assert code_references_ident('y := x + 1', 'x', true)
	assert code_references_ident('for x < 3 {', 'x', true)
	assert code_references_ident(r'${x}', 'x', true)
}

fn test_a_member_or_a_field_label_is_not_a_reference() {
	assert !code_references_ident('cfg.x', 'x', true)
	assert !code_references_ident('a.b.x', 'x', true)
	assert !code_references_ident('match e { .x {} }', 'x', true)
	assert !code_references_ident('_ = Config{ x: 1 }', 'x', true)
	assert !code_references_ident('x: for {}', 'x', true)
}

fn test_a_new_binding_is_not_a_reference() {
	assert !code_references_ident('x := 1', 'x', true)
	assert !code_references_ident('mut x := 1', 'x', true)
	assert !code_references_ident('a, x := pair()', 'x', true)
	assert !code_references_ident('x, b := pair()', 'x', true)
	assert !code_references_ident('mut a, mut x := pair()', 'x', true)
	assert !code_references_ident('for x in list {', 'x', true)
	assert !code_references_ident('for i, x in list {', 'x', true)
	assert !code_references_ident('for mut x in list {', 'x', true)
}

fn test_a_longer_name_that_merely_contains_the_searched_one() {
	assert !code_references_ident('println(x_ray)', 'x', true)
	assert !code_references_ident('println(prefix)', 'fix', true)
	assert !code_references_ident('', 'x', true)
	assert !code_references_ident('println(x)', '', true)
}

fn test_a_number_does_not_turn_the_next_name_into_a_member() {
	assert code_references_ident('println(1.5 + x)', 'x', true)
	assert code_references_ident('for i in 0 .. x {', 'x', true)
}

fn test_a_pipe_lambda_parameter_shadows_the_searched_name() {
	assert !code_references_ident('cb := |x| x + 1', 'x', true)
	assert !code_references_ident('arr.map(|x| x * 2)', 'x', true)
	assert !code_references_ident('f(|mut x| x.len)', 'x', true)
	assert !code_references_ident('cb := |a, x| a + x', 'x', true)
	// The body of such a lambda is one expression, so it ends with its line.
	assert !code_references_ident('cb := |x| x + 1\nprintln(y)', 'x', true)
	assert code_references_ident('cb := |y| y + 1\nprintln(x)', 'x', true)
	// A lambda binding another name still reads the searched one.
	assert code_references_ident('arr.map(|i| i * x)', 'x', true)
	assert code_references_ident('f(|i| i, x)', 'x', true)
}

fn test_a_bitwise_or_does_not_open_a_lambda() {
	assert code_references_ident('a | x', 'x', true)
	assert code_references_ident('flags := a | x | b', 'x', true)
	assert code_references_ident('f(a || x)', 'x', true)
}

fn test_a_map_key_expression_is_a_reference() {
	assert code_references_ident('m := {x: 1}', 'x', true)
	assert code_references_ident('m := {\n\tx: 1\n}', 'x', true)
	assert code_references_ident('f({x: 1})', 'x', true)
	assert code_references_ident('m := {k: {x: 1}}', 'x', true)
	// A struct literal names its fields, and its own `{` follows the type.
	assert !code_references_ident('_ = Config{x: 1}', 'x', true)
	assert !code_references_ident('_ = []Config{}\n_ = Config{\n\tx: 1\n}', 'x', true)
	assert !code_references_ident('_ = {k: Config{x: 1}}', 'x', true)
	assert !code_references_ident('f(g(a), Config{x: 1})', 'x', true)
}

fn test_a_plain_assignment_writes_without_reading() {
	assert !code_references_ident('x = 1', 'x', false)
	assert !code_references_ident('println(1)\nx = 1', 'x', false)
	assert !code_references_ident('if c { x = 1 }', 'x', false)
	// Every other assignment, and every other position, still reads it.
	assert code_references_ident('x += 1', 'x', false)
	assert code_references_ident('x == 1', 'x', false)
	assert code_references_ident('x != 1', 'x', false)
	assert code_references_ident('y = x', 'x', false)
	assert code_references_ident('x.field = 1', 'x', false)
	assert code_references_ident('m[x] = 1', 'x', false)
	// fn_body_read_names skips the first child of an assignment alone.
	assert code_references_ident('a, x = pair()', 'x', false)
	// The unused parameter notice counts a write, the way fn_body_uses_ident does.
	assert code_references_ident('x = 1', 'x', true)
}

fn test_a_block_bodied_lambda_shadows_its_whole_body() {
	assert !code_references_ident('cb := |x| {\n\tprintln(x)\n\tx + 1\n}', 'x', true)
	assert !code_references_ident('cb := |x| (\n\tx + 1\n)', 'x', true)
	assert code_references_ident('cb := |y| {\n\ty + 1\n}\nprintln(x)', 'x', true)
}

fn test_a_generic_struct_literal_still_names_its_fields() {
	assert !code_references_ident('_ = Box[int]{x: 1}', 'x', true)
	assert !code_references_ident('_ = mymod.Box[int]{x: 1}', 'x', true)
	assert !code_references_ident('_ = []Box[int]{}\n_ = Box[int]{\n\tx: 1\n}', 'x', true)
	assert !code_references_ident('_ = Box[Pair[int]]{x: 1}', 'x', true)
	assert !code_references_ident('_ = map[string]Box[int]{}\n_ = Box[int]{x: 1}', 'x', true)
	// A bare brace still opens a map literal, whatever precedes the statement.
	assert code_references_ident('_ = arr[i]\nm := {x: 1}', 'x', true)
}

fn test_a_pipe_lambda_may_follow_a_colon_or_a_push() {
	assert !code_references_ident('s := S{cb: |x| x + 1}', 'x', true)
	assert !code_references_ident('m := {k: |x| x + 1}', 'x', true)
	assert !code_references_ident('arr << |x| x + 1', 'x', true)
	// A bitwise or in the same places is not a lambda.
	assert code_references_ident('s := S{flags: a | x}', 'x', true)
	assert code_references_ident('arr << a | x', 'x', true)
}

fn test_a_keyword_before_the_brace_does_not_make_a_struct_literal() {
	assert code_references_ident('return {x: 1}', 'x', true)
	assert code_references_ident('return {\n\tx: 1\n}', 'x', true)
	assert code_references_ident('_ = k in {x: 1}', 'x', true)
	// A real type name before it still labels a field.
	assert !code_references_ident('return Config{x: 1}', 'x', true)
	assert !code_references_ident('return Box[int]{x: 1}', 'x', true)
}

// code_references_ident runs on the output of code_text_in_range, so a literal
// has to go through the sanitizer to be tested.
fn scanned_code(code string) string {
	return code_text_in_range(code, 0, code.len)
}

fn test_a_string_prefix_is_not_a_name() {
	assert !code_references_ident(scanned_code("println(r'hello')"), 'r', true)
	assert !code_references_ident(scanned_code("println(c'hello')"), 'c', true)
	assert !code_references_ident(scanned_code("println(js'hello')"), 'js', true)
	assert !code_references_ident(scanned_code('println(r"hello")'), 'r', true)
	// A raw string has no interpolation, so its `${..}` stays plain text.
	assert !code_references_ident(scanned_code(r"println(r'${x}')"), 'x', true)
	assert code_references_ident(scanned_code(r"println('${x}')"), 'x', true)
	// Nor does it have escapes: the quote after the backslash still closes
	// the literal, so what follows is code again.
	assert code_references_ident(scanned_code(r"println(r'a\') + x"), 'x', true)
	// The name itself still reads outside of a literal.
	assert code_references_ident(scanned_code("println(r, r'hello')"), 'r', true)
}

fn test_only_the_first_target_of_a_multi_assignment_is_written() {
	assert !code_references_ident('x, y = pair()', 'x', false)
	assert !code_references_ident('x, b, c = triple()', 'x', false)
	assert code_references_ident('x, y = pair()', 'y', false)
	assert code_references_ident('a, x, c = triple()', 'x', false)
	// Only a bare name is the target; an index or a field is read.
	assert code_references_ident('m[x], y = pair()', 'x', false)
	assert code_references_ident('x.f, y = pair()', 'x', false)
	// The unused parameter notice counts every write as a use.
	assert code_references_ident('x, y = pair()', 'x', true)
}

fn test_a_nested_literal_inside_an_interpolation_is_sanitized_too() {
	assert !code_references_ident(scanned_code('println("\${lookup(\'x\')}")'), 'x', true)
	assert !code_references_ident(scanned_code('println(\'\${lookup("x")}\')'), 'x', true)
	assert !code_references_ident(scanned_code('println("\${f(/* x */ 1)}")'), 'x', true)
	// The body itself is still code, and a brace of a nested literal does not
	// close the interpolation.
	assert code_references_ident(scanned_code('println("\${lookup(x)}")'), 'x', true)
	assert code_references_ident(scanned_code('println(\'\${f("}")}\') + x'), 'x', true)
	assert code_references_ident(scanned_code('println(\'\${f("}")}\')'), 'f', true)
}

fn test_a_c_string_is_not_interpolated() {
	assert !code_references_ident(scanned_code(r"_ = c'${x}'"), 'x', true)
	assert !code_references_ident(scanned_code(r'_ = c"${x}"'), 'x', true)
	// It does have escapes, unlike a raw string, so the literal runs on.
	assert code_references_ident(scanned_code("_ = c'a\\'b' + x"), 'x', true)
	// An ordinary string still holds code in its interpolation.
	assert code_references_ident(scanned_code(r"_ = '${x}'"), 'x', true)
}

fn test_a_semicolon_ends_a_pipe_lambda_body() {
	assert code_references_ident('cb := |x| x + 1; println(x)', 'x', true)
	assert !code_references_ident('cb := |x| x + 1; _ = cb', 'x', true)
	assert !code_references_ident('cb := |x| x + 1', 'x', true)
}

fn test_a_short_struct_argument_names_a_field() {
	assert !code_references_ident('configure(x: 1)', 'x', true)
	assert !code_references_ident('_ = cfg.configure(x: 1)', 'x', true)
	assert !code_references_ident('_ = build[int](x: 1)', 'x', true)
	assert !code_references_ident('return configure(a: 1, x: 2)', 'x', true)
	assert !code_references_ident('configure(f(a), x: 1)', 'x', true)
	// A map literal passed as an argument keeps its key an expression.
	assert code_references_ident('configure({x: 1})', 'x', true)
	assert code_references_ident('configure(x, y: 1)', 'x', true)
}

fn test_a_comptime_condition_is_not_a_read() {
	assert !code_references_ident('\$if x ? {\n\tprintln(1)\n}', 'x', true)
	assert !code_references_ident('\$if windows {\n} \$else \$if x ? {\n}', 'x', true)
	assert !code_references_ident('\$if T is x {\n}', 'x', true)
	// The body of such a branch is ordinary code again.
	assert code_references_ident('\$if windows {\n\tprintln(x)\n}', 'x', true)
	assert code_references_ident('\$if windows {\n}\nprintln(x)', 'x', true)
}

fn test_a_lambda_body_may_start_on_the_next_line() {
	assert !code_references_ident('cb := |x|\n\tx + 1', 'x', true)
	assert !code_references_ident('cb := |x|\n\t{\n\t\tx + 1\n\t}', 'x', true)
	assert code_references_ident('cb := |y|\n\ty + 1\nprintln(x)', 'x', true)
}
