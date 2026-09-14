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

fn test_a_typed_map_literal_holds_key_expressions() {
	assert code_references_ident('m := map[string]int{x: 1}', 'x', true)
	assert code_references_ident('return map[string]int{x: 1}', 'x', true)
	assert code_references_ident('m := map[string]Box[int]{x: 1}', 'x', true)
	// An array initialisation names its options, and still reads their values.
	assert !code_references_ident('_ = []int{len: 3, cap: x}', 'len', true)
	assert code_references_ident('_ = []int{len: 3, cap: x}', 'x', true)
	assert !code_references_ident('_ = Config{x: 1}', 'x', true)
}

fn test_a_compile_time_name_is_not_a_variable() {
	assert !code_references_ident('_ = \$env(h)', 'env', true)
	assert !code_references_ident('_ = \$embed_file(p)', 'embed_file', true)
	assert !code_references_ident('_ = \$d(n, 1)', 'd', true)
	assert !code_references_ident('println(@FILE)', 'FILE', true)
	// Their arguments are ordinary code, and an interpolation keeps no `$`.
	assert code_references_ident('_ = \$d(n, x)', 'x', true)
	assert code_references_ident(scanned_code(r"_ = '${env}'"), 'env', true)
}

fn test_a_function_valued_map_type_is_still_a_map() {
	assert code_references_ident('m := map[string]fn (int) int{x: cb}', 'x', true)
	assert code_references_ident('m := map[string]fn (int) (int, int){x: cb}', 'x', true)
	assert code_references_ident('m := map[string][]int{x: v}', 'x', true)
	assert code_references_ident('return map[string]fn (int) int{x: cb}', 'x', true)
	// An array of maps initialises an array, and names its options.
	assert !code_references_ident('_ = []map[string]int{len: 3}', 'len', true)
	assert !code_references_ident('_ = []int{len: 3}', 'len', true)
	// A call before the literal does not extend its type.
	assert !code_references_ident('_ = f(a)\n_ = Config{x: 1}', 'x', true)
}

fn test_a_lambda_body_may_continue_on_the_next_line() {
	assert !code_references_ident('cb := |x| 1 +\n\tx', 'x', true)
	assert !code_references_ident('cb := |x| 1 +\n\tx * 2', 'x', true)
	assert code_references_ident('cb := |y| 1 +\n\ty\nprintln(x)', 'x', true)
	// A line that can end an expression ends the body.
	assert code_references_ident('cb := |y| y\nprintln(x)', 'x', true)
	assert code_references_ident('cb := |y| f(y)?\nprintln(x)', 'x', true)
}

fn test_assembly_instructions_are_not_v_code() {
	assert !code_references_ident('asm arm64 {\n\tmov x0, x1\n}', 'x0', true)
	assert !code_references_ident('asm amd64 {\n\txor rax, rax\n}', 'rax', true)
	assert !code_references_ident('asm amd64 raw {\n\tmov eax, x0\n\t; r (b) as b\n}', 'x0', true)
	// The expression of an output, input or clobber clause is V code.
	assert code_references_ident('asm amd64 {\n\tmov eax, a\n\t; =r (a) as a\n}', 'a', true)
	assert code_references_ident('asm amd64 {\n\tmov eax, x0\n\t; r (x0) as x0\n}', 'x0', true)
	// And so is everything around the block.
	assert code_references_ident('asm arm64 {\n\tmov x0, x1\n}\nprintln(x0)', 'x0', true)
	assert code_references_ident('println(x0)\nasm arm64 {\n\tmov x0, x1\n}', 'x0', true)
}

fn test_a_literal_lambda_body_still_leaves_a_token() {
	// The body disappears from the sanitized code, so it needs a placeholder;
	// without one the range would run on to the statement below.
	assert code_references_ident(scanned_code("cb := |y| 'constant'\nprintln(x)"), 'x', true)
	assert !code_references_ident(scanned_code("cb := |x| 'constant'\nprintln(y)"), 'x', true)
	assert code_references_ident(scanned_code("cb := |x| 'constant'\nprintln(x)"), 'x', true)
	// An interpolated body keeps its own code, and still ends with its line.
	assert code_references_ident(scanned_code(r"cb := |y| '${a}'\nprintln(x)"), 'x', true)
	assert code_references_ident(scanned_code(r"cb := |y| '${x}'"), 'x', true)
}

fn test_a_keyword_does_not_end_a_lambda_body() {
	assert !code_references_ident('cb := |x| unsafe\n{ x }', 'x', true)
	assert !code_references_ident('cb := |x| lock\n{ x }', 'x', true)
	assert code_references_ident('cb := |y| unsafe\n{ y }\nprintln(x)', 'x', true)
	// The literals are the keywords an expression may stop at.
	assert code_references_ident('cb := |y| true\nprintln(x)', 'x', true)
	assert code_references_ident('cb := |y| none\nprintln(x)', 'x', true)
	assert code_references_ident('cb := |y| y\nprintln(x)', 'x', true)
}

fn test_a_channel_valued_map_type_is_still_a_map() {
	assert code_references_ident('m := map[string]chan int{x: ch}', 'x', true)
	assert code_references_ident('m := map[string]thread int{x: t}', 'x', true)
	assert code_references_ident('m := map[string]map[string]int{x: inner}', 'x', true)
}

fn test_a_label_is_not_a_variable() {
	assert !code_references_ident('unsafe {\n\tgoto x\n}\nx:\nprintln(1)', 'x', true)
	assert !code_references_ident('goto x', 'x', true)
	assert !code_references_ident('break x', 'x', true)
	assert !code_references_ident('continue x', 'x', true)
	assert !code_references_ident('unsafe {\n\tx: println(1)\n}', 'x', true)
	assert !code_references_ident('x: for {\n\tbreak x\n}', 'x', true)
	// A statement of its own on the next line is not the label of a `break`.
	assert code_references_ident('for {\n\tbreak\n}\nprintln(x)', 'x', true)
	// And a map entry is still a key, inside a block as much as outside one.
	assert code_references_ident('unsafe {\n\tm := {x: 1}\n}', 'x', true)
	assert code_references_ident('return {x: 1}', 'x', true)
}

fn test_a_keyword_field_ends_a_lambda_body() {
	assert code_references_ident('cb := |x| cfg.type\nprintln(x)', 'x', true)
	assert code_references_ident('cb := |y| cfg.match\nprintln(x)', 'x', true)
	// Without the selector it is the keyword it looks like.
	assert !code_references_ident('cb := |x| unsafe\n{ x }', 'x', true)
}

fn test_a_short_struct_argument_of_a_dynamic_callee() {
	assert !code_references_ident('_ = make_handler()(x: 1)', 'x', true)
	assert !code_references_ident('_ = (handler)(x: 1)', 'x', true)
	assert !code_references_ident('_ = handlers[i]()(x: 1)', 'x', true)
	assert !code_references_ident('_ = f(a)(b)(x: 1)', 'x', true)
	// A map literal argument still keys, and the other arguments still read.
	assert code_references_ident('_ = make_handler()({x: 1})', 'x', true)
	assert code_references_ident('_ = (handler)(x, y: 1)', 'x', true)
}

fn test_an_interpolation_format_is_not_a_name() {
	assert !code_references_ident(scanned_code(r"println('${value:x}')"), 'x', true)
	assert !code_references_ident(scanned_code(r"println('${value:-10}')"), 'x', true)
	assert !code_references_ident(scanned_code(r"println('${value:.3f}')"), 'f', true)
	// The expression before it is still code, nested colons included.
	assert code_references_ident(scanned_code(r"println('${x:04}')"), 'x', true)
	assert code_references_ident(scanned_code(r"println('${Config{x: 1}.y:04}')"), 'Config', true)
	assert !code_references_ident(scanned_code(r"println('${Config{x: 1}.y:04}')"), 'x', true)
}

fn test_a_label_of_a_nested_block() {
	assert !code_references_ident('if true {\n\tunsafe {\n\t\tgoto x\n\t}\n\tx:\n\tprintln(1)\n}', 'x', true)
	assert !code_references_ident('if f() {\n\tx: println(1)\n}', 'x', true)
	assert !code_references_ident('for i < n {\n\tx: println(1)\n}', 'x', true)
	// A map literal in any of those still keys with an expression.
	assert code_references_ident('if true {\n\tm := {x: 1}\n}', 'x', true)
	assert code_references_ident('if true {\n\treturn {x: 1}\n}', 'x', true)
	assert code_references_ident('f({x: 1})', 'x', true)
	assert code_references_ident('_ = [{x: 1}]', 'x', true)
}

fn test_a_literal_ended_condition_opens_a_block() {
	assert !code_references_ident('if n == 1 {\n\tx: println(1)\n}', 'x', true)
	assert !code_references_ident(scanned_code("if s == 'a' {\n\tx: println(1)\n}"), 'x', true)
	assert !code_references_ident('if n == 1 {\n\tunsafe {\n\t\tgoto x\n\t}\n\tx:\n}', 'x', true)
	// A map literal is still one wherever an expression may start.
	assert code_references_ident('if n == 1 {\n\tm := {x: 1}\n}', 'x', true)
	assert code_references_ident('_ = f(1, {x: 1})', 'x', true)
}

fn test_a_line_comment_keeps_its_newline() {
	// Without the newline the `)` and the `x` would share a line, and the
	// assignment below would read as one.
	assert !code_references_ident(scanned_code('println(1) // comment\nx = 1'), 'x', false)
	assert !code_references_ident(scanned_code('println(1) /* comment */\nx = 1'), 'x', false)
	assert code_references_ident(scanned_code('println(1) // comment\ny = x'), 'x', false)
}

fn test_a_labeled_assignment_still_writes() {
	assert !code_references_ident('retry: x = 1', 'x', false)
	assert !code_references_ident('retry: x, y = pair()', 'x', false)
	// The other positions of the statement are unchanged.
	assert code_references_ident('retry: y = x', 'x', false)
	assert code_references_ident('retry: x += 1', 'x', false)
	assert code_references_ident('retry: x, y = pair()', 'y', false)
	// And a write is a use of a parameter, label or not.
	assert code_references_ident('retry: x = 1', 'x', true)
}

fn test_an_addressed_map_literal_is_still_a_map() {
	assert code_references_ident('m := &map[string]int{x: 1}', 'x', true)
	assert code_references_ident('return &map[string]int{x: 1}', 'x', true)
	// A pointer inside the type is part of it, not a prefix of the literal.
	assert code_references_ident('m := map[string]&Config{x: 1}', 'x', true)
	assert code_references_ident('m := &map[string]&Config{x: 1}', 'x', true)
	// An addressed struct literal still names its fields.
	assert !code_references_ident('c := &Config{x: 1}', 'x', true)
	assert !code_references_ident('c := &Box[int]{x: 1}', 'x', true)
	// And an array of maps still initialises an array.
	assert !code_references_ident('_ = []map[string]int{len: 3}', 'len', true)
}

fn test_a_lambda_body_may_continue_with_a_selector() {
	assert !code_references_ident('cb := |x| make()\n\t.consume(x)', 'x', true)
	assert !code_references_ident('cb := |x| make()\n\t.a()\n\t.b(x)', 'x', true)
	assert !code_references_ident('cb := |x| make\n\t(x)', 'x', true)
	// A statement of its own on the next line still ends the body.
	assert code_references_ident('cb := |y| make()\n\t.consume(y)\nprintln(x)', 'x', true)
	assert code_references_ident('cb := |y| y\nprintln(x)', 'x', true)
}

fn test_a_propagated_condition_opens_a_block() {
	assert !code_references_ident('if get_bool()! {\n\tx: println(1)\n}', 'x', true)
	assert !code_references_ident('if get_bool()? {\n\tx: println(1)\n}', 'x', true)
	// A map literal is still one after an operator.
	assert code_references_ident('m := {x: 1}', 'x', true)
	assert code_references_ident('if get_bool()! {\n\tm := {x: 1}\n}', 'x', true)
}

fn test_a_type_marker_is_not_a_variable() {
	assert !code_references_ident('c := chan int{}', 'chan', true)
	assert !code_references_ident('c := chan []int{}', 'chan', true)
	assert !code_references_ident('ts := []thread{}', 'thread', true)
	assert !code_references_ident('ts := []thread int{}', 'thread', true)
	// An ambiguous follower reads the variable of that name instead, which is
	// the safe way round: `chan & 1` and `chan[0]` do read it, and a
	// `chan &Config{}` that does not only costs a notice.
	assert code_references_ident('_ = chan & 1', 'chan', true)
	assert code_references_ident('_ = chan[0]', 'chan', true)
	assert code_references_ident('println(chan)', 'chan', true)
	assert code_references_ident('ts << thread', 'thread', true)
	assert code_references_ident('_ = chan.cap', 'chan', true)
}

fn test_a_type_marker_needs_its_context() {
	// `{` also follows a condition, so the token before the marker decides.
	assert code_references_ident('if chan {\n\tprintln(1)\n}', 'chan', true)
	assert code_references_ident('match chan {\n\t1 {}\n}', 'chan', true)
	assert code_references_ident('for chan {\n\tbreak\n}', 'chan', true)
	assert code_references_ident('if !chan {\n\tprintln(1)\n}', 'chan', true)
	assert !code_references_ident('ts := []thread{}', 'thread', true)
	assert !code_references_ident('_ = f([]thread{})', 'thread', true)
	assert !code_references_ident('return []thread{}', 'thread', true)
}

fn test_an_operator_keyword_after_a_marker_reads_it() {
	assert code_references_ident('if chan in allowed {\n\tprintln(1)\n}', 'chan', true)
	assert code_references_ident('if chan is Config {\n\tprintln(1)\n}', 'chan', true)
	assert code_references_ident('_ = chan or { 0 }', 'chan', true)
	assert code_references_ident('_ = thread as int', 'thread', true)
	// A type name after it still names a type.
	assert !code_references_ident('c := chan int{}', 'chan', true)
	assert !code_references_ident('c := chan map[string]int{}', 'chan', true)
	assert !code_references_ident('c := chan chan int{}', 'chan', true)
}

fn test_an_anonymous_struct_value_type_is_traversed() {
	assert code_references_ident('m := map[string]struct { n int }{x: 1}', 'x', true)
	assert code_references_ident('m := map[string]struct { n int, o int }{x: 1}', 'x', true)
	assert code_references_ident('return map[string]struct { n int }{x: 1}', 'x', true)
	// A struct literal is still one, and a block still labels its statements.
	assert !code_references_ident('c := Config{x: 1}', 'x', true)
	assert !code_references_ident('if cond {\n\tx: println(1)\n}', 'x', true)
	assert !code_references_ident('unsafe {\n\tx: println(1)\n}', 'x', true)
	assert !code_references_ident('if a {\n} else {\n\tx: println(1)\n}', 'x', true)
	// An array of those still initialises an array.
	assert !code_references_ident('_ = []struct { n int }{len: 3}', 'len', true)
}

fn test_a_sibling_branch_binds_its_own() {
	branches := [ComptimeBranchRange{10, 20}, ComptimeBranchRange{30, 40}]
	// Declared in the first branch, so the second cannot read it.
	assert declaration_is_in_a_sibling_branch(branches, 12, branches[1])
	assert declaration_is_in_a_sibling_branch(branches, 35, branches[0])
	// Its own branch reads it, and so does one that encloses the declaration.
	assert !declaration_is_in_a_sibling_branch(branches, 12, branches[0])
	assert !declaration_is_in_a_sibling_branch([ComptimeBranchRange{0, 50},
		ComptimeBranchRange{10, 20}], 12, ComptimeBranchRange{0, 50})
	// A parameter, and a variable that no branch declares, are read by any.
	assert !declaration_is_in_a_sibling_branch(branches, -1, branches[0])
	assert !declaration_is_in_a_sibling_branch(branches, 25, branches[0])
}

fn test_a_chained_index_still_calls() {
	assert !code_references_ident('_ = handlers[i][j](x: 1)', 'x', true)
	assert !code_references_ident('_ = handlers[i][j][k](x: 1)', 'x', true)
	assert !code_references_ident('_ = hs[0]()(x: 1)', 'x', true)
	// The indices themselves are still read.
	assert code_references_ident('_ = handlers[x][j](y: 1)', 'x', true)
	assert code_references_ident('_ = handlers[i][x](y: 1)', 'x', true)
}

fn test_a_short_struct_argument_of_a_block_ended_callee() {
	assert !code_references_ident('fn (_ Config) {}(x: 1)', 'x', true)
	assert !code_references_ident('fn (c Config) {\nprintln(c)\n}(x: 1)', 'x', true)
	// The other arguments of such a call still read.
	assert code_references_ident('fn (_ Config) {}(x, y: 1)', 'x', true)
	// The `}` of an unrelated block does not turn what follows into a call,
	// and a map literal after one still keys with an expression.
	assert code_references_ident('if ok {\n}\nm := {\nx: 1\n}', 'x', true)
	assert code_references_ident('if ok {\n}\n_ = (x + 1).str()', 'x', true)
}

fn test_one_branch_scan_answers_every_name() {
	// The tokens of a branch are shared by the names checked against it, so
	// the tokenized form has to answer exactly what the text one does.
	code := scanned_code('cfg.x = 1\nprintln(y)\nfn (_ Config) {}(z: 1)')
	tokens, lines := code_tokens(code)
	for name in ['x', 'y', 'z'] {
		for writes in [true, false] {
			from_tokens := tokens_reference_ident(tokens, lines, name, writes)
			assert from_tokens == code_references_ident(code, name, writes)
		}
	}
	assert tokens_reference_ident(tokens, lines, 'y', true)
	assert !tokens_reference_ident(tokens, lines, 'z', true)
	assert !tokens_reference_ident(tokens, lines, '', true)
}
