module method_form

const program = "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { longest(b, a) }
}

pub fn pick[T Named](xs []T) T {
	return xs[0]
}

fn (u User) longest() string {
	return 'longest(x) // not a call'
}

fn main() {
	f := longest[User] // a value
	println(longest(u, v).name + pick([u]).name + u.longest() + '\${longest(u, v).name}')
}
"

fn test_a_program_without_generic_functions_has_no_method_form() {
	assert of('fn main() {\n\tprintln(1)\n}\n') == none
	// A generic method is no generic function.
	assert of('fn (b Box[T]) get[T]() T {\n\treturn b.item\n}\n') == none
}

fn test_the_generic_functions_become_methods_of_host() {
	form := of(program) or { panic('no method form') }
	lines := form.source.split('\n')
	assert lines[0] == 'fn (host_ Host) longest[T Named](a T, b T) T {'
	// A call in the body of one, to itself.
	assert lines[1] == '\treturn if a.name.len >= b.name.len { a } else { host.longest(b, a) }'
	assert lines[4] == 'pub fn (host_ Host) pick[T Named](xs []T) T {'
	// A method of the same name, a string and a comment stay as they are.
	assert lines[8] == 'fn (u User) longest() string {'
	assert lines[9] == "\treturn 'longest(x) // not a call'"
	// A function value, the calls, and the code of an interpolation.
	assert lines[13] == '\tf := host.longest[User] // a value'
	assert lines[14] == "\tprintln(host.longest(u, v).name + host.pick([u]).name + u.longest() + '\${host.longest(u, v).name}')"
	// The program keeps its lines: `Host` comes after them.
	assert lines.len == program.split('\n').len + 4
	assert form.source.ends_with('\nstruct Host {}\n\nconst host = Host{}\n')
}

fn test_a_column_of_the_program_moves_by_what_the_method_form_adds_before_it() {
	form := of(program) or { panic('no method form') }
	// Line 1 gets `(host_ Host) ` before `longest`, at column 4.
	assert form.col(1, 3) == 3
	assert form.col(1, 4) == 4 + host_receiver.len
	assert form.col(1, 20) == 20 + host_receiver.len
	// Line 15 gets `host.` before each of its three calls, not before
	// `u.longest()`.
	line := program.split('\n')[14]
	first := line.index('longest(') or { 0 } + 1
	assert form.col(15, first) == first + host_value.len
	assert form.col(15, line.len) == line.len + 3 * host_value.len
	assert form.col(2, 5) == 5
}

fn test_the_method_form_reports_the_same_errors_where_it_moves_them() {
	form := of(program) or { panic('no method form') }
	col := program.split('\n')[14].index('longest(') or { 0 } + 1
	errors := ['15:${col}: could not infer the generic type', '2:5: `int` has no property `name`']
	// In another order, one at the name of the call, one at `host`.
	assert form.differences(errors, ['2:5: `int` has no property `name`',
		'15:${col + host_value.len}: could not infer the generic type']) == ''
	assert form.differences(errors, ['2:5: `int` has no property `name`',
		'15:${col}: could not infer the generic type']) == ''
	// A message that names the method through `Host`.
	assert form.differences(['2:5: in call to `longest`'], ['2:5: in call to `Host.longest`']) == ''
}

fn test_the_method_form_differs_when_it_reports_otherwise() {
	form := of(program) or { panic('no method form') }
	errors := ['2:5: `int` has no property `name`']
	assert form.differences(errors, []string{}).contains('reports 1 errors, its method form 0')
	assert form.differences(errors, ['2:5: `int` has no property `nme`']) != ''
	assert form.differences(errors, ['3:5: `int` has no property `name`']) != ''
	assert form.differences(errors, ['2:6: `int` has no property `name`']) != ''
}
