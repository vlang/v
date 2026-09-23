module main

import os

// A generic function is checked with its type parameters left open, where
// `x.name` cannot be judged. A check also has to check each concrete instance,
// as V1 did: `show(1)` cannot read `x.name`, and it has to fail there instead
// of in the C compiler.

const generic_prelude = 'module main

struct User {
	name string
}

fn (u User) greet() string {
	return u.name
}
'

fn check_program(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_generic_instances_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude + source) or { panic(err) }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
}

// error_lines returns `line:col: message` for every error of a check output.
fn error_lines(output string) []string {
	mut lines := []string{}
	for line in output.split_into_lines() {
		if !line.contains(': error: ') {
			continue
		}
		location := line.all_before(': error: ').split(':')
		if location.len < 3 {
			continue
		}
		lines << '${location[location.len - 2]}:${location[location.len - 1]}: ${line.all_after(': error: ')}'
	}
	return lines
}

fn test_an_instance_that_reads_a_field_its_type_lacks_is_reported() {
	res := check_program('int_field', 'fn show[T](x T) string {\n\treturn x.name\n}\n\nfn main() {\n\tprintln(show(1))\n}\n')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['11:11: `int` has no property `name`'], res.output
}

fn test_a_misspelled_field_is_reported_for_a_struct_instance() {
	res := check_program('struct_typo', "fn show[T](x T) string {\n\treturn x.nme\n}\n\nfn main() {\n\tprintln(show(User{ name: 'a' }))\n}\n")
	assert res.exit_code == 1, res.output
	errors := error_lines(res.output)
	assert errors.len == 1, res.output
	assert errors[0].starts_with('11:11: type `User` has no field named `nme`'), res.output
}

fn test_an_instance_that_calls_a_method_its_type_lacks_is_reported() {
	res := check_program('int_method', "fn call[T](x T) string {\n\treturn x.greet()\n}\n\nfn main() {\n\tprintln(call(User{ name: 'a' }))\n\tprintln(call(1))\n}\n")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['11:11: unknown method or field: `int.greet`.'], res.output
}

fn test_a_method_of_a_generic_struct_is_checked_for_each_instance() {
	res := check_program('generic_method', "struct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) label() string {\n\treturn b.v.name\n}\n\nfn main() {\n\tprintln(Box[User]{ v: User{ name: 'a' } }.label())\n\tprintln(Box[int]{ v: 1 }.label())\n}\n")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['15:13: `int` has no property `name`'], res.output
}

fn test_instances_that_have_what_the_body_uses_stay_valid() {
	res := check_program('valid', "fn show[T](x T) string {\n\treturn x.name\n}\n\nfn call[T](x T) string {\n\treturn x.greet()\n}\n\nfn main() {\n\tprintln(show(User{ name: 'a' }))\n\tprintln(call(User{ name: 'b' }))\n}\n")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}
