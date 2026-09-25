module main

import os

// A type parameter can name an interface it must satisfy, `fn f[T Named]`: a
// call is checked against the interface, where V checks a value passed to an
// interface parameter, and the body can use on a `T` only what the interface
// declares. A type parameter without a constraint works as before.

const constraint_prelude = 'module main

interface Named {
	name string
}

interface Greeter {
	greet() string
}

interface NamedGreeter {
	Named
	Greeter
}

struct User {
	name string
	age  int
}

fn (u User) greet() string {
	return u.name
}

struct Pet {
	name string
}
'

// The prelude above is 27 lines: a program's own lines start at 28.
const first_line = 28

fn check_program(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, constraint_prelude + source) or { panic(err) }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
}

// error_lines returns `line:col: message` for every error of a check output,
// with the line counted from the program's own first line.
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
		program_line := location[location.len - 2].int() - first_line + 1
		lines << '${program_line}:${location[location.len - 1]}: ${line.all_after(': error: ')}'
	}
	return lines
}

fn test_a_constrained_call_with_types_that_implement_the_interface_is_valid() {
	res := check_program('valid', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	u := longest(User{ name: 'Alex', age: 30 }, User{ name: 'Bob', age: 25 })
	println(u.age)
	println(longest(Pet{ name: 'Rex' }, Pet{ name: 'Tom' }).name)
}
")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_call_with_a_type_that_does_not_implement_the_constraint_is_reported_at_the_call() {
	res := check_program('int_call', 'fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	println(longest(1, 2))
}
')
	assert res.exit_code == 1, res.output
	// Once, at the call: not again in the body of the instance for `int`.
	assert error_lines(res.output) == ["6:18: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_missing_method_of_the_constraint_is_reported_at_the_call() {
	res := check_program('pet_call', "fn welcome[T NamedGreeter](x T) string {
	return x.greet() + x.name
}

fn main() {
	println(welcome(User{ name: 'a' }))
	println(welcome(Pet{ name: 'b' }))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["7:18: `Pet` doesn't implement method `greet` of interface `NamedGreeter`"], res.output
}

fn test_the_body_can_use_only_what_the_constraint_declares() {
	// Reported even though nothing calls `oldest`.
	res := check_program('body', 'fn oldest[T Named](a T, b T) T {
	if a.age > b.age {
		return a
	}
	println(a.nme)
	return b
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'2:7: type `T` has no field named `age`: its constraint `Named` does not declare it',
		'2:15: type `T` has no field named `age`: its constraint `Named` does not declare it',
		'5:12: type `T` has no field named `nme`: its constraint `Named` does not declare it',
	], res.output
}

fn test_a_method_the_constraint_does_not_declare_is_reported_in_the_body() {
	res := check_program('body_method', 'fn label[T Named](x T) string {
	return x.greet()
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'2:11: type `T` has no method `greet`: its constraint `Named` does not declare it',
	], res.output
}

fn test_a_generic_struct_checks_its_type_arguments() {
	res := check_program('struct', "struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return b.item.name
}

fn main() {
	println(Box[User]{ item: User{ name: 'a' } }.label())
	println(Box[int]{ item: 1 }.label())
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["11:10: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_constraint_must_be_an_interface() {
	res := check_program('not_interface', 'fn twice[T User](x T) T {
	return x
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'1:12: the constraint of `T` must be an interface or a constraint, not `User`',
	], res.output
}

fn test_a_constraint_is_a_type_name_alone() {
	// V3 used to skip what follows the name of a type parameter: this compiled.
	res := check_program('not_a_type', 'fn id[T Foo + 42](x T) T {
	return x
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['1:13: unexpected token `+`, expecting `,` or `]`'], res.output
}

fn test_an_unconstrained_generic_works_as_before() {
	res := check_program('unconstrained', "fn show[T](x T) string {
	return x.name
}

fn main() {
	println(show(User{ name: 'a' }))
	println(show(1))
}
")
	assert res.exit_code == 1, res.output
	// The error of the instance for `int`, in the body, as without constraints.
	assert error_lines(res.output) == ['2:11: `int` has no property `name`'], res.output
}

fn test_a_constraint_from_another_module_is_checked_at_the_call() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_modules_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'shapes')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'shapes', 'shapes.v'), 'module shapes

pub interface Named {
	name string
}

pub fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'main.v'), "module main

import shapes

struct City {
	name string
}

fn main() {
	println(shapes.longest(City{ name: 'Lima' }, City{ name: 'Oslo' }).name)
	println(shapes.longest(1, 2))
}
") or {
		panic(err)
	}
	// Several parser workers, as a machine with more cores runs it.
	res := os.execute('cd ${os.quoted_path(dir)} && VJOBS=4 ${os.quoted_path(@VEXE)} -new-compiler -check -nocolor .')
	assert res.exit_code == 1, res.output
	errors := res.output.split_into_lines().filter(it.contains(': error: '))
	assert errors == ["main.v:11:25: error: `int` doesn't implement field `name` of interface `Named`"], res.output
}
