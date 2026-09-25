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

fn test_an_explicit_type_argument_is_checked_against_the_constraint() {
	res := check_program('explicit', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	println(longest[User](User{ name: 'a' }, User{ name: 'b' }).age)
	println(longest[int](1, 2))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["7:18: `int` doesn't implement field `name` of interface `Named`"], res.output
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

fn test_a_local_that_gets_its_value_from_a_t_is_a_t() {
	// Every way a local can take a value of type `T`: each `.age` below is on a `T`.
	res := check_program('derived', "fn derived[T Named](a T, b T, items []T, m map[string]T, c bool, n int) int {
	x := a
	mut total := x.age
	mut y := a
	y = b
	total += y.age
	z := if c { a } else { b }
	total += z.age
	for w in items {
		total += w.age
	}
	for i, w2 in items {
		total += i + w2.age
	}
	first := items[0]
	total += first.age
	p, q := a, b
	total += p.name.len + q.age
	r := (a)
	total += r.age
	s := &a
	total += s.age
	t := match n {
		1 { a }
		else { b }
	}
	total += t.age
	u := m['k'] or { a }
	total += u.age
	v := x
	total += v.age
	return total + x.greet().len
}

struct Box[T Named] {
	item T
}

fn from_field[T Named](b Box[T]) int {
	x := b.item
	return x.age
}

constraint Number = int | f64

fn from_set[T Number](x T) int {
	y := x
	return y.len
}

fn main() {}
")
	assert res.exit_code == 1, res.output
	no_age := 'type `T` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'3:17: ${no_age}',
		'6:13: ${no_age}',
		'8:13: ${no_age}',
		'10:14: ${no_age}',
		'13:19: ${no_age}',
		'16:17: ${no_age}',
		'18:26: ${no_age}',
		'20:13: ${no_age}',
		'22:13: ${no_age}',
		'27:13: ${no_age}',
		'29:13: ${no_age}',
		'31:13: ${no_age}',
		'32:19: type `T` has no method `greet`: its constraint `Named` does not declare it',
		'41:11: ${no_age}',
		'48:11: type `T` has no field named `len`: `int`, in its constraint `Number`, does not have it',
	], res.output
}

fn test_a_local_from_a_guard_a_call_or_an_array_method_is_a_t() {
	// `U`, not `T`: the type of a call must be the caller's parameter, not the
	// callee's `T`, which has the same name in `find` and `pair`.
	res := check_program('derived_calls', "fn find[T Named](items []T) ?T {
	return none
}

fn pair[T Named](a T) (T, int) {
	return a, 1
}

fn derived[U Named](a U, items []U, m map[string]U) int {
	mut total := 0
	if v := m['k'] {
		total += v.age
	}
	if w := find(items) {
		total += w.age
	}
	p, n := pair(a)
	total += n + p.age
	arr := [a]
	total += arr[0].age
	f := items.first()
	total += f.age
	for g in items.filter(it.name.len > 0) {
		total += g.age
	}
	for h in items.reverse() {
		total += h.age
	}
	return total
}

fn main() {}
")
	assert res.exit_code == 1, res.output
	no_age := 'type `U` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'12:14: ${no_age}',
		'15:14: ${no_age}',
		'18:17: ${no_age}',
		'20:18: ${no_age}',
		'22:13: ${no_age}',
		'24:14: ${no_age}',
		'27:14: ${no_age}',
	], res.output
}

fn test_a_generic_call_in_the_body_has_the_type_of_the_caller() {
	// `pick(n)` is a `U`: its own `T` is bound to the caller's `U`, and the
	// caller's `T` is another type parameter, with another constraint.
	res := check_program('call_type', 'fn pick[T Named](a T) T {
	return a
}

struct Box[T Named] {
	item T
}

fn (b Box[T]) get() T {
	return b.item
}

fn mixed[T Greeter, U Named](g T, n U, b Box[U]) int {
	x := pick(n)
	println(x.name)
	println(g.greet())
	y := b.get()
	println(y.name)
	return x.age + y.age + pick(n).age
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	no_age := 'type `U` has no field named `age`: its constraint `Named` does not declare it'
	assert error_lines(res.output) == [
		'19:11: ${no_age}',
		'19:19: ${no_age}',
		'19:33: ${no_age}',
	], res.output
}

fn test_a_local_of_type_t_can_use_what_the_constraint_declares() {
	res := check_program('derived_valid', "fn total[T Named](a T, items []T) int {
	x := a
	mut sum := x.name.len
	for y in items {
		sum += y.name.len
	}
	n := 5
	sum += n
	s := 'abc'
	sum += s.len
	first := items[0]
	return sum + first.name.len
}

fn main() {
	println(total(User{ name: 'a' }, [User{
		name: 'b'
	}]))
}
")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
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

fn test_a_generic_struct_type_written_in_a_declaration_is_checked() {
	res := check_program('declared', "struct Box[T Named] {
	item T
}

struct Shelf {
	good Box[User]
	bad  Box[int]
}

fn show(b Box[int]) string {
	return ''
}

fn keep(b Box[User]) Box[User] {
	return b
}

fn main() {}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		"7:7: `int` doesn't implement field `name` of interface `Named`",
		"10:11: `int` doesn't implement field `name` of interface `Named`",
	], res.output
}

fn test_an_inferred_generic_struct_init_is_checked() {
	res := check_program('inferred_init', "struct Box[T Named] {
	item T
}

fn main() {
	good := Box{
		item: User{ name: 'a' }
	}
	bad := Box{
		item: 1
	}
	println('\${good.item.name} \${bad}')
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["9:9: `int` doesn't implement field `name` of interface `Named`"], res.output
}

fn test_a_generic_function_value_is_checked() {
	res := check_program('fn_value', "fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn main() {
	good := longest[User]
	bad := longest[int]
	println(good(User{ name: 'a' }, User{ name: 'b' }).age)
	println(bad(1, 2))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ["7:17: `int` doesn't implement field `name` of interface `Named`"], res.output
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

// A constraint can also be a set of types, declared with `constraint`: the
// type argument must be one of them, and the body can use on a value of type
// `T` what every type of the set has.

fn test_a_call_with_a_type_of_the_constraint_set_is_valid() {
	res := check_program('set_valid', 'constraint Number = int | i64 | f64

fn double[T Number](x T) T {
	return x + x
}

fn describe[T Number](x T) string {
	return x.str()
}

fn main() {
	println(double(2))
	println(double(2.5))
	println(describe(i64(3)))
}
')
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_call_with_a_type_outside_the_constraint_set_is_reported_at_the_call() {
	res := check_program('set_call', "constraint Number = int | i64 | f64

fn double[T Number](x T) T {
	return x + x
}

fn main() {
	println(double('a'))
}
")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['8:17: cannot use `string` as `T`: it is not in its constraint `Number`'], res.output
}

fn test_the_body_can_use_only_what_every_type_of_the_set_has() {
	res := check_program('set_body', 'constraint Animal = User | Pet

fn label[T Animal](x T) string {
	return x.name + x.greet()
}

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == [
		'4:20: type `T` has no method `greet`: `Pet`, in its constraint `Animal`, does not have it',
	], res.output
}

fn test_a_constraint_set_of_unknown_types_is_reported() {
	res := check_program('set_unknown', 'constraint Bad = int | Foo

fn main() {}
')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['1:24: unknown type `Foo`'], res.output
}

fn test_constraint_is_still_a_name() {
	res := check_program('name', "struct Rule {
	constraint string
}

fn main() {
	constraint := Rule{
		constraint: 'x'
	}
	println(constraint.constraint)
}
")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

fn test_a_program_with_constraints_builds_and_runs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_constraints_run_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, constraint_prelude + "struct Box[T Named] {
	item T
}

fn (b Box[T]) label() string {
	return 'box of \${b.item.name}'
}

constraint Number = int | i64 | f64

fn longest[T Named](a T, b T) T {
	return if a.name.len >= b.name.len { a } else { b }
}

fn double[T Number](x T) T {
	return x + x
}

fn main() {
	u := longest(User{ name: 'Alex', age: 30 }, User{ name: 'Bo', age: 25 })
	println(u.age)
	println(double(21))
	println(double(1.25))
	println(Box[User]{ item: u }.label())
}
") or {
		panic(err)
	}
	res := os.execute('${os.quoted_path(@VEXE)} -new-compiler run ${os.quoted_path(path)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split_into_lines() == ['30', '42', '2.5', 'box of Alex'], res.output
}
