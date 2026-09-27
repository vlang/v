// vtest build: !windows
module main

import os

// A check checks the body of a generic function whose type parameters all have
// a constraint as the body of any other function, with each type parameter as
// a type that its constraint admits. The body of one with a type parameter
// without a constraint is left to its instances, as before.

const work_dir = os.join_path(os.vtmp_dir(), 'generic_body_check_${os.getpid()}')

fn testsuite_begin() {
	os.mkdir_all(work_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(work_dir) or {}
}

// check checks `source` as the main.v of a directory of its own, as an editor
// does, and returns the lines of its errors and warnings.
fn check(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(@VEXE)} -new-compiler -check -nocolor .')
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& (it.contains(': error: ') || it.contains(': warning: ')))
}

// build builds `source` as the main.v of a directory of its own and returns the
// lines of its errors.
fn build(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := os.execute('cd ${os.quoted_path(dir)} && ${os.quoted_path(@VEXE)} -new-compiler -nocolor -o prog .')
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& it.contains(': error: '))
}

fn test_a_build_checks_a_generic_body_whose_type_parameters_all_have_constraints() {
	// As it checks the body of any other function: the errors are V's, not
	// those of the C compiler, and `e = x` is one although `main` only asks for
	// the instance with `int`.
	errors := build('build', 'module main

type Number = int | f64

fn assign[T Number](x T) int {
	mut e := 0
	e = x
	return e
}

fn typo[T Number](x T) T {
	asd
	return x
}

fn plain[T](x T) T {
	return x
}

fn main() {
	println(assign(1))
	println(typo(2))
	println(plain(3))
}
')
	assert errors.len == 2, errors.str()
	assert errors[0].starts_with('main.v:7:6: error: cannot assign to `e`: expected `int`, not `f64`: when `T` is `f64`, in its constraint `Number`'), errors[0]
	assert errors[1].starts_with('main.v:12:2: error: unexpected name `asd`'), errors[1]
}

fn test_a_generic_body_whose_type_parameters_all_have_constraints_is_checked() {
	errors := check('independent', "module main

type Numeric = int | f64

interface Named {
	name string
}

struct User {
	name string
}

fn test[T Numeric](value T) string {
	\$if T is f64 {
		return 'float'
	} \$else {
		return 'number'
	}
	asd
}

fn named[T Named](x T) string {
	sum := 1 + 'a'
	println(sum)
	return x.name
}

fn wrong_return[T Named](x T) string {
	println(x.name)
	return 5
}

fn main() {
	println(test(1))
	println(named(User{'a'}))
	println(wrong_return(User{'b'}))
}
")
	assert errors.len == 3, errors.str()
	assert errors[0].starts_with('main.v:19:2: error: unexpected name `asd`'), errors[0]
	assert errors[1].starts_with('main.v:23:9: error: operator `+` cannot concatenate `int` and `string`'), errors[1]
	assert errors[2].starts_with('main.v:30:9: error: cannot use `int literal` as type `string` in return argument'), errors[2]
}

fn test_a_compile_time_in_decides_the_branch_that_a_type_checks() {
	// `$if x in [int]` and `$if T !in [f64]` choose code by the type, as `is`
	// does: each check of the body takes the branch that its type takes.
	errors := check('comptime_in', 'module main

type Number = int | f64

fn takes_string(s string) int {
	return s.len
}

fn with_in[T Number](x T) int {
	\$if x in [int] {
		return takes_string(x)
	}
	return 0
}

fn with_type_not_in[T Number](x T) int {
	\$if T !in [f64] {
		return takes_string(x)
	}
	return 0
}

fn describe[T Number](x T) string {
	\$if T in [int] {
		return x.hex()
	}
	return x.str()
}

fn main() {
	println(with_in(1))
	println(with_type_not_in(2))
	println(describe(3))
}
')
	assert errors.len == 2, errors.str()
	assert errors[0].starts_with('main.v:11:23: error: cannot use `int` as `string` in argument 1 to `takes_string`: when `T` is `int`, in its constraint `Number`'), errors[0]
	assert errors[1].starts_with('main.v:18:23: error: cannot use `int` as `string` in argument 1 to `takes_string`: when `T` is `int`, in its constraint `Number`'), errors[1]
}

fn test_an_interface_constraint_checks_the_branch_of_a_type_that_implements_it() {
	// With an interface as its constraint, the body is checked with the interface,
	// where `x is User` is false: the branch of each type that a `$if` tests and
	// that implements the interface is checked with that type as well.
	errors := check('iface_branch', 'module main

interface Named {
	name string
}

struct User {
	name string
	age  int
}

struct Admin {
	name  string
	level int
}

fn takes_int(n int) int {
	return n
}

fn describe[T Named](x T) int {
	\$if x is User {
		return takes_int(x.name)
	}
	return 0
}

fn level_of[T Named](x T) int {
	\$if T is Admin {
		return x.level
	}
	\$if x is User {
		return x.age
	}
	return 0
}

fn main() {
	println(describe(User{"ana", 30}))
	println(level_of(Admin{"bob", 2}))
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:23:20: error: cannot use `string` as `int` in argument 1 to `takes_int`: when `T` is `User`, which implements `Named`'), errors[0]
}

fn test_a_constraint_that_names_its_type_parameter_is_checked_too() {
	// `[T Comparable[T]]`: `T` is any type that implements `Comparable` of
	// itself; the body is checked with that interface, `T` open inside it.
	errors := check('self_constraint', 'module main

interface Comparable[T] {
	less(other T) bool
}

struct Version {
	major int
}

fn (a Version) less(b Version) bool {
	return a.major < b.major
}

fn smallest_index[T Comparable[T]](items []T) int {
	mut best := 0
	for item in items {
		if item.less(items[best]) {
			best = item
		}
	}
	return best
}

fn smallest[T Comparable[T]](items []T) T {
	mut min := items[0]
	for item in items {
		if item.less(min) {
			min = item
		}
	}
	return min
}

fn main() {
	println(smallest_index([Version{2}, Version{1}]))
	println(smallest([Version{2}, Version{1}]))
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:19:11: error: cannot assign to `best`: expected `int`'), errors[0]
	assert errors[0].contains('`T` is any type that implements `Comparable[T]`'), errors[0]
}

fn test_a_constraint_that_names_another_type_parameter_is_checked_too() {
	// `[C Container[T], T Named]`: `C` is checked as `Container[Named]`, the
	// type of `T` put into the type of `C`.
	errors := check('cross_constraint', 'module main

interface Named {
	name string
}

interface Container[T] {
	get() T
}

fn unwrap_name[C Container[T], T Named](c C) string {
	return c.get().name
}

fn unwrap_length[C Container[T], T Named](c C) string {
	return c.get().name.len
}

fn main() {}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:16:9: error: cannot use `int` as type `string` in return argument'), errors[0]
}

fn test_a_local_that_a_compile_time_is_tests_checks_the_branch_of_its_type() {
	// `y := x` holds the `T` of `x`: `$if y is User` has a branch for `User`.
	errors := check('local_is', 'module main

interface Named {
	name string
}

struct User {
	name string
	age  int
}

fn takes_int(n int) int {
	return n
}

fn describe[T Named](x T) int {
	y := x
	\$if y is User {
		return takes_int(y.name)
	}
	return 0
}

fn main() {
	println(describe(User{"ana", 30}))
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:19:20: error: cannot use `string` as `int` in argument 1 to `takes_int`: when `T` is `User`, which implements `Named`'), errors[0]
}

fn test_every_combination_of_the_types_of_the_constraints_is_checked() {
	// 6 x 6 combinations: the error is there only when `T` is `f64` and `U`
	// is `string`, and neither of them is the first type of its set.
	errors := check('combinations', 'module main

type Num6 = int | i8 | i16 | i32 | i64 | f64

type Val6 = int | i8 | i16 | i32 | i64 | string

fn mix[T Num6, U Val6](a T, b U) string {
	\$if T is f64 {
		\$if U is string {
			return a
		}
	}
	return ""
}

fn main() {
	println(mix(1.5, "b"))
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:10:11: error: cannot use `f64` as type `string` in return argument'), errors[0]
	assert errors[0].contains('when `T` is `f64`'), errors[0]
	assert errors[0].contains('when `U` is `string`'), errors[0]
}

fn test_a_generic_body_with_a_type_parameter_without_a_constraint_is_not_checked() {
	// As before: V checks such a body in each of its instances.
	errors := check('unconstrained', "module main

interface Named {
	name string
}

struct User {
	name string
}

struct Box[T] {
	value T
}

struct Shelf[T Named] {
	item T
}

fn takes_int(n int) int {
	return n
}

fn plain[T](x T) T {
	n := takes_int('s')
	println(n)
	return x
}

fn mixed[T Named, U](x T, y U) string {
	n := takes_int('s')
	println(n)
	return x.name
}

fn (b Box[T]) get() T {
	n := takes_int('s')
	println(n)
	return b.value
}

fn (s Shelf[T]) pick[U](u U) string {
	n := takes_int('s')
	println(n)
	return s.item.name
}

fn main() {
	println(plain(1))
	println(mixed(User{'a'}, 2))
	println(Box[int]{3}.get())
	println(Shelf[User]{User{'b'}}.pick(4))
}
")
	assert errors == []string{}
}

fn test_a_generic_body_reports_what_the_constraints_of_its_type_parameters_decide() {
	errors := check('flows', "module main

type Number = int | f64

interface Named {
	name string
}

struct User {
	name string
}

struct Shelf[T Named] {
	item T
}

fn takes_int(n int) int {
	return n
}

fn assign_named[T Named](x T) int {
	mut e := 0
	e = x
	return e
}

fn assign_number[T Number](x T) int {
	mut e := 0
	e = x
	return e
}

fn return_string[T Number](x T) T {
	println(x)
	return 'no'
}

fn pass_named[T Named](x T) int {
	return takes_int(x)
}

fn name_plus_one[T Named](x T) string {
	return x.name + 1
}

fn (s Shelf[T]) count() int {
	return s.item.name
}

fn main() {
	println(assign_named(User{'a'}))
	println(assign_number(1))
	println(return_string(1))
	println(pass_named(User{'a'}))
	println(name_plus_one(User{'a'}))
	println(Shelf[User]{User{'b'}}.count())
}
")
	assert errors.len == 6, errors.str()
	assert errors[0].starts_with('main.v:23:6: error: '), errors[0]
	assert errors[0].contains('`T` is any type that implements `Named`'), errors[0]
	assert errors[1].starts_with('main.v:29:6: error: '), errors[1]
	assert errors[1].contains('when `T` is `f64`, in its constraint `Number`'), errors[1]
	assert errors[2].starts_with('main.v:35:9: error: '), errors[2]
	assert errors[2].contains('in its constraint `Number`'), errors[2]
	assert errors[3].starts_with('main.v:39:19: error: '), errors[3]
	assert errors[3].contains('`T` is any type that implements `Named`'), errors[3]
	assert errors[4].starts_with('main.v:43:'), errors[4]
	assert errors[4].contains('`T` is any type that implements `Named`'), errors[4]
	assert errors[5].starts_with('main.v:47:9: error: '), errors[5]
	assert errors[5].contains('`T` is any type that implements `Named`'), errors[5]
}

fn test_a_generic_body_reports_nothing_the_type_parameters_decide() {
	// Each statement below depends on `T`: the checks against the constraints
	// say what is wrong there, and nothing is wrong here.
	errors := check('dependent', "module main

type Numeric = int | f64

interface Named {
	name string
}

struct User {
	name string
}

fn total_length[T Named](xs []T) int {
	lengths := xs.map(it.name.len)
	mut sum := 0
	for n in lengths {
		sum += n
	}
	return sum
}

fn describe[T Numeric](x T) string {
	\$if T is int {
		return x.hex()
	}
	return x.str()
}

fn first[T Named](xs []T) string {
	for x in xs {
		return x.name
	}
	return ''
}

fn main() {
	println(total_length([User{'a'}]))
	println(describe(10))
	println(first([User{'b'}]))
}
")
	assert errors == []string{}
}

fn test_an_error_of_a_type_parameter_is_told_once_by_its_constraint() {
	errors := check('constraint', "module main

interface Named {
	name string
}

struct User {
	name string
}

fn named[T Named](x T) string {
	return x.nombre
}

fn main() {
	println(named(User{'a'}))
}
")
	assert errors.len == 1, errors.str()
	assert errors[0].contains('type `T` has no field named `nombre`: its constraint `Named` does not declare it'), errors[0]
}
