// vtest build: !windows
module main

import os
import v.cmdexec
import v.compiler_tests.method_form

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
// does, and returns the lines of its errors and warnings. Its method form has to
// report the same (see same_with_methods).
fn check(name string, source string) []string {
	lines := check_form(name, source)
	same_with_methods(name, source, lines, check_form)
	return lines
}

fn check_form(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := cmdexec.run_in(@VEXE, ['-new-compiler', '-check', '-nocolor', '.'], dir)
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& (it.contains(': error: ') || it.contains(': warning: ')))
}

// build builds `source` as the main.v of a directory of its own and returns the
// lines of its errors. Its method form has to report the same.
fn build(name string, source string) []string {
	lines := build_form(name, source)
	same_with_methods(name, source, lines, build_form)
	return lines
}

fn build_form(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := cmdexec.run_in(@VEXE, ['-new-compiler', '-nocolor', '-o', 'prog', '.'], dir)
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& it.contains(': error: '))
}

// same_with_methods checks, with `check`, the method form of `source`: its generic
// functions written as methods of a struct without type parameters, whose own
// type parameters have to behave as those of the functions, with their
// constraints. It has to report the lines that `lines` are, where it moves them
// (see method_form.MethodForm).
fn same_with_methods(name string, source string, lines []string, check fn (string, string) []string) {
	form := method_form.of(source) or { return }
	method_lines := check('${name}_methods', form.source)
	difference := form.differences(lines.map(it.all_after('main.v:')), method_lines.map(it.all_after('main.v:')))
	assert difference == '', 'the method form of `${name}`: ${difference}'
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

fn test_past_the_budget_every_way_the_compile_time_ifs_can_go_is_checked() {
	// 10 * 10 * 10 = 1000 combinations, past the budget: every two type
	// parameters meet with every two of their types, but a branch that `$if`s on
	// three of them lead to needs three types at once. Each way that the `$if`s
	// can go is checked too: through an `$else`, a group, a list or a `&&`.
	errors := check('branches', 'module main

type Number = i8 | i16 | i32 | int | i64 | u8 | u16 | u32 | f32 | f64

fn nested[A Number, B Number, C Number](a A, b B, c C) int {
	\$if A is f64 {
		\$if B is f64 {
			\$if C is f64 {
				return a
			}
		}
	}
	return 0
}

fn in_else[A Number, B Number, C Number](a A, b B, c C) int {
	\$if A is i8 {
		return 1
	} \$else {
		\$if B is f64 {
			\$if C is f64 {
				return b
			}
		}
	}
	return 0
}

fn groups[A Number, B Number, C Number](a A, b B, c C) int {
	\$if A is \$float {
		\$if B in [f32, f64] {
			\$if c is \$float {
				return c
			}
		}
	}
	return 0
}

fn joined[A Number, B Number, C Number](a A, b B, c C) int {
	\$if a is f64 && b is f64 && c is f64 {
		return a
	}
	return 0
}

fn main() {}
')
	assert errors.len == 4, errors.str()
	assert errors[0] == 'main.v:9:12: error: cannot use `f64` as type `int` in return argument: when `A` is `f64`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`, and when `C` is `f64`, in its constraint `Number`', errors[0]
	assert errors[1] == 'main.v:22:12: error: cannot use `f64` as type `int` in return argument: when `A` is not `i8`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`, and when `C` is `f64`, in its constraint `Number`', errors[1]
	assert errors[2] == 'main.v:33:12: error: cannot use `f32` as type `int` in return argument: when `A` is `f32` or `f64`, in its constraint `Number`, and when `B` is `f32` or `f64`, in its constraint `Number`, and when `C` is `f32`, in its constraint `Number`', errors[2]
	assert errors[3] == 'main.v:42:10: error: cannot use `f64` as type `int` in return argument: when `A` is `f64`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`, and when `C` is `f64`, in its constraint `Number`', errors[3]
}

fn test_each_type_of_a_compile_time_in_list_checks_its_branch() {
	// `$if x in [User, Admin] {` with an interface: the body is checked with
	// each type of the list, the second one too.
	errors := check('in_list', 'module main

interface Named {
	name string
}

struct User {
	name string
}

struct Admin {
	name string
}

fn (u User) label() string {
	return u.name
}

fn (a Admin) label() int {
	return a.name.len
}

fn tag[T Named](x T) string {
	\$if x in [User, Admin] {
		return x.label()
	}
	return x.name
}

fn main() {
	println(tag(User{"ana"}))
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:25:10: error: cannot use `int` as type `string` in return argument: when `T` is `Admin`, which implements `Named`'), errors[0]
}

fn test_in_each_way_the_compile_time_ifs_go_every_type_is_checked() {
	// Past the budget, a branch that `$if`s on `A` and `B` lead to is checked with
	// each type of `C` too, which no `$if` tests: `takes_i64(c)` fails only when
	// `C` is `f32` or `f64`, neither the first type of `Number`.
	errors := check('untested', 'module main

type Number = i8 | i16 | i32 | int | i64 | u8 | u16 | u32 | f32 | f64

fn takes_i64(n i64) i64 {
	return n
}

fn wide[A Number, B Number, C Number](a A, b B, c C) i64 {
	\$if A is f64 {
		\$if B is f64 {
			return takes_i64(c)
		}
	}
	return 0
}

fn main() {}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('main.v:12:21: error: cannot use `f32` as `i64` in argument 1 to `takes_i64`: '), errors[0]
}

fn test_an_error_names_only_the_type_parameters_that_decide_it() {
	// `return a` fails whatever `C` is: the error does not name it.
	errors := check('deciders', 'module main

type Number = i8 | i16 | i32 | int | i64 | u8 | u16 | u32 | f32 | f64

fn two[A Number, B Number, C Number](a A, b B, c C) int {
	\$if A is f64 {
		\$if B is f64 {
			return a
		}
	}
	return 0
}

fn main() {}
')
	assert errors == ['main.v:8:11: error: cannot use `f64` as type `int` in return argument: when `A` is `f64`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`'], errors.str()
}

fn test_a_type_that_constraints_unfold_into_is_named_by_its_type_parameter() {
	// `[C Container[T], T Wrapper[C]]`: `z` is a `T`, not the unfolding of what
	// `T` and `C` name of each other.
	errors := check('unfolded', 'module main

interface Wrapper[C] {
	inner() C
}

interface Container[T] {
	get() T
}

fn loop_types[C Container[T], T Wrapper[C]](c C) int {
	x := c.get()
	y := x.inner()
	z := y.get()
	return z
}

fn main() {}
')
	assert errors == ['main.v:15:9: error: cannot use `T` as type `int` in return argument: `C` is any type that implements `Container[T]`, and `T` is any type that implements `Wrapper[C]`'], errors.str()
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
