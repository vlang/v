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

fn check_form(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := cmdexec.run_in(@VEXE, ['-check', '-nocolor', '.'], dir)
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& (it.contains(': error: ') || it.contains(': warning: ')))
}

// check_fixture_form keeps fixture-mode deferral of constrained bodies while checking visibility.
fn check_fixture_form(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := cmdexec.run_in(@VEXE, ['-checker-fixture', '-check', '-nocolor', '.'], dir)
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& (it.contains(': error: ') || it.contains(': warning: ')))
}

fn build_form(name string, source string) []string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	res := cmdexec.run_in(@VEXE, ['-nocolor', '-o', 'prog', '.'], dir)
	return res.output.split_into_lines().filter(it.starts_with('main.v:')
		&& it.contains(': error: '))
}

fn assert_body_method_diagnostics(name string, form method_form.MethodForm, functions []string, methods []string) {
	difference := form.differences(functions.map(it.all_after('main.v:')), methods.map(it.all_after('main.v:')))
	assert difference == '', 'the method form of ${name}: ${difference}'
}

fn test_a_build_checks_a_generic_body_whose_type_parameters_all_have_constraints() {
	// As it checks the body of any other function: the errors are V's, not
	// those of the C compiler, and `e = x` is one although `main` only asks for
	// the instance with `int`.
	source := 'module main

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
'
	form := method_form.of(source) or { panic('missing method form for build') }
	diagnostics := check_body_batch('build_forms', {
		'build':         source
		'build_methods': form.source
	}, '', ['-o', 'prog'])
	errors := diagnostics['build']
	assert errors.len == 2, errors.str()
	assert errors[0].starts_with('main.v:7:6: error: cannot assign to `e`: expected `int`, not `f64`: when `T` is `f64`, in its constraint `Number`'), errors[0]
	assert errors[1].starts_with('main.v:12:2: error: unexpected name `asd`'), errors[1]
	assert_body_method_diagnostics('build', form, errors, diagnostics['build_methods'])
}

struct GenericBodyDiagnosticCase {
	name     string
	source   string
	prefixes []string
}

fn check_body_batch(name string, sources map[string]string, main_fixture string, flags []string) map[string][]string {
	dir := os.join_path(work_dir, name)
	os.mkdir_all(dir) or { panic(err) }
	mut imports := []string{cap: sources.len}
	mut calls := []string{cap: sources.len}
	mut diagnostics := map[string][]string{}
	for label, source in sources {
		diagnostics[label] = []
		if label == main_fixture {
			// The unused constrained bodies need main-module resolution.
			entry := 'exercise_${label}'
			fixture := source.replace_once('\nfn main()', '\nfn ${entry}()')
			os.write_file(os.join_path(dir, '${label}.v'), fixture) or { panic(err) }
			calls << '\t${entry}()'
			continue
		}
		mod_dir := os.join_path(dir, label)
		os.mkdir_all(mod_dir) or { panic(err) }
		fixture := source.replace_once('module main\n', 'module ${label}\n').replace_once('\nfn main()',
			'\npub fn exercise()')
		os.write_file(os.join_path(mod_dir, 'main.v'), fixture) or { panic(err) }
		imports << 'import ${label}'
		calls << '\t${label}.exercise()'
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n') or {
		panic(err)
	}
	res := cmdexec.run_in(@VEXE, ['-nocolor', '-message-limit', '100', ...flags, '.'], dir)
	assert res.exit_code != 0, res.output
	assert !res.output.contains('C compilation error'), res.output
	for line in res.output.split_into_lines() {
		if !line.contains(': error: ') && !line.contains(': warning: ') {
			continue
		}
		file := line.all_before(':')
		label := if file.ends_with('/main.v') {
			file.all_before('/main.v')
		} else {
			file.all_before('.v')
		}
		assert label in diagnostics, line
		// Normalize the fixture path while preserving line and column.
		diagnostics[label] << 'main.v:' + line.all_after('${file}:').replace_each([
			'${name}.${label}.',
			'',
			'${label}.',
			'',
		])
	}
	return diagnostics
}

fn test_generic_body_function_and_method_diagnostics() {
	cases := [
		GenericBodyDiagnosticCase{
			name:     'independent'
			source:   "module main

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
"
			prefixes: [
				'main.v:19:2: error: unexpected name `asd`',
				'main.v:23:9: error: operator `+` cannot concatenate `int` and `string`',
				'main.v:30:9: error: cannot use `int literal` as type `string` in return argument',
			]
		},
		// `$if x in [int]` and `$if T !in [f64]` choose code by the type, as `is`
		// does: each check of the body takes the branch that its type takes.
		GenericBodyDiagnosticCase{
			name:     'comptime_in'
			source:   'module main

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
'
			prefixes: [
				'main.v:11:23: error: cannot use `int` as `string` in argument 1 to `takes_string`: when `T` is `int`, in its constraint `Number`',
				'main.v:18:23: error: cannot use `int` as `string` in argument 1 to `takes_string`: when `T` is `int`, in its constraint `Number`',
			]
		},
		// With an interface as its constraint, the body is checked with the interface,
		// where `x is User` is false: the branch of each type that a `$if` tests and
		// that implements the interface is checked with that type as well.
		GenericBodyDiagnosticCase{
			name:     'iface_branch'
			source:   'module main

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
'
			prefixes: [
				'main.v:23:20: error: cannot use `string` as `int` in argument 1 to `takes_int`: when `T` is `User`, which implements `Named`',
			]
		},
		// `[T Comparable[T]]`: `T` is any type that implements `Comparable` of
		// itself; the body is checked with that interface, `T` open inside it.
		GenericBodyDiagnosticCase{
			name:     'self_constraint'
			source:   'module main

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
'
			prefixes: [
				'main.v:19:11: error: cannot assign to `best`: expected `int`',
			]
		},
		// `y := x` holds the `T` of `x`: `$if y is User` has a branch for `User`.
		GenericBodyDiagnosticCase{
			name:     'local_is'
			source:   'module main

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
'
			prefixes: [
				'main.v:19:20: error: cannot use `string` as `int` in argument 1 to `takes_int`: when `T` is `User`, which implements `Named`',
			]
		},
		// 6 x 6 combinations: the error is there only when `T` is `f64` and `U`
		// is `string`, and neither of them is the first type of its set.
		GenericBodyDiagnosticCase{
			name:     'combinations'
			source:   'module main

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
'
			prefixes: [
				'main.v:10:11: error: cannot use `f64` as type `string` in return argument',
			]
		},
		// `$if x in [User, Admin] {` with an interface: the body is checked with
		// each type of the list, the second one too.
		GenericBodyDiagnosticCase{
			name:     'in_list'
			source:   'module main

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
'
			prefixes: [
				'main.v:25:10: error: cannot use `int` as type `string` in return argument: when `T` is `Admin`, which implements `Named`',
			]
		},
		GenericBodyDiagnosticCase{
			name:     'flows'
			source:   "module main

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
"
			prefixes: [
				'main.v:23:6: error: cannot assign to `e`: expected `int`, not `Named`: `T` is any type that implements `Named`',
				'main.v:29:6: error: cannot assign to `e`: expected `int`, not `f64`: when `T` is `f64`, in its constraint `Number`',
				'main.v:35:9: error: cannot use `string` as type `int` in return argument: when `T` is `int`, in its constraint `Number`',
				'main.v:39:19: error: cannot use `Named` as `int` in argument 1 to `takes_int`: `T` is any type that implements `Named`',
				'main.v:43:9: error: infix expr: cannot use `int literal` (right expression) as `string`: `T` is any type that implements `Named`',
				'main.v:47:9: error: cannot use `string` as type `int` in return argument: `T` is any type that implements `Named`',
			]
		},
		GenericBodyDiagnosticCase{
			name:     'constraint'
			source:   "module main

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
"
			prefixes: [
				'main.v:12:11: error: type `T` has no field named `nombre`: its constraint `Named` does not declare it',
			]
		},
	]
	mut sources := map[string]string{}
	for case in cases {
		sources[case.name] = case.source
	}
	sources['unused_bodies'] = generic_unused_bodies_source
	sources['branches'] = generic_branch_budget_source
	sources['untested'] = generic_untested_parameter_source
	function_errors := check_body_batch('body_functions', sources, 'unused_bodies', ['-check'])

	mut forms := map[string]method_form.MethodForm{}
	mut method_sources := map[string]string{}
	for name, source in sources {
		form := method_form.of(source) or { panic('missing method form for ${name}') }
		forms[name] = form
		method_sources[name] = form.source
	}
	method_errors := check_body_batch('body_methods', method_sources, 'unused_bodies', ['-check'])
	for case in cases {
		errors := function_errors[case.name]
		assert errors.len == case.prefixes.len, '${case.name}: ${errors}'
		for i, expected in case.prefixes {
			assert errors[i].starts_with(expected), '${case.name}: ${errors[i]}'
		}
	}
	for name, form in forms {
		assert_body_method_diagnostics(name, form, function_errors[name], method_errors[name])
	}
	assert function_errors['self_constraint'][0].contains('`T` is any type that implements `Comparable[T]`'), function_errors['self_constraint'][0]
	assert function_errors['combinations'][0].contains('when `T` is `f64`'), function_errors['combinations'][0]
	assert function_errors['combinations'][0].contains('when `U` is `string`'), function_errors['combinations'][0]

	unused_errors := function_errors['unused_bodies']
	assert unused_errors.len == 3, unused_errors.str()
	assert unused_errors[0].starts_with('main.v:20:9: error: cannot use `int` as type `string` in return argument'), unused_errors[0]
	assert unused_errors[1] == 'main.v:27:11: error: cannot use `f64` as type `int` in return argument: when `A` is `f64`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`', unused_errors[1]
	assert unused_errors[2] == 'main.v:43:9: error: cannot use `T` as type `int` in return argument: `C` is any type that implements `Container[T]`, and `T` is any type that implements `Wrapper[C]`', unused_errors[2]

	branch_errors := function_errors['branches']
	assert branch_errors.len == 4, branch_errors.str()
	assert branch_errors[0] == 'main.v:9:12: error: cannot use `f64` as type `int` in return argument: when `A` is `f64`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`, and when `C` is `f64`, in its constraint `Number`', branch_errors[0]
	assert branch_errors[1] == 'main.v:22:12: error: cannot use `f64` as type `int` in return argument: when `A` is not `i8`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`, and when `C` is `f64`, in its constraint `Number`', branch_errors[1]
	assert branch_errors[2] == 'main.v:33:12: error: cannot use `f32` as type `int` in return argument: when `A` is `f32` or `f64`, in its constraint `Number`, and when `B` is `f32` or `f64`, in its constraint `Number`, and when `C` is `f32`, in its constraint `Number`', branch_errors[2]
	assert branch_errors[3] == 'main.v:42:10: error: cannot use `f64` as type `int` in return argument: when `A` is `f64`, in its constraint `Number`, and when `B` is `f64`, in its constraint `Number`, and when `C` is `f64`, in its constraint `Number`', branch_errors[3]

	untested_errors := function_errors['untested']
	assert untested_errors.len == 1, untested_errors.str()
	assert untested_errors[0].starts_with('main.v:12:21: error: cannot use `f32` as `i64` in argument 1 to `takes_i64`: '), untested_errors[0]
}

const generic_unused_bodies_source = 'module main

type Number = i8 | i16 | i32 | int | i64 | u8 | u16 | u32 | f32 | f64

interface Container[T] {
	get() T
}

// `[C Container[T], T Named]`: `C` is checked as `Container[Named]`, the
// type of `T` put into the type of `C`.
interface Named {
	name string
}

fn unwrap_name[C Container[T], T Named](c C) string {
	return c.get().name
}

fn unwrap_length[C Container[T], T Named](c C) string {
	return c.get().name.len
}

// `return a` fails whatever `C` is: the error does not name it.
fn two[A Number, B Number, C Number](a A, b B, c C) int {
	\$if A is f64 {
		\$if B is f64 {
			return a
		}
	}
	return 0
}

// `[C Container[T], T Wrapper[C]]`: `z` is a `T`, not the unfolding of what
// `T` and `C` name of each other.
interface Wrapper[C] {
	inner() C
}

fn loop_types[C Container[T], T Wrapper[C]](c C) int {
	x := c.get()
	y := x.inner()
	z := y.get()
	return z
}

fn main() {}
'

// 10 * 10 * 10 = 1000 combinations, past the budget: every two type
// parameters meet with every two of their types, but a branch that `$if`s on
// three of them lead to needs three types at once. Each way that the `$if`s
// can go is checked too: through an `$else`, a group, a list or a `&&`.
const generic_branch_budget_source = 'module main

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
'

// Past the budget, a branch that `$if`s on `A` and `B` lead to is checked with
// each type of `C` too, which no `$if` tests: `takes_i64(c)` fails only when
// `C` is `f32` or `f64`, neither the first type of `Number`.
const generic_untested_parameter_source = 'module main

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
'

// As before: V checks such a body in each of its instances, not open.
const generic_unconstrained_source = "module main

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
"

fn test_a_generic_body_without_a_constraint_reports_what_is_private_to_another_module() {
	// Whether a name of another module is private does not depend on `T`: a
	// check and a build report it for the function, with an instance or without
	// one, as for a function without type parameters. A member reached through
	// `T` has no type there, and the rest of the body is left to the instances.
	source := 'module main

import privlib

fn unused[T](mut s privlib.S, x T) T {
	s.secret = 1
	return x
}

fn used[T](mut s privlib.S, x T) T {
	s.shown = privlib.private_fn() + s.private_method()
	return x
}

fn through[T](mut s T) {
	s.shown = 2
}

fn main() {
	mut s := privlib.S{}
	println(used(mut s, 1))
	through(mut s)
}
'
	for name, run in {
		'private':         check_form
		'private_build':   build_form
		'private_fixture': check_fixture_form
	} {
		path := os.join_path(work_dir, name, 'privlib')
		os.mkdir_all(path) or { panic(err) }
		os.write_file(os.join_path(path, 'privlib.v'), 'module privlib

pub struct S {
	secret int
pub mut:
	shown int
}

fn private_fn() int {
	return 1
}

fn (s S) private_method() int {
	return s.secret
}
') or {
			panic(err)
		}
		errors := run(name, source)
		assert errors == [
			'main.v:6:4: error: field `${name}.privlib.S.secret` is not public',
			'main.v:11:20: error: function `${name}.privlib.private_fn` is private',
			'main.v:11:37: error: method `privlib.S.private_method` is private',
		], errors.str()
	}
}

// Each statement below depends on `T`: the checks against the constraints
// say what is wrong there, and nothing is wrong here.
const generic_dependent_source = "module main

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
"

fn test_generic_body_instance_diagnostics() {
	// The unconstrained instances report late errors. The other fixture has to
	// reach the same checking stage with no diagnostics in either form.
	sources := {
		'unconstrained': generic_unconstrained_source
		'dependent':     generic_dependent_source
	}
	mut fixtures := sources.clone()
	mut forms := map[string]method_form.MethodForm{}
	for name, source in sources {
		form := method_form.of(source) or { panic('missing method form for ${name}') }
		forms[name] = form
		fixtures['${name}_methods'] = form.source
	}
	diagnostics := check_body_batch('instance_checks', fixtures, '', ['-check'])
	for name, form in forms {
		assert_body_method_diagnostics(name, form, diagnostics[name], diagnostics['${name}_methods'])
	}
	errors := diagnostics['unconstrained']
	assert errors == [
		'main.v:24:17: error: cannot use `string` as `int` in argument 1 to `takes_int`',
		'main.v:30:17: error: cannot use `string` as `int` in argument 1 to `takes_int`',
		'main.v:36:17: error: cannot use `string` as `int` in argument 1 to `takes_int`',
		'main.v:42:17: error: cannot use `string` as `int` in argument 1 to `takes_int`',
	], errors.str()
	assert diagnostics['dependent'] == []string{}
}
