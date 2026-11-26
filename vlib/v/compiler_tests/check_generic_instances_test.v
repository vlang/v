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

fn check_program(name string, source string, flags ...string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_generic_instances_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude + source) or { panic(err) }
	return os.exec([@VEXE, '-check', '-nocolor', ...flags, path])
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

fn test_instances_report_missing_fields_and_methods() {
	res := check_program('missing_members', "fn show[T](x T) string {
	return x.name
}

fn misspelled[T](x T) string {
	return x.nme
}

fn call[T](x T) string {
	return x.greet()
}

struct Box[T] {
	v T
}

fn (b Box[T]) label() string {
	return b.v.name
}

type Octet = string

struct Elm[T] {
	val T
}

fn (el Elm[T]) size() int {
	return el.val.size()
}

fn main() {
	println(show(1))
	println(misspelled(User{ name: 'a' }))
	println(call(User{ name: 'a' }))
	println(call(1))
	println(Box[User]{ v: User{ name: 'a' } }.label())
	println(Box[int]{ v: 1 }.label())

	el := Elm[Octet]{
		val: Octet('xx')
	}
	println(el.size())
}
")
	assert res.exit_code == 1, res.output
	errors := error_lines(res.output)
	assert errors.len == 5, res.output
	assert '11:11: `int` has no property `name`' in errors, res.output
	assert errors.any(it.starts_with('15:11: type `User` has no field named `nme`')), res.output
	assert '19:11: unknown method or field: `int.greet`.' in errors, res.output
	assert '27:13: `int` has no property `name`' in errors, res.output
	assert '37:16: unknown method or field: `string.size`' in errors, res.output
}

// Specializations resolve fields and methods on concrete types and keep the
// methods declared on a type argument such as the Octet alias.
fn test_instances_use_members_of_concrete_types_and_aliases() {
	source := "fn show[T](x T) string {
	return x.name
}

fn call[T](x T) string {
	return x.greet()
}

type Octet = string

fn (o Octet) pack() []u8 {
	return o.bytes()
}

struct Elm[T] {
	val T
}

fn (el Elm[T]) pack() []u8 {
	return el.val.pack()
}

fn main() {
	println(show(User{ name: 'a' }))
	println(call(User{ name: 'b' }))

	el := Elm[Octet]{
		val: Octet('xx')
	}
	println(el.pack())
}
"
	for res in [check_program('instance_members_check', source),
		build_program('instance_members_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// A method of a generic struct that declares a return type but returns
// nothing is reported for the declaration itself: no instance returns either.
fn test_a_generic_struct_method_without_a_return_is_reported() {
	res := check_program('method_missing_return', 'struct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) label() string {\n}\n\nfn main() {\n\tprintln(Box[int]{ v: 1 }.label())\n}\n')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['14:1: missing return at end of function `label`'], res.output
}

// Assigning through the immutable receiver of a method of a generic struct is
// reported for the instance: the receiver is immutable whatever `T` is.
fn test_a_generic_struct_method_assigning_through_an_immutable_receiver_is_reported() {
	res := check_program('method_immutable_receiver', 'struct Box[T] {\nmut:\n\tv T\n}\n\nfn (b &Box[T]) set(v T) {\n\tb.v = v\n}\n\nfn main() {\n\tmut b := Box[int]{}\n\tb.set(1)\n\tprintln(b.v)\n}\n')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['16:2: `b` is immutable, declare it with `mut` to make it mutable'], res.output
}

// Calling a method of a generic struct with fewer arguments than it takes is
// reported where the generic caller does so: the missing arguments are not
// zeroes.
fn test_a_generic_struct_method_called_with_fewer_arguments_is_reported() {
	res := check_program('method_arg_count', 'struct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) show(n int, rest []int) {\n\tprintln(n)\n\tprintln(rest.len)\n}\n\nfn call[T](b Box[T]) {\n\tb.show()\n}\n\nfn main() {\n\tcall(Box[int]{ v: 1 })\n}\n')
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['20:4: expected 2 arguments, but got 0'], res.output
}

// Methods of a generic struct that return, mutate through a `mut` receiver,
// and are called with the right number of arguments stay valid.
fn test_valid_generic_struct_methods_stay_valid() {
	res := check_program('methods_valid', 'struct Box[T] {\nmut:\n\tv T\n}\n\nfn (b &Box[T]) get() T {\n\treturn b.v\n}\n\nfn (mut b Box[T]) set(v T) {\n\tb.v = v\n}\n\nfn (b Box[T]) show(n int, rest []int) int {\n\tif rest.len > 0 {\n\t\treturn n + rest[0]\n\t}\n\treturn n\n}\n\nfn call[T](b Box[T]) int {\n\treturn b.show(1, [2])\n}\n\nfn main() {\n\tmut b := Box[int]{\n\t\tv: 3\n\t}\n\tprintln(b.get())\n\tb.set(10)\n\tprintln(b.get())\n\tprintln(call(b))\n}\n')
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

// A generic function or method that ends in a user-defined `@[noreturn]` call
// does not fall through: no `return` is missing. The builtin `panic`/`exit`
// and an infinite loop terminate the same way.
fn test_generic_noreturn_terminated_functions_stay_valid() {
	source := "@[noreturn]\nfn die() { panic('expected') }\n\nfn fail[T]() T { die() }\n\nstruct NBox[T] {\n\tv T\n}\n\nfn (b NBox[T]) fail() T { die() }\n\nfn panic_fail[T]() T { panic('boom') }\n\nfn exit_fail[T]() T { exit(1) }\n\nfn loop_fail[T]() T { for {} }\n\nfn main() {\n\tif false { _ = fail[int]() }\n\tif false { _ = NBox[int]{}.fail() }\n\tif false { _ = panic_fail[int]() }\n\tif false { _ = exit_fail[int]() }\n\tif false { _ = loop_fail[int]() }\n\tprintln('ok')\n}\n"
	for res in [check_program('noreturn_valid_check', source),
		build_program('noreturn_valid_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// Calling a capturing closure through another local bound to it passes no
// arguments either: the clone counts the captures as parameters for the alias
// as it does for the original.
fn test_generic_closure_alias_calls_stay_valid() {
	source := 'fn apply[T](value T) {\n\tread := fn [value] [T]() { println(value) }\n\talias := read\n\talias()\n\tread()\n}\n\nfn main() {\n\tapply(3)\n}\n'
	for res in [check_program('closure_alias_check', source),
		build_program('closure_alias_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

const box_with_label = 'struct Box[T] {
	v T
}

fn (b Box[T]) label() string {
	return b.v.name
}

type Sum = Box[int] | string

struct Holder {
	s Sum
}

'

// A check monomorphizes the tree as it was parsed. There a smartcast receiver is
// still the sum type or the option, which the method of the variant does not
// belong to: only what the checker narrowed it to tells which instance it asks for.
fn test_an_instance_asked_for_by_a_smartcast_receiver_is_reported() {
	bodies := {
		'match_arm':     "fn f(s Sum) string {\n\treturn match s {\n\t\tBox[int] { s.label() }\n\t\tstring { s }\n\t}\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n}\n"
		'mut_match_arm': "fn f(mut s Sum) string {\n\treturn match mut s {\n\t\tBox[int] { s.label() }\n\t\tstring { s }\n\t}\n}\n\nfn main() {\n\tmut s := Sum('a')\n\tprintln(f(mut s))\n}\n"
		'if_is':         "fn f(s Sum) string {\n\tif s is Box[int] {\n\t\treturn s.label()\n\t}\n\treturn ''\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n}\n"
		'field':         "fn f(h Holder) string {\n\treturn match h.s {\n\t\tBox[int] { h.s.label() }\n\t\tstring { h.s }\n\t}\n}\n\nfn main() {\n\tprintln(f(Holder{ s: Sum('a') }))\n}\n"
		'paren':         "fn f(s Sum) string {\n\treturn match s {\n\t\tBox[int] { (s).label() }\n\t\tstring { s }\n\t}\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n}\n"
		'nested_paren':  "fn f(h Holder) string {\n\tif h.s is Box[int] {\n\t\treturn ((h.s)).label()\n\t}\n\treturn ''\n}\n\nfn main() {\n\tprintln(f(Holder{ s: Sum('a') }))\n}\n"
		'none_guard':    "fn f(b ?Box[int]) string {\n\tif b != none {\n\t\treturn b.label()\n\t}\n\treturn ''\n}\n\nfn main() {\n\tprintln(f(none))\n}\n"
		// The narrowing after a guard that returns, and that of a captured value,
		// are not found from the enclosing blocks: the checker records them.
		'not_is_guard':  "fn f(s Sum) string {\n\tif s !is Box[int] {\n\t\treturn ''\n\t}\n\treturn s.label()\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n}\n"
		'closure':       "fn f(s Sum) string {\n\tif s is Box[int] {\n\t\tg := fn [s] () string {\n\t\t\treturn s.label()\n\t\t}\n\t\treturn g()\n\t}\n\treturn ''\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n}\n"
	}
	for name, body in bodies {
		res := check_program('smartcast_${name}', box_with_label + body)
		assert res.exit_code == 1, '${name}: ${res.output}'
		assert error_lines(res.output) == ['15:13: `int` has no property `name`'], '${name}: ${res.output}'
	}
}

fn test_a_method_with_its_own_parameter_on_a_smartcast_receiver_is_reported() {
	res := check_program('smartcast_method_param', "struct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) label[U](u U) string {\n\treturn u.name + b.v.str()\n}\n\ntype Sum = Box[int] | string\n\nfn f(s Sum) string {\n\treturn match s {\n\t\tBox[int] { s.label(1) }\n\t\tstring { s }\n\t}\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n}\n")
	assert res.exit_code == 1, res.output
	assert error_lines(res.output) == ['15:11: `int` has no property `name`'], res.output
}

// Only the variant that the arm narrows to is asked for: `Box[int]` has no arm
// that calls `label` here, and must not be reported.
fn test_a_smartcast_receiver_asks_only_for_the_variant_of_its_arm() {
	res := check_program('smartcast_variant', "struct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) label() string {\n\treturn b.v.name\n}\n\ntype Sum = Box[User] | Box[int] | string\n\nfn f(s Sum) string {\n\treturn match s {\n\t\tBox[User] { s.label() }\n\t\tBox[int] { 'int' }\n\t\tstring { s }\n\t}\n}\n\nfn g(s Sum) string {\n\tif s is Box[User] {\n\t\treturn s.label()\n\t}\n\treturn ''\n}\n\nfn h(s Sum) string {\n\tif s !is Box[User] {\n\t\treturn ''\n\t}\n\treturn s.label()\n}\n\nfn main() {\n\tprintln(f(Sum('a')))\n\tprintln(g(Sum('a')))\n\tprintln(h(Sum('a')))\n}\n")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
}

// The shadowing is found in the clone of `m`, which has to exist in a check too.
fn test_a_global_shadowed_in_an_instance_of_a_smartcast_receiver_is_reported() {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_generic_instances_smartcast_global_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, "module main\n\n__global gv = 1\n\ntype Sum = Box[int] | string\n\nstruct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) m() T {\n\t\$if T is int {\n\t\tgv := 3\n\t\t_ = gv\n\t}\n\treturn b.v\n}\n\nfn wrap(s Sum) int {\n\treturn match s {\n\t\tBox[int] { s.m() }\n\t\tstring { 0 }\n\t}\n}\n\nfn main() {\n\tprintln(wrap(Sum('a')))\n}\n") or {
		panic(err)
	}
	check := os.exec([@VEXE, '-enable-globals', '-check', '-nocolor', path])
	build := os.exec([@VEXE, '-enable-globals', '-nocolor', '-o', os.join_path(dir, 'program'),
		path])
	for res in [check, build] {
		assert res.exit_code == 1, res.output
		assert error_lines(res.output) == ['13:3: variable `gv` shadows a global variable'], res.output
	}
}

// A library module outside the program, where ~/.vmodules keeps them. A check
// checks the instances of the program's generics only, so the body of an
// instance of this module matters only where it can ask for one of those.
const library_module = 'module advlib

pub interface Namer {
	name() string
}

// total applies `+` to values of T.
pub fn total[T](xs []T) T {
	mut acc := xs[0]
	for x in xs[1..] {
		acc = acc + x
	}
	return acc
}

// label_of calls a method of T.
pub fn label_of[T](x T) string {
	return x.label()
}

// label_twice asks for label_of[T], an instance that the library asks for.
pub fn label_twice[T](x T) string {
	return label_of(x) + label_of(x)
}

// with_namer calls an interface method on a value that does not depend on T.
pub fn with_namer[T](xs []T, n Namer) string {
	return n.name() + xs.len.str()
}

// names_of calls the interface method of T.
pub fn names_of[T](xs []T) string {
	mut s := ""
	for x in xs {
		s += x.name()
	}
	return s
}

// run_job calls a method with type parameters of its own on a field of T.
pub fn run_job[T](x T) string {
	return x.runner.run(1)
}
'

struct LibraryCheck {
	res   os.Result
	trace []string // what the check traced about the instances
}

// check_with_library checks `source` as a program that can import advlib.
fn check_with_library(name string, source string) LibraryCheck {
	return check_with_modules_env(name, '', {
		'main.v': source
	}, {
		'advlib/advlib.v': library_module
	})
}

// check_with_modules writes the program `files` and, outside it, the library
// `modules`, and checks the program's main.v.
fn check_with_modules(name string, files map[string]string, modules map[string]string) LibraryCheck {
	return check_with_modules_env(name, '', files, modules)
}

// check_with_modules_env is check_with_modules with the environment `env`
// (`NAME=value`) added to the check.
fn check_with_modules_env(name string, env string, files map[string]string, modules map[string]string) LibraryCheck {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_generic_library_${name}_${os.getpid()}')
	modules_dir := os.join_path(dir, 'modules')
	program := os.join_path(dir, 'program')
	defer {
		os.rmdir_all(dir) or {}
	}
	for root, contents in {
		modules_dir: modules
		program:     files
	} {
		for relative, content in contents {
			path := os.join_path(root, relative)
			os.mkdir_all(os.dir(path)) or { panic(err) }
			os.write_file(path, content) or { panic(err) }
		}
	}
	trace := os.join_path(dir, 'trace.txt')
	// The child reads these from its environment, so set them here and pass only the
	// compiler arguments to `os.exec`.
	previous_modules := os.getenv_opt('VMODULES')
	previous_trace := os.getenv_opt('V_DIAGNOSTICS_TRACE')
	os.setenv('VMODULES', modules_dir, true)
	os.setenv('V_DIAGNOSTICS_TRACE', trace, true)
	defer {
		if value := previous_modules {
			os.setenv('VMODULES', value, true)
		} else {
			os.unsetenv('VMODULES')
		}
		if value := previous_trace {
			os.setenv('V_DIAGNOSTICS_TRACE', value, true)
		} else {
			os.unsetenv('V_DIAGNOSTICS_TRACE')
		}
	}
	// V runs a `defer` at the end of its block, so the variable is declared out here to
	// stay set until the check has run.
	env_name := env.all_before('=')
	previous_env := os.getenv_opt(env_name)
	if env != '' {
		os.setenv(env_name, env.all_after('='), true)
	}
	defer {
		if env_name != '' {
			if value := previous_env {
				os.setenv(env_name, value, true)
			} else {
				os.unsetenv(env_name)
			}
		}
	}
	res := os.exec([@VEXE, '-check', '-nocolor', os.join_path(program, 'main.v')])
	prefix := 'v-diagnostics-server: instances: '
	lines := (os.read_file(trace) or { '' }).split_into_lines().filter(it.starts_with(prefix)).map(it[prefix.len..])
	return LibraryCheck{
		res:   res
		trace: lines
	}
}

const dispatch_program = 'import advlib

struct Box[T] {
	v T
}

fn (b Box[T]) name() string {
	return b.v.name
}

fn main() {
	n := advlib.Namer(Box[int]{ v: 1 })
	println(advlib.with_namer([1], n))
}
'

const header_program = 'import advlib\n\nstruct User {\n\tname string\n}\n\nfn (u User) label() string {\n\treturn u.name\n}\n\nfn show[T](x T) string {\n\treturn x.label()\n}\n\nfn main() {\n\tprintln(show(User{}))\n\tprintln(advlib.label_twice(User{}))\n}\n'

const method_level_program = 'import advlib\n\nstruct Runner {}\n\nfn (r Runner) run[T](x T) string {\n\treturn x.name\n}\n\nstruct Job {\n\trunner Runner\n}\n\nfn main() {\n\tprintln(advlib.run_job(Job{}))\n}\n'

fn test_a_check_clones_a_library_instance_without_its_body() {
	// `label_of[User]`, which the library asks for, calls `User.label`, which is
	// no instance: its body can give the check nothing to look at. The program
	// asks for `label_twice[User]`, which keeps its body to be checked.
	check := check_with_library('headers', header_program)
	assert check.res.exit_code == 0, check.res.output
	assert check.trace == ['1 of 2 library instances cloned without their bodies'], check.trace.str()
}

fn test_only_all_keeps_every_library_body() {
	for value in ['1', ''] {
		check := check_with_modules_env('bodies_not_all_${value}', 'V_CHECK_LIBRARY_BODIES=${value}', {
			'main.v': header_program
		}, {
			'advlib/advlib.v': library_module
		})
		assert check.res.exit_code == 0, check.res.output
		assert check.trace == ['1 of 2 library instances cloned without their bodies'], check.trace.str()
	}
}

fn test_v_check_library_bodies_keeps_bodies_and_restores_environment() {
	keys := ['VMODULES', 'V_DIAGNOSTICS_TRACE', 'V_CHECK_LIBRARY_BODIES']
	mut previous := map[string]string{}
	mut present := map[string]bool{}
	for key in keys {
		if value := os.getenv_opt(key) {
			previous[key] = value
			present[key] = true
		}
		os.setenv(key, 'saved_${key}', true)
	}
	defer {
		for key in keys {
			if present[key] {
				os.setenv(key, previous[key], true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	check := check_with_modules_env('restore_env', 'V_CHECK_LIBRARY_BODIES=all', {
		'main.v': header_program
	}, {
		'advlib/advlib.v': library_module
	})
	assert check.res.exit_code == 0, check.res.output
	assert check.trace == ['0 of 2 library instances cloned without their bodies',
		'every library instance keeps its body: V_CHECK_LIBRARY_BODIES=all asks for them'], check.trace.str()
	for key in keys {
		assert os.getenv(key) == 'saved_${key}'
	}
}

fn test_a_library_instance_whose_types_reach_a_program_generic_keeps_its_body() {
	// `Job` holds a `Runner`, which has a method with type parameters of its own.
	check := check_with_library('method_level', method_level_program)
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['6:11: `int` has no property `name`'], check.res.output
	assert check.trace == ['0 of 1 library instances cloned without their bodies'], check.trace.str()
}

fn test_a_library_instance_given_an_interface_or_a_program_generic_type_keeps_its_body() {
	source := "import advlib

struct Dog {}

fn (d Dog) name() string {
	return 'dog'
}

struct Pair[T] {
	a T
}

fn first[T](xs []T) T {
	return xs[0]
}

fn main() {
	println(first([1]))
	println(advlib.with_namer_from_library([advlib.Namer(Dog{})], advlib.Namer(Dog{})))
	println(advlib.with_namer_from_library([Pair[int]{ a: 1 }], advlib.Namer(Dog{})))
}
"
	// Let the library call with_namer so its type argument determines whether its body is kept.
	check := check_with_modules('type_argument_trace', {
		'main.v': source
	}, {
		'advlib/advlib.v': library_module + '
pub fn with_namer_from_library[T](xs []T, n Namer) string {
	return with_namer(xs, n)
}
'
	})
	assert check.res.exit_code == 0, check.res.output
	assert check.trace == ['0 of 4 library instances cloned without their bodies'], check.trace.str()
}

fn test_a_method_of_a_program_generic_type_keeps_the_body_of_every_library_instance() {
	check := check_with_library('dispatch', dispatch_program)
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['8:13: `int` has no property `name`'], check.res.output
	assert check.trace == ['0 of 1 library instances cloned without their bodies',
		'every library instance keeps its body: `Box.name` is a method of a generic type of the program'], check.trace.str()
}

// The instances below are asked for only in the body of a library instance.
// Each check has to report the member that the concrete type lacks.

fn test_calls_in_library_bodies_check_program_instances() {
	check := check_with_library('operator_method_and_interface', 'import advlib

struct Vec[T] {
	x T
}

fn (a Vec[T]) + (b Vec[T]) Vec[T] {
	return Vec[T]{
		x: a.x.name
	}
}

struct Box[T] {
	v T
}

fn (b Box[T]) label() string {
	return b.v.name
}

struct NamedBox[T] {
	v T
}

fn (b NamedBox[T]) name() string {
	return b.v.name
}

interface Named {
	name() string
}

fn main() {
	println(advlib.total([Vec[int]{1}, Vec[int]{2}]).x)
	println(advlib.label_of(Box[int]{ v: 1 }))
	xs := [Named(NamedBox[int]{ v: 1 })]
	println(advlib.names_of(xs))
}
')
	assert check.res.exit_code == 1, check.res.output
	errors := error_lines(check.res.output)
	assert errors.len == 3, check.res.output
	assert '9:10: `int` has no property `name`' in errors, check.res.output
	assert '18:13: `int` has no property `name`' in errors, check.res.output
	assert '26:13: `int` has no property `name`' in errors, check.res.output
}

fn test_a_comptime_for_lowered_after_an_interface_call_in_a_library_body_is_checked() {
	// `Person.name` is lowered because the body of `with_namer` calls `Namer.name`:
	// only then does its `$for` ask for `show[int]`.
	check := check_with_library('comptime_for', "import advlib\n\nfn show[T](x T) string {\n\treturn x.name\n}\n\nstruct Person {\n\tage int\n}\n\nfn (p Person) name() string {\n\tmut s := ''\n\t\$for f in Person.fields {\n\t\ts += show(p.\$(f.name))\n\t}\n\treturn s\n}\n\nfn main() {\n\tprintln(advlib.with_namer([1], advlib.Namer(Person{})))\n}\n")
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['4:11: `int` has no property `name`'], check.res.output
}

fn test_a_library_that_imports_a_module_of_the_program_keeps_every_body() {
	// advlib2 imports the program's own module `progmod`, so its code can ask for
	// `progmod.gen[int]` whatever its type arguments are.
	check := check_with_modules('import', {
		'main.v':            'import advlib2\n\nfn first[T](xs []T) T {\n\treturn xs[0]\n}\n\nfn main() {\n\tprintln(first([1]))\n\tprintln(advlib2.call_gen(1))\n}\n'
		'progmod/progmod.v': 'module progmod\n\npub fn gen[T](x T) string {\n\treturn x.name\n}\n'
	}, {
		'advlib2/advlib2.v': 'module advlib2\n\nimport progmod\n\npub fn call_gen[T](x T) string {\n\treturn progmod.gen(x)\n}\n'
	})
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['4:11: `int` has no property `name`'], check.res.output
	assert check.trace.len == 2, check.trace.str()
	assert check.trace[1].ends_with('imports the module `progmod` of the program'), check.trace.str()
}

// build_program builds `source` with the prelude, with no V1 to fall back on when
// the C compiler fails.
fn build_program(name string, source string, flags ...string) os.Result {
	return build_program_with_env(name, source, [], ...flags)
}

// build_program_with_env is build_program with the environment variables `env`
// (`NAME=value`) set for the build.
fn build_program_with_env(name string, source string, env []string, flags ...string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_build_generic_instances_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude + source) or { panic(err) }
	mut cmd := ['env']
	cmd << env
	cmd << [@VEXE, '-nocolor', ...flags, '-o', os.join_path(dir, 'main'), path]
	return os.exec(cmd)
}

// Both check and build diagnose invalid expressions in a generic body when
// the function is specialized.
fn test_instances_report_member_operator_and_guard_errors() {
	source := "fn show[T](x T) string {
	return x.nme
}

fn add[T](a T) int {
	return a + 'x'
}

fn guard[T](a T) {
	if v := a {
		println(v)
	}
}

fn main() {
	println(show(User{ name: 'a' }))
	println(add(1))
	guard(User{ name: 'a' })
}
"
	for res in [check_program('instance_expr_check', source),
		build_program('instance_expr_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		errors := error_lines(res.output)
		assert errors.len == 3, res.output
		assert errors.any(it.starts_with('11:11: type `User` has no field named `nme`')), res.output
		assert '15:9: operator `+` cannot concatenate `int` and `string`' in errors, res.output
		assert '19:10: expression should either return an Option or a Result' in errors, res.output
	}
}

// Both check and build report incompatible returns when generic functions,
// closures, and methods are specialized. A local initialized from a parameter
// also retains the instance's concrete type.
const return_program = "fn is_set[T](x T) bool {
	return x
}

fn name_flag[T](x T) bool {
	return x.name
}

fn checked[T](mut x T) !int {
	return x.name
}

fn later[T](mut x T) {
	read := fn [mut x] [T] () !int {
		return x.name
	}
	_ = read
}

struct Box[T] {
	v T
}

fn (b Box[T]) flag() bool {
	return b.v
}

fn invalid[T](value T) bool {
	local := value
	return local
}

fn main() {
	println(is_set(1))
	println(name_flag(User{ name: 'a' }))
	mut u := User{
		name: 'b'
	}
	println(checked(mut u) or { 0 })
	later(mut u)
	println(Box[int]{ v: 1 }.flag())
	println(invalid(1))
}
"

const return_errors = ['11:9: cannot use `int` as type `bool` in return argument',
	'15:9: cannot use `string` as type `bool` in return argument',
	'19:9: cannot use `string` as type `!int` in return argument',
	'24:10: cannot use `string` as type `!int` in return argument',
	'34:9: cannot use `int` as type `bool` in return argument',
	'39:9: cannot use `int` as type `bool` in return argument']

fn test_an_instance_that_returns_a_value_of_another_type_is_reported() {
	for res in [check_program('return_check', return_program),
		build_program('return_build', return_program)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == return_errors, res.output
	}
}

// Returns whose types depend on a call, a type parameter literal, or a value
// narrowed by `is` stay valid when the generic functions are specialized.
fn test_instances_return_values_and_local_callbacks() {
	source := "type Value = int | string

fn same[T](x T) T {
	return x
}

fn widen[T](x T) i64 {
	return x
}

fn greeting[T](x T) string {
	f := fn [x] [T] () string {
		return x.greet()
	}
	return f()
}

fn zero[T]() T {
	return T{}
}

fn pick[T](v Value, fallback T) int {
	if v is int {
		return v
	}
	return 0
}

// A local function literal keeps its callable type when returned directly or
// through another local in a specialization.
fn callback[T](x T) fn () !int {
	make := fn () !int {
		return 1
	}
	return make
}

fn forwarded_callback[T](x T) fn () !int {
	make := fn () !int {
		return 2
	}
	forwarded := make
	return forwarded
}

fn main() {
	println(same(1))
	println(same('a'))
	println(widen(i32(2)))
	println(widen(u8(3)))
	println(greeting(User{ name: 'c' }))
	println(zero[map[string]int]())
	println(pick(Value(4), 0))

	f := callback(1)
	g := forwarded_callback('a')
	assert f()! == 1
	assert g()! == 2
}
"
	for res in [check_program('return_valid_check', source),
		build_program('return_valid_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// Check the operator in the active compile-time branch and option guards over
// parameters, map indexes, and smartcast values.
fn test_valid_operators_and_option_guards_in_instances() {
	source := "fn join[T](a T, b T) T {
	return a + b
}

fn inc[T](a T) T {
	\$if T is int {
		return a + 1
	} \$else {
		return a
	}
}

fn plain[T](a T) string {
	if v := a {
		return '\${v}'
	}
	return 'none'
}

fn opt[T](a ?T) string {
	if v := a {
		return '\${v}'
	}
	return 'none'
}

fn mut_opt[T](mut a ?T) string {
	if v := a {
		return '\${v}'
	}
	return 'none'
}

fn index[T](m map[string]T) string {
	if v := m['a'] {
		return '\${v}'
	}
	return 'none'
}

fn smart[T](a ?T) string {
	if a != none {
		if v := a {
			return '\${v}'
		}
	}
	return 'none'
}

fn smart_else[T](mut a ?T) string {
	if a == none {
		return 'none'
	} else {
		if mut v := a {
			return '\${v}'
		}
	}
	return 'none'
}

fn smart_match[T](mut a ?T, n int) string {
	if a != none {
		match n {
			1 {
				if mut v := a {
					return '\${v}'
				}
			}
			else {}
		}
	}
	return 'none'
}

struct Box[T] {
	val ?T
}

fn (b Box[T]) get() string {
	if b.val != none {
		if v := b.val {
			return '\${v}'
		}
	}
	return 'none'
}

fn main() {
	println(join(1, 2))
	println(join('a', 'b'))
	println(inc(1))
	println(inc('s'))

	x := ?int(3)
	println(plain(x))
	println(opt[int](4))
	mut y := ?string('s')
	println(mut_opt(mut y))
	println(index({
		'a': 5
	}))
	println(smart[int](6))
	println(smart_else(mut y))
	println(smart_match(mut y, 1))
	println(Box[int]{ val: 7 }.get())
}
"
	for res in [check_program('operators_and_guards_check', source),
		build_program('operators_and_guards_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// An instance reports its error where the source's `#line` directive maps the
// returned value, as the code generated from another language does.
fn test_an_instance_error_is_reported_at_its_line_directive() {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_generic_instances_line_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'view.txt'), 'first\nsecond\nvalid = count\n') or {
		panic(err)
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude +
		'fn valid[T](x T) bool {\n#line 3 "view.txt"\n\treturn x.name\n}\n\nfn main() {\n\tprintln(valid(User{}))\n}\n') or {
		panic(err)
	}
	res := os.exec([@VEXE, '-check', '-nocolor', path])
	assert res.exit_code == 1, res.output
	assert res.output.contains('view.txt:3:9: error: cannot use `string` as type `bool` in return argument'), res.output
}

// call_arg_program passes names and fields of the wrong type from generic
// bodies: a `bool` to a `string` parameter of a function, an `int` to a method
// and to a fn-typed field, a callback of another signature, and the same from a
// generic closure and a method of a generic struct. Loop values passed directly
// from a parameter, through an array alias, or from an array literal are checked too.
const call_arg_program = "fn take(s string) int {
	return s.len
}

fn on_change(cb fn (string)) {
	cb('a')
}

struct Sink {
	put fn (string) = fn (s string) {}
}

fn (k Sink) show(s string) {
	println(s)
}

struct App {
	flag   bool
	action fn (int) = fn (n int) {}
}

fn label[A](mut app A) int {
	return take(app.flag)
}

fn submit[A](mut app A) {
	on_change(app.action)
}

fn send[T](x T, k Sink) {
	k.show(x)
	k.put(x)
}

fn later[T](x T) {
	f := fn [x] [T] () int {
		return take(x)
	}
	_ = f
}

struct Box[T] {
	v T
}

fn (b Box[T]) send() int {
	return take(b.v)
}

fn take_u8(n u8) {}

fn from_param[T](values []T) {
	for value in values {
		take_u8(value)
	}
}

fn from_alias[T](values []T) {
	alias := values
	for value in alias {
		take_u8(value)
	}
}

fn from_literal[T](value T) {
	for item in [value] {
		take_u8(item)
	}
}

fn main() {
	mut app := App{}
	println(label(mut app))
	submit(mut app)
	send(1, Sink{})
	later(2)
	println(Box[bool]{ v: true }.send())
	from_param([u64(1)])
	from_alias([u64(1)])
	from_literal(u64(1))
}
"

const call_arg_errors = ['32:14: cannot use `bool` as `string` in argument 1 to `take`',
	'36:12: cannot use `fn (int)` as `fn (string)` in argument 1 to `on_change`',
	'40:9: cannot use `int` as `string` in argument 1 to `Sink.show`',
	'41:8: cannot use `int` as `string` in argument 1 to `Sink.put`',
	'46:15: cannot use `int` as `string` in argument 1 to `take`',
	'56:14: cannot use `bool` as `string` in argument 1 to `take`',
	'63:11: cannot use `u64` as `u8` in argument 1 to `take_u8`',
	'70:11: cannot use `u64` as `u8` in argument 1 to `take_u8`',
	'76:11: cannot use `u64` as `u8` in argument 1 to `take_u8`']

// A plain argument of an instance with another type than its parameter is
// reported for that instance, in a check and in a build, at the argument.
fn test_an_instance_that_passes_an_argument_of_another_type_is_reported() {
	for res in [check_program('call_arg_check', call_arg_program),
		build_program('call_arg_build', call_arg_program)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == call_arg_errors, res.output
	}
}

// Arguments of the types their parameters take stay valid, converted or not,
// and so do the arguments that a clone cannot judge from its concrete types: a
// call result, a value narrowed by `is` or `match`, one passed in the body of a
// `$for`, a spread `...args`, and a local initialized by a `match` or a fn
// literal, which a clone can type as an `int` and as the literal's return type.
// Loop values also keep the element type inferred from a `match`.
fn test_instances_that_pass_arguments_of_their_types_stay_valid() {
	source := "type Value = int | string

interface Greeter {
	greet() string
}

struct Named {
	label string
}

fn take(s string) int {
	return s.len
}

fn take_i64(n i64) i64 {
	return n
}

fn speak(g Greeter) string {
	return g.greet()
}

fn store(v Value) int {
	return if v is int { v } else { 0 }
}

fn show_any[U](u U) string {
	return '\${u}'
}

fn grow(mut a []int) {
	a << 1
}

fn names[T](x T) int {
	return take(x.name)
}

fn widen[T](x T) i64 {
	return take_i64(x)
}

fn greet_all[T](x T) string {
	return speak(x)
}

fn wrap[T](x T) int {
	return store(x)
}

fn pass[T](x T) string {
	return show_any(x)
}

fn update[T](mut x T) {
	grow(mut x)
}

fn later[T](x T) int {
	f := fn [x] [T] () int {
		return take(x.name)
	}
	return f()
}

fn via_call[T](x T) int {
	return take(x.greet())
}

fn narrowed[T](v Value, fallback T) int {
	if v is string {
		return take(v)
	}
	return match v {
		string { take(v) }
		int { v }
	}
}

fn take_u8(n u8) u8 {
	return n
}

fn pair(a int, b int) int {
	return a + b
}

fn run(f fn () !int) int {
	return f() or { 0 }
}

fn sized[T](x T) u8 {
	len := match typeof(x).name {
		'u8' { u8(2) }
		else { 4 }
	}
	return take_u8(len)
}

fn check_loop_lengths[T](x T) {
	lengths := [match typeof(x).name {
		'u8' { u8(2) }
		else { 4 }
	}]
	for length in lengths {
		assert take_u8(length) == 2
	}
	aliases := lengths
	for _, length in aliases {
		copy := length
		assert take_u8(copy) == 2
	}
}

fn spread[T](args ...T) int {
	return pair(...args)
}

fn callback[T](x T) int {
	make := fn () !int {
		return 1
	}
	return run(make)
}

fn fields[T](x T, v Value) int {
	mut n := 0
	\$for f in T.fields {
		\$if f.typ is string {
			if v is string {
				n += take(v)
			}
		}
	}
	return n
}

fn main() {
	u := User{
		name: 'a'
	}
	println(names(u))
	println(widen(i32(2)))
	println(widen(u8(3)))
	println(greet_all(u))
	println(wrap(4))
	println(wrap('c'))
	println(pass(5))
	println(pass(u))
	mut nums := [1]
	update(mut nums)
	println(later(u))
	println(via_call(u))
	println(narrowed(Value('d'), 0))
	println(fields(Named{ label: 'e' }, Value('f')))
	println(sized(1))
	check_loop_lengths(u8(1))
	println(spread(1, 2))
	println(callback(u))
}
"
	for res in [check_program('call_arg_valid_check', source),
		build_program('call_arg_valid_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// The layout fixtures specialize arrays and maps of `T` with the element types
// the body expects and with elements of a different size.
const layout_declarations = 'type Counts8 = map[string]u8

struct Wide {
	vals []u64
}

fn to_bytes[T](a []T) []u8 {
	return []u8(a)
}

fn widen[T](a []T) []u64 {
	return a
}

fn assign[T](a []T) []u64 {
	mut b := []u64{}
	b = a.clone()
	return b
}

fn wrap[T](a []T) Wide {
	return Wide{
		vals: a
	}
}

fn sum(a []i64) i64 {
	return a.len
}

fn total[T](a []T) i64 {
	return sum(a)
}

fn narrow_map[T](m map[string]T) Counts8 {
	return Counts8(m)
}

fn widen_map[T](m map[string]T) map[string]u64 {
	return m
}

fn map_total(m map[string]i64) i64 {
	return m.len
}

fn total_map[T](m map[string]T) i64 {
	return map_total(m)
}

'

const layout_calls = "	println(to_bytes([u8(1)]))
	println(widen([u64(1)]))
	println(assign([u64(1)]))
	println(wrap([u64(1)]))
	println(total([i64(1)]))
	println(narrow_map({
		'a': u8(1)
	}))
	println(widen_map({
		'a': u64(1)
	}))
	println(total_map({
		'a': i64(1)
	}))
"

const layout_program = layout_declarations + 'fn main() {\n' + layout_calls

const bad_layout_calls = "	println(to_bytes([1, 2]))
	println(to_bytes([i32(1)]))
	println(widen([u8(1)]))
	println(assign([u32(1)]))
	println(wrap([u8(1)]))
	println(total([i32(1)]))
	println(narrow_map({
		'a': 1
	}))
	println(widen_map({
		'a': u8(1)
	}))
	println(total_map({
		'a': i32(1)
	}))
"

const layout_errors = ['17:9: cannot cast array `[]int` to `[]u8`',
	'17:9: cannot cast array `[]i32` to `[]u8`',
	'21:9: cannot use `[]u8` as type `[]u64` in return argument',
	'26:6: cannot assign to `b`: expected `[]u64`, not `[]u32`',
	'32:3: cannot assign to field `vals`: expected `[]u64`, not `[]u8`',
	'41:13: cannot use `[]i32` as `[]i64` in argument 1 to `sum`',
	'45:9: cannot cast `map[string]int` to `Counts8` (alias to `map[string]u8`)',
	'49:9: cannot use `map[string]u8` as type `map[string]u64` in return argument',
	'57:19: cannot use `map[string]i32` as `map[string]i64` in argument 1 to `map_total`']

const wrapped_layout_declarations = 'fn opt[T](a ?[]T) ?[]u64 {
	return a
}

fn res[T](a []T) ![]u64 {
	return a
}

fn fixed[T](a [3]T) [3]u64 {
	return a
}

fn nested[T](a [][]T) [][]u64 {
	return a
}

fn take_mut(mut a []u64) {
	a << 1
}

fn grow[T](mut a []T) {
	take_mut(mut a)
}

fn append[T](a []T) [][]u64 {
	mut out := [][]u64{}
	out << a
	return out
}

fn speakers[T](a []T) []Speaker {
	return a
}

interface Speaker {
	speak() string
}

fn (u User) speak() string {
	return u.name
}

'

const wrapped_layout_calls = '	println(opt[u64]([u64(1)]))
	println(res([u64(1)]) or { [] })
	println(fixed([u64(1), 2, 3]!))
	println(nested([[u64(1)]]))
	mut wide := [u64(1)]
	grow(mut wide)
	println(append([u64(1)]))
	println(speakers([User{}]).len)
'

// Check and build report each element-type mismatch at its use in the
// generic body, including wrapped arrays, mutable arguments, and appends.
fn test_an_instance_that_uses_an_array_or_map_with_other_elements_is_reported() {
	bad_wrapped_layout_calls := '	println(opt[u8]([u8(1)]))
	println(res([u8(1)]) or { [] })
	println(fixed([u8(1), 2, 3]!))
	println(nested([[u8(1)]]))
	mut narrow := [u8(1)]
	grow(mut narrow)
	println(append([u8(1)]))
'
	source := layout_declarations + wrapped_layout_declarations + container_types +
		container_conversions + container_nest_and_count + 'fn main() {\n' + layout_calls +
		bad_layout_calls + wrapped_layout_calls + bad_wrapped_layout_calls +
		container_type_calls + bad_container_type_calls + '}\n'
	wrapped_errors := [
		'61:9: cannot use `?[]u8` as type `?[]u64` in return argument',
		'65:9: cannot use `[]u8` as type `![]u64` in return argument',
		'69:9: cannot use `[3]u8` as type `[3]u64` in return argument',
		'73:9: cannot use `[][]u8` as type `[][]u64` in return argument',
		'81:15: cannot use `&[]u8` as `&[]u64` in argument 1 to `take_mut`',
		'86:9: cannot append `[]u8` to `[][]u64`',
	]
	container_errors := container_errors_at(['111:9', '115:9', '123:18', '26:6', '32:3', '128:9',
		'133:9'])
	// The fixture reports more errors than the default diagnostic limit.
	flags := ['-message-limit', '100']
	for res in [check_program('layout_check', source, ...flags),
		build_program('layout_build', source, ...flags)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		errors := error_lines(res.output)
		// The compiler can interleave the groups. Check the order within each group.
		assert errors.len == layout_errors.len + wrapped_errors.len + container_errors.len, res.output
		assert errors.filter(it in layout_errors) == layout_errors, res.output
		assert errors.filter(it in wrapped_errors) == wrapped_errors, res.output
		assert errors.filter(it in container_errors) == container_errors, res.output
	}
}

// Arrays and maps with the expected elements stay valid, including wrapped
// arrays. An array can also be converted element by element to an interface array.
fn test_instances_with_the_expected_elements_stay_valid() {
	source := layout_declarations + wrapped_layout_declarations + container_types +
		container_conversions + container_nest_and_count + 'fn main() {\n' +
		layout_calls + wrapped_layout_calls + container_type_calls + '}\n'
	for res in [check_program('layout_valid_check', source),
		build_program('layout_valid_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// A build that cannot fork, as on Windows, checks the instances of its generics
// in a compiler process of its own (see start_program_instance_check_process).
// V_CHECK_INSTANCES_IN_BUILD=process makes any build do so, and it reports what
// a check reports, once, and still builds a valid program.
fn test_a_build_checks_the_instances_in_a_compiler_process_where_it_cannot_fork() {
	env := ['V_CHECK_INSTANCES_IN_BUILD=process']
	source := layout_declarations + 'fn show[T](x T) string {
	return x.nme
}

' + container_types + container_conversions + container_nest_and_count + 'fn main() {
	mut unused := 1
' + layout_calls + bad_layout_calls + container_type_calls + bad_container_type_calls +
		"	println(show(User{ name: 'a' }))
}
"
	res := build_program_with_env('invalid_build_process', source, env)
	assert res.exit_code != 0, res.output
	assert !res.output.contains('C compilation error'), res.output
	// The warning of the build is printed once, by the build.
	assert res.output.count('unused variable: `unused`') == 1, res.output
	// The assignment and field errors use the shared layout declarations.
	container_errors := container_errors_at(['73:9', '77:9', '85:18', '26:6', '32:3', '90:9', '95:9'])
	errors := error_lines(res.output)
	assert errors.len == layout_errors.len + container_errors.len + 1, res.output
	assert errors.filter(it in layout_errors) == layout_errors, res.output
	assert errors.filter(it in container_errors) == container_errors, res.output
	assert errors.any(it.starts_with('61:11: type `User` has no field named `nme`')), res.output
	valid := build_program_with_env('layout_valid_build_process', layout_program + '}\n', env)
	assert valid.exit_code == 0, valid.output
	assert error_lines(valid.output) == [], valid.output
}

const container_types = 'struct Foo {
	a int
}

struct Bar {
	b string
}

'

const container_conversions = 'fn as_u64[T](a []T) []u64 {
	return a
}

fn as_bars[T](a []T) []Bar {
	return a
}

fn take_u64(a []u64) int {
	return a.len
}

fn pass[T](a []T) int {
	return take_u64(a)
}

'

const container_nest_and_count = 'fn nest[T](a []T) [][]u64 {
	mut out := [][]u64{}
	out << a
	return out
}

fn counts[T](m map[string]T) map[string]int {
	return m
}

'

const container_type_calls = '	println(as_u64([u64(1)]))
	println(as_bars([Bar{}]))
	println(pass([u64(1)]))
	println(assign([u64(1)]))
	println(wrap([u64(1)]))
	println(nest([u64(1)]))
	println(counts({
		"a": 1
	}))
'

const bad_container_type_calls = "	println(as_u64(['a']))
	println(as_bars([Foo{}]))
	println(pass(['x']))
	println(assign(['y']))
	println(wrap(['z']))
	println(nest(['w']))
	println(counts({
		'a': 'b'
	}))
"

const container_type_error_messages = [
	'cannot use `[]string` as type `[]u64` in return argument',
	'cannot use `[]Foo` as type `[]Bar` in return argument',
	'cannot use `[]string` as `[]u64` in argument 1 to `take_u64`',
	'cannot assign to `b`: expected `[]u64`, not `[]string`',
	'cannot assign to field `vals`: expected `[]u64`, not `[]string`',
	'cannot append `[]string` to `[][]u64`',
	'cannot use `map[string]string` as type `map[string]int` in return argument',
]

fn container_errors_at(positions []string) []string {
	mut errors := []string{cap: container_type_error_messages.len}
	for i, message in container_type_error_messages {
		errors << '${positions[i]}: ${message}'
	}
	return errors
}

// The check of an instance does not see the locals declared in a `$for` body,
// and a name there can then resolve to a constant of the main module, an
// `[]string` here. Such a body gives no error about the elements of an array or
// a map.
fn test_a_local_of_a_comptime_for_body_is_not_taken_for_a_constant() {
	check := check_with_modules('comptime_for_local', {
		'main.v':            "import progmod\n\nconst routes = ['/', '/x']\n\nstruct Foo {\n\ta int\n}\n\nfn main() {\n\tprintln(routes)\n\tprintln(progmod.first_routes(Foo{}).routes)\n}\n"
		'progmod/progmod.v': "module progmod\n\npub struct Holder {\npub:\n\troutes &map[string]int\n}\n\nfn make_routes() map[string]int {\n\treturn {\n\t\t'a': 1\n\t}\n}\n\npub fn first_routes[T](x T) Holder {\n\t\$for field in T.fields {\n\t\troutes := make_routes()\n\t\treturn Holder{\n\t\t\troutes: &routes\n\t\t}\n\t}\n\treturn Holder{\n\t\troutes: unsafe { nil }\n\t}\n}\n"
	}, {})
	assert check.res.exit_code == 0, check.res.output
	assert error_lines(check.res.output) == [], check.res.output
}

const map_cast_comptime_for_program = "fn to_counts[T](m map[string]T) map[string]u8 {
	return map[string]u8(m)
}

struct Foo {
	a int
}

struct Bar {
	b int
}

fn take_bars(items []Bar) int {
	return items.len
}

fn wrap[T](x T) []Bar {
	\$for field in T.fields {
		items := [x]
		return items
	}
	return []
}

fn count[T](x T) int {
	\$for field in T.fields {
		items := [x]
		return take_bars(items)
	}
	return 0
}

fn main() {
	println(to_counts({
		'a': u8(7)
	}))
	println(wrap(Bar{ b: 1 }))
	println(count(Bar{ b: 1 }))
"

// A map cast preserves element types. A local array in a compile-time loop
// has the element type of its specialization, in both returns and arguments.
fn test_map_cast_and_comptime_for_instances_reject_wrong_element_types() {
	source := map_cast_comptime_for_program + "	println(to_counts({
		'a': 258
	}))
	println(wrap(Foo{ a: 2 }))
	println(count(Foo{ a: 2 }))
}
"
	for res in [check_program('map_cast_comptime_for_check', source),
		build_program('map_cast_comptime_for_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == [
			'11:9: cannot cast map `map[string]int` to `map[string]u8`',
			'29:10: cannot use `[]Foo` as type `[]Bar` in return argument',
			'37:20: cannot use `[]Foo` as `[]Bar` in argument 1 to `take_bars`',
		], res.output
	}
}

// A map cast to its own type and compile-time loops with the expected types build and run.
fn test_map_cast_and_comptime_for_instances_build_and_run() {
	source := map_cast_comptime_for_program + "	println(routes)
	println(first_routes(Foo{}))
}

const routes = ['/', '/x']

struct Holder {
	routes &map[string]int
}

fn make_routes() map[string]int {
	return {
		'a': 1
	}
}

// The local map shadows the module constant inside the compile-time loop.
fn first_routes[T](x T) int {
	\$for field in T.fields {
		routes := make_routes()
		h := Holder{
			routes: &routes
		}
		return h.routes.len
	}
	return 0
}
"
	check := check_program('map_cast_comptime_for_valid_check', source)
	assert check.exit_code == 0, check.output
	assert error_lines(check.output) == [], check.output
	dir := os.join_path(os.vtmp_dir(), 'v3_run_generic_instances_map_cast_comptime_for_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude + source) or { panic(err) }
	res := os.exec(['env', @VEXE, '-nocolor', 'run', path])
	assert res.exit_code == 0, res.output
	assert res.output.split_into_lines() == ["{'a': 7}", '[Bar{', '    b: 1', '}]', '1', "['/', '/x']",
		'1'], res.output
}

// A library module outside the program whose generic functions are wrong for
// some type arguments.
const checked_library_module = 'module chklib

pub fn to_bytes[T](a []T) []u8 {
	return []u8(a)
}

pub fn label_of[T](x T) string {
	return x.label()
}

pub fn widen[T](a []T) []u64 {
	return a
}

fn inner[T](a []T) []u8 {
	return []u8(a)
}

// outer asks for `inner[int]` whatever `T` is.
pub fn outer[T](x T) int {
	return inner([1, 2]).len
}
'

const checked_library_program = "import chklib

struct User {
	name string
}

fn (u User) label() string {
	return u.name
}

fn main() {
	println(chklib.to_bytes([u8(1)]))
	println(chklib.label_of(User{}))
	println(chklib.widen([u64(1)]))
	println(chklib.outer('x'))
"

// build_with_modules builds the program `files` with the library `modules`
// outside it, as check_with_modules checks it.
fn build_with_modules(name string, files map[string]string, modules map[string]string) os.Result {
	return build_with_modules_env(name, files, modules, [])
}

// build_with_modules_env builds with the additional environment entries `env`.
fn build_with_modules_env(name string, files map[string]string, modules map[string]string, env []string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_build_generic_library_${name}_${os.getpid()}')
	modules_dir := os.join_path(dir, 'modules')
	program := os.join_path(dir, 'program')
	defer {
		os.rmdir_all(dir) or {}
	}
	for root, contents in {
		modules_dir: modules
		program:     files
	} {
		for relative, content in contents {
			path := os.join_path(root, relative)
			os.mkdir_all(os.dir(path)) or { panic(err) }
			os.write_file(path, content) or { panic(err) }
		}
	}
	mut cmd := ['env', 'VMODULES=${modules_dir}']
	cmd << env
	cmd << [@VEXE, '-nocolor', '-o', os.join_path(dir, 'main'), os.join_path(program, 'main.v')]
	return os.exec(cmd)
}

// An instance of a library generic that the program's own code asks for is
// checked as an instance of the program's generics is, and reported in the
// library's body, as V 0.5.2 does, also when the program declares no generic
// function itself: in a check, and in a Unix build, whether the instance check
// uses a fork or a fresh musl process. An instance that the library asks for,
// `inner[int]`, is the library's and is not checked.
fn test_an_instance_of_a_library_generic_that_the_program_asks_for_is_checked() {
	files := {
		'main.v': checked_library_program +
			"\tprintln(chklib.to_bytes([1, 2]))\n\tprintln(chklib.label_of(1))\n\tprintln(chklib.widen(['a']))\n}\n"
	}
	modules := {
		'chklib/chklib.v': checked_library_module
	}
	mut results := [check_with_modules('checked_library', files, modules).res]
	process := build_with_modules_env('checked_library_process', files, modules, [
		'V3_INTERNAL_INSTANCE_CHECK=1',
		'V3_INTERNAL_INSTANCE_CHECK_LIBRARY_INSTANCES=1',
	])
	assert process.output.contains('\x1ev3 instance check report\x1e'), process.output
	results << process
	$if !windows {
		results << build_with_modules('checked_library_build', files, modules)
	}
	for res in results {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == [
			'4:9: cannot cast array `[]int` to `[]u8`',
			'8:11: unknown method or field: `int.label`.',
			'12:9: cannot use `[]string` as type `[]u64` in return argument',
		], res.output
		assert res.output.contains('chklib.v:4:9: error:'), res.output
	}
}

// The instances that the program asks for with the types the library's bodies
// expect stay valid.
fn test_instances_of_a_library_generic_with_the_expected_types_stay_valid() {
	files := {
		'main.v': checked_library_program + '}\n'
	}
	modules := {
		'chklib/chklib.v': checked_library_module
	}
	check := check_with_modules('checked_library_valid', files, modules)
	process := build_with_modules_env('checked_library_valid_process', files, modules, [
		'V3_INTERNAL_INSTANCE_CHECK=1',
		'V3_INTERNAL_INSTANCE_CHECK_LIBRARY_INSTANCES=1',
	])
	assert process.output.contains('\x1ev3 instance check report\x1e'), process.output
	for res in [check.res, process, build_with_modules('checked_library_valid_build', files, modules)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}
