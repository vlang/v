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
	return os.exec([@VEXE, '-check', '-nocolor', path])
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

fn main() {
	println(show(1))
	println(misspelled(User{ name: 'a' }))
	println(call(User{ name: 'a' }))
	println(call(1))
	println(Box[User]{ v: User{ name: 'a' } }.label())
	println(Box[int]{ v: 1 }.label())
}
")
	assert res.exit_code == 1, res.output
	errors := error_lines(res.output)
	assert errors.len == 4, res.output
	assert '11:11: `int` has no property `name`' in errors, res.output
	assert errors.any(it.starts_with('15:11: type `User` has no field named `nme`')), res.output
	assert '19:11: unknown method or field: `int.greet`.' in errors, res.output
	assert '27:13: `int` has no property `name`' in errors, res.output
}

fn test_instances_that_have_what_the_body_uses_stay_valid() {
	res := check_program('valid', "fn show[T](x T) string {\n\treturn x.name\n}\n\nfn call[T](x T) string {\n\treturn x.greet()\n}\n\nfn main() {\n\tprintln(show(User{ name: 'a' }))\n\tprintln(call(User{ name: 'b' }))\n}\n")
	assert res.exit_code == 0, res.output
	assert error_lines(res.output) == [], res.output
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

const operator_program = 'import advlib

struct Vec[T] {
	x T
}

fn (a Vec[T]) + (b Vec[T]) Vec[T] {
	return Vec[T]{
		x: a.x.name
	}
}

fn main() {
	println(advlib.total([Vec[int]{1}, Vec[int]{2}]).x)
}
'

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

fn test_v_check_library_bodies_keeps_the_body_of_every_library_instance() {
	check := check_with_modules_env('bodies_env', 'V_CHECK_LIBRARY_BODIES=all', {
		'main.v': header_program
	}, {
		'advlib/advlib.v': library_module
	})
	assert check.res.exit_code == 0, check.res.output
	assert check.trace == ['0 of 2 library instances cloned without their bodies',
		'every library instance keeps its body: V_CHECK_LIBRARY_BODIES=all asks for them'], check.trace.str()
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

fn test_library_check_restores_existing_environment_values() {
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
	for key in keys {
		assert os.getenv(key) == 'saved_${key}'
	}
}

fn test_a_library_instance_whose_types_reach_a_program_generic_keeps_its_body() {
	// `Job` holds a `Runner`, which has a method with type parameters of its own.
	check := check_with_library('method_level_trace', method_level_program)
	assert check.trace == ['0 of 1 library instances cloned without their bodies'], check.trace.str()
}

fn test_a_library_instance_given_an_interface_or_a_program_generic_type_keeps_its_body() {
	prelude := "import advlib\n\nstruct Dog {}\n\nfn (d Dog) name() string {\n\treturn 'dog'\n}\n\nstruct Pair[T] {\n\ta T\n}\n\nfn first[T](xs []T) T {\n\treturn xs[0]\n}\n\n"
	iface := check_with_library('interface_trace', prelude + 'fn main() {\n\tprintln(first([1]))\n\tprintln(advlib.with_namer([advlib.Namer(Dog{})], advlib.Namer(Dog{})))\n}\n')
	assert iface.res.exit_code == 0, iface.res.output
	assert iface.trace == ['0 of 1 library instances cloned without their bodies'], iface.trace.str()
	pair := check_with_library('generic_type_trace', prelude + 'fn main() {\n\tprintln(first([1]))\n\tprintln(advlib.with_namer([Pair[int]{ a: 1 }], advlib.Namer(Dog{})))\n}\n')
	assert pair.res.exit_code == 0, pair.res.output
	assert pair.trace == ['0 of 1 library instances cloned without their bodies'], pair.trace.str()
}

fn test_a_method_of_a_program_generic_type_keeps_the_body_of_every_library_instance() {
	check := check_with_library('dispatch_trace', dispatch_program)
	assert check.trace == ['0 of 1 library instances cloned without their bodies',
		'every library instance keeps its body: `Box.name` is a method of a generic type of the program'], check.trace.str()
}

// The instances below are asked for only in the body of a library instance.
// Each check has to report the member that the concrete type lacks.

fn test_an_operator_applied_in_a_library_body_checks_the_program_instance() {
	check := check_with_library('operator', operator_program)
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['9:10: `int` has no property `name`'], check.res.output
}

fn test_a_method_called_in_a_library_body_checks_the_program_instance() {
	check := check_with_library('method', 'import advlib\n\nstruct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) label() string {\n\treturn b.v.name\n}\n\nfn main() {\n\tprintln(advlib.label_of(Box[int]{ v: 1 }))\n}\n')
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['8:13: `int` has no property `name`'], check.res.output
}

fn test_an_interface_call_in_a_library_body_checks_the_program_instance_it_reaches() {
	check := check_with_library('dispatch', dispatch_program)
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['8:13: `int` has no property `name`'], check.res.output
}

fn test_an_interface_type_argument_checks_the_program_instance_its_call_reaches() {
	check := check_with_library('interface_arg', 'import advlib\n\nstruct Box[T] {\n\tv T\n}\n\nfn (b Box[T]) name() string {\n\treturn b.v.name\n}\n\ninterface Named {\n\tname() string\n}\n\nfn main() {\n\txs := [Named(Box[int]{ v: 1 })]\n\tprintln(advlib.names_of(xs))\n}\n')
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['8:13: `int` has no property `name`'], check.res.output
}

fn test_a_method_with_its_own_type_parameters_reached_through_a_field_is_checked() {
	check := check_with_library('method_level', method_level_program)
	assert check.res.exit_code == 1, check.res.output
	assert error_lines(check.res.output) == ['6:11: `int` has no property `name`'], check.res.output
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
fn build_program(name string, source string) os.Result {
	return build_program_with_env(name, source, [])
}

// build_program_with_env is build_program with the environment variables `env`
// (`NAME=value`) set for the build.
fn build_program_with_env(name string, source string, env []string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_build_generic_instances_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude + source) or { panic(err) }
	mut cmd := ['env']
	cmd << env
	cmd << [@VEXE, '-nocolor', '-o', os.join_path(dir, 'main'), path]
	return os.exec(cmd)
}

// A build reports the errors of an instance where a check does, as V1 did: the
// body of a generic function without constraints is still not checked on its
// own, but each of its instances is, instead of failing in the C compiler.
fn test_a_build_reports_the_member_an_instance_lacks() {
	source := "fn show[T](x T) string {\n\treturn x.nme\n}\n\nfn main() {\n\tprintln(show(User{ name: 'a' }))\n}\n"
	res := build_program('build_struct_typo', source)
	assert res.exit_code != 0, res.output
	assert !res.output.contains('C compilation error'), res.output
	errors := error_lines(res.output)
	assert errors.len == 1, res.output
	assert errors[0].starts_with('11:11: type `User` has no field named `nme`'), res.output
}

// An operator that the type of an instance does not have is reported for that
// instance too, in a check and in a build: `int + string`.
fn test_an_operator_an_instance_lacks_is_reported() {
	source := "fn add[T](a T) int {\n\treturn a + 'x'\n}\n\nfn main() {\n\tprintln(add(1))\n}\n"
	for res in [check_program('operator_check', source), build_program('operator_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == ['11:9: operator `+` cannot concatenate `int` and `string`'], res.output
	}
}

// An operator that every instance has, and one in a `$if` branch that an
// instance does not take, give no error.
fn test_operators_that_each_instance_has_stay_valid() {
	source := "fn join[T](a T, b T) T {\n\treturn a + b\n}\n\nfn inc[T](a T) T {\n\t\$if T is int {\n\t\treturn a + 1\n\t} \$else {\n\t\treturn a\n\t}\n}\n\nfn main() {\n\tprintln(join(1, 2))\n\tprintln(join('a', 'b'))\n\tprintln(inc(1))\n\tprintln(inc('s'))\n}\n"
	for res in [check_program('operators_valid_check', source),
		build_program('operators_valid_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
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

// An `if v := a` guard over an `a T` whose instance is neither an option nor a
// result is reported for that instance, in a check and in a build, as V1 did,
// instead of failing in the C compiler.
fn test_an_if_guard_over_a_plain_instance_is_reported() {
	source := "fn show[T](a T) {\n\tif v := a {\n\t\tprintln(v)\n\t}\n}\n\nfn main() {\n\tshow(User{ name: 'a' })\n}\n"
	for res in [check_program('if_guard_check', source), build_program('if_guard_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == [
			'11:10: expression should either return an Option or a Result',
		], res.output
	}
}

// Guards over instances that are options, over `?T` and `mut ?T` parameters, over
// a map index, and over options smartcast to their payload (`if a != none`, the
// `else` of `if a == none`, a `match` arm, a `?T` field) give no error.
fn test_if_guards_over_options_in_instances_stay_valid() {
	source := "fn plain[T](a T) string {\n\tif v := a {\n\t\treturn '\${v}'\n\t}\n\treturn 'none'\n}\n\nfn opt[T](a ?T) string {\n\tif v := a {\n\t\treturn '\${v}'\n\t}\n\treturn 'none'\n}\n\nfn mut_opt[T](mut a ?T) string {\n\tif v := a {\n\t\treturn '\${v}'\n\t}\n\treturn 'none'\n}\n\nfn index[T](m map[string]T) string {\n\tif v := m['a'] {\n\t\treturn '\${v}'\n\t}\n\treturn 'none'\n}\n\nfn smart[T](a ?T) string {\n\tif a != none {\n\t\tif v := a {\n\t\t\treturn '\${v}'\n\t\t}\n\t}\n\treturn 'none'\n}\n\nfn smart_else[T](mut a ?T) string {\n\tif a == none {\n\t\treturn 'none'\n\t} else {\n\t\tif mut v := a {\n\t\t\treturn '\${v}'\n\t\t}\n\t}\n\treturn 'none'\n}\n\nfn smart_match[T](mut a ?T, n int) string {\n\tif a != none {\n\t\tmatch n {\n\t\t\t1 {\n\t\t\t\tif mut v := a {\n\t\t\t\t\treturn '\${v}'\n\t\t\t\t}\n\t\t\t}\n\t\t\telse {}\n\t\t}\n\t}\n\treturn 'none'\n}\n\nstruct Box[T] {\n\tval ?T\n}\n\nfn (b Box[T]) get() string {\n\tif b.val != none {\n\t\tif v := b.val {\n\t\t\treturn '\${v}'\n\t\t}\n\t}\n\treturn 'none'\n}\n\nfn main() {\n\tx := ?int(3)\n\tprintln(plain(x))\n\tprintln(opt[int](4))\n\tmut y := ?string('s')\n\tprintln(mut_opt(mut y))\n\tprintln(index({\n\t\t'a': 5\n\t}))\n\tprintln(smart[int](6))\n\tprintln(smart_else(mut y))\n\tprintln(smart_match(mut y, 1))\n\tprintln(Box[int]{ val: 7 }.get())\n}\n"
	for res in [check_program('if_guard_valid_check', source),
		build_program('if_guard_valid_build', source)] {
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

// A type argument that is an alias with methods of its own, `Octet` for `T`: the
// clone of `pack` spells its base type, `string`, where the alias's method is
// the one `el.val.pack()` calls, and the instance is valid in a check and in a
// build.
fn test_an_instance_of_an_alias_has_the_methods_of_the_alias() {
	source := "type Octet = string\n\nfn (o Octet) pack() []u8 {\n\treturn o.bytes()\n}\n\nstruct Elm[T] {\n\tval T\n}\n\nfn (el Elm[T]) pack() []u8 {\n\treturn el.val.pack()\n}\n\nfn main() {\n\tel := Elm[Octet]{\n\t\tval: Octet('xx')\n\t}\n\tprintln(el.pack())\n}\n"
	for res in [check_program('alias_methods_check', source),
		build_program('alias_methods_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
}

// The alias does not lend its base type a method it lacks: `Octet` has no
// `size`, and `string` has none either.
fn test_an_instance_of_an_alias_still_lacks_what_neither_has() {
	res := check_program('alias_lacks', "type Octet = string\n\nstruct Elm[T] {\n\tval T\n}\n\nfn (el Elm[T]) size() int {\n\treturn el.val.size()\n}\n\nfn main() {\n\tel := Elm[Octet]{\n\t\tval: Octet('xx')\n\t}\n\tprintln(el.size())\n}\n")
	assert res.exit_code == 1, res.output
	errors := error_lines(res.output)
	assert errors.len == 1, res.output
	assert errors[0].contains('size'), res.output
}

// layout_program uses arrays and maps of `T` where arrays and maps of fixed
// element types are expected, and is the program of the layout tests below. Each
// generic function has an instance with the elements it expects, and the ones
// below have one with other elements too, which read them with the wrong size.
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

const bad_layout_program = layout_program + bad_layout_calls + '}\n'

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
	source := layout_declarations + wrapped_layout_declarations + 'fn main() {\n' + layout_calls +
		bad_layout_calls + wrapped_layout_calls + bad_wrapped_layout_calls + '}\n'
	wrapped_errors := [
		'61:9: cannot use `?[]u8` as type `?[]u64` in return argument',
		'65:9: cannot use `[]u8` as type `![]u64` in return argument',
		'69:9: cannot use `[3]u8` as type `[3]u64` in return argument',
		'73:9: cannot use `[][]u8` as type `[][]u64` in return argument',
		'81:15: cannot use `&[]u8` as `&[]u64` in argument 1 to `take_mut`',
		'86:9: cannot append `[]u8` to `[][]u64`',
	]
	for res in [check_program('layout_check', source), build_program('layout_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		errors := error_lines(res.output)
		// The compiler can interleave the groups. Check the order within each group.
		assert errors.len == layout_errors.len + wrapped_errors.len, res.output
		assert errors.filter(it in layout_errors) == layout_errors, res.output
		assert errors.filter(it in wrapped_errors) == wrapped_errors, res.output
	}
}

// Arrays and maps with the expected elements stay valid, including wrapped
// arrays. An array can also be converted element by element to an interface array.
fn test_instances_with_the_expected_elements_stay_valid() {
	source := layout_declarations + wrapped_layout_declarations + 'fn main() {\n' + layout_calls +
		wrapped_layout_calls + '}\n'
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
	res := build_program_with_env('layout_build_process', bad_layout_program, env)
	assert res.exit_code != 0, res.output
	assert !res.output.contains('C compilation error'), res.output
	assert error_lines(res.output) == layout_errors, res.output
	member := build_program_with_env('member_build_process', "fn show[T](x T) string {\n\treturn x.nme\n}\n\nfn main() {\n\tmut unused := 1\n\tprintln(show(User{ name: 'a' }))\n}\n",
		env)
	assert member.exit_code != 0, member.output
	// The warning of the build is printed once, by the build.
	assert member.output.count('unused variable: `unused`') == 1, member.output
	errors := error_lines(member.output)
	assert errors.len == 1, member.output
	assert errors[0].starts_with('11:11: type `User` has no field named `nme`'), member.output
	valid := build_program_with_env('layout_valid_build_process', layout_program + '}\n', env)
	assert valid.exit_code == 0, valid.output
	assert error_lines(valid.output) == [], valid.output
}

// container_type_program uses arrays and maps of `T` where ones of other element
// types are expected. Each generic function has an instance with the elements
// it expects.
const container_type_program = 'struct Foo {
	a int
}

struct Bar {
	b string
}

struct Wide {
	vals []u64
}

fn as_u64[T](a []T) []u64 {
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

fn nest[T](a []T) [][]u64 {
	mut out := [][]u64{}
	out << a
	return out
}

fn counts[T](m map[string]T) map[string]int {
	return m
}

fn main() {
	println(as_u64([u64(1)]))
	println(as_bars([Bar{}]))
	println(pass([u64(1)]))
	println(assign([u64(1)]))
	println(wrap([u64(1)]))
	println(nest([u64(1)]))
	println(counts({
		"a": 1
	}))
'

// An instance that uses an array or a map as one whose elements have another
// type, which its elements cannot be used as at all (`[]string` as `[]u64`,
// `[]Foo` as `[]Bar`), is reported where the body does so, once for that
// instance, with the wording of the same error outside a generic function, which
// is V 0.5.2's. A check and a build report the same; the build used to report
// only a return, without its position.
fn test_an_instance_that_uses_an_array_or_map_of_another_type_is_reported() {
	source := container_type_program + "\tprintln(as_u64(['a']))\n\tprintln(as_bars([Foo{}]))\n\tprintln(pass(['x']))\n\tprintln(assign(['y']))\n\tprintln(wrap(['z']))\n\tprintln(nest(['w']))\n\tprintln(counts({\n\t\t'a': 'b'\n\t}))\n}\n"
	for res in [check_program('container_type_check', source),
		build_program('container_type_build', source), build_program_with_env('container_type_build_process', source, [
			'V_CHECK_INSTANCES_IN_BUILD=process',
		])] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == [
			'23:9: cannot use `[]string` as type `[]u64` in return argument',
			'27:9: cannot use `[]Foo` as type `[]Bar` in return argument',
			'35:18: cannot use `[]string` as `[]u64` in argument 1 to `take_u64`',
			'40:6: cannot assign to `b`: expected `[]u64`, not `[]string`',
			'46:3: cannot assign to field `vals`: expected `[]u64`, not `[]string`',
			'52:9: cannot append `[]string` to `[][]u64`',
			'57:9: cannot use `map[string]string` as type `map[string]int` in return argument',
		], res.output
	}
}

// The instances with the expected elements stay valid.
fn test_instances_with_arrays_and_maps_of_the_expected_type_stay_valid() {
	source := container_type_program + '}\n'
	for res in [check_program('container_type_valid_check', source),
		build_program('container_type_valid_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
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

const map_cast_declarations = 'fn to_counts[T](m map[string]T) map[string]u8 {
	return map[string]u8(m)
}

'

const map_cast_calls = "	println(to_counts({
		'a': u8(7)
	}))
"

// `map[K]V(m)` is a cast, as `?map[K]V(m)` is, and it keeps the elements as
// they are. An instance that casts a map whose values are stored otherwise is
// reported where it does so, in a check and in a build. It used to pass the
// check and give the C compiler an invalid cast.
fn test_a_map_type_cast_is_checked_for_each_instance() {
	source := map_cast_declarations + 'fn main() {\n' + map_cast_calls +
		"\tprintln(to_counts({\n\t\t'a': 258\n\t}))\n}\n"
	for res in [check_program('map_cast_check', source), build_program('map_cast_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == [
			'11:9: cannot cast map `map[string]int` to `map[string]u8`',
		], res.output
	}
}

const comptime_for_declarations = 'struct Foo {
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

'

const comptime_for_calls = '	println(wrap(Bar{ b: 1 }))
	println(count(Bar{ b: 1 }))
'

// The body of a `$for` is checked in each instance as the rest of the body is:
// its locals are seen and its returns are checked. `[x]` is a `[]Bar` for a
// `Bar`, but a `[]Foo` for a `Foo`, which is no `[]Bar`. It used to pass the
// check and a build, and reach the C compiler.
fn test_an_array_of_another_type_in_a_comptime_for_body_is_reported_per_instance() {
	source := comptime_for_declarations + 'fn main() {\n' + comptime_for_calls +
		'\tprintln(wrap(Foo{ a: 2 }))\n\tprintln(count(Foo{ a: 2 }))\n}\n'
	for res in [check_program('comptime_for_check', source),
		build_program('comptime_for_build', source)] {
		assert res.exit_code != 0, res.output
		assert !res.output.contains('C compilation error'), res.output
		assert error_lines(res.output) == [
			'25:10: cannot use `[]Foo` as type `[]Bar` in return argument',
			'33:20: cannot use `[]Foo` as `[]Bar` in argument 1 to `take_bars`',
		], res.output
	}
}

// A map cast to its own type and compile-time loops with the expected types build and run.
fn test_map_cast_and_comptime_for_instances_build_and_run() {
	source := map_cast_declarations + comptime_for_declarations + 'fn main() {\n' + map_cast_calls +
		comptime_for_calls + '}\n'
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
	assert res.output.split_into_lines() == ["{'a': 7}", '[Bar{', '    b: 1', '}]', '1'], res.output
}

// A local of a `$for` body that has the name of a constant of its own module
// is the local, in the check of an instance too: `&routes` is a
// `&map[string]int`, not a `&[]string`.
fn test_a_local_of_a_comptime_for_body_shadows_a_constant_of_its_module() {
	source := "const routes = ['/', '/x']\n\nstruct Foo {\n\ta int\n}\n\nstruct Holder {\n\troutes &map[string]int\n}\n\nfn make_routes() map[string]int {\n\treturn {\n\t\t'a': 1\n\t}\n}\n\nfn first_routes[T](x T) int {\n\t\$for field in T.fields {\n\t\troutes := make_routes()\n\t\th := Holder{\n\t\t\troutes: &routes\n\t\t}\n\t\treturn h.routes.len\n\t}\n\treturn 0\n}\n\nfn main() {\n\tprintln(routes)\n\tprintln(first_routes(Foo{}))\n}\n"
	for res in [check_program('comptime_for_shadow_check', source),
		build_program('comptime_for_shadow_build', source)] {
		assert res.exit_code == 0, res.output
		assert error_lines(res.output) == [], res.output
	}
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
