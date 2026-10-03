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
	return os.exec([@VEXE, '-new-compiler', '-check', '-nocolor', path])
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
// (`NAME=value ...`) added to the check.
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
	res := os.exec(['${env}', 'VMODULES=' + '${modules_dir}', 'V_DIAGNOSTICS_TRACE=' + '${trace}',
		@VEXE, '-new-compiler', '-check', '-nocolor', os.join_path(program, 'main.v')])
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

const header_program = 'import advlib\n\nstruct User {\n\tname string\n}\n\nfn (u User) label() string {\n\treturn u.name\n}\n\nfn show[T](x T) string {\n\treturn x.label()\n}\n\nfn main() {\n\tprintln(show(User{}))\n\tprintln(advlib.label_of(User{}))\n}\n'

const method_level_program = 'import advlib\n\nstruct Runner {}\n\nfn (r Runner) run[T](x T) string {\n\treturn x.name\n}\n\nstruct Job {\n\trunner Runner\n}\n\nfn main() {\n\tprintln(advlib.run_job(Job{}))\n}\n'

fn test_a_check_clones_a_library_instance_without_its_body() {
	// `label_of[User]` calls `User.label`, which is no instance: its body can
	// give the check nothing to look at.
	check := check_with_library('headers', header_program)
	assert check.res.exit_code == 0, check.res.output
	assert check.trace == ['1 of 1 library instances cloned without their bodies'], check.trace.str()
}

fn test_v_check_library_bodies_keeps_the_body_of_every_library_instance() {
	check := check_with_modules_env('bodies_env', 'V_CHECK_LIBRARY_BODIES=1', {
		'main.v': header_program
	}, {
		'advlib/advlib.v': library_module
	})
	assert check.res.exit_code == 0, check.res.output
	assert check.trace == ['0 of 1 library instances cloned without their bodies',
		'every library instance keeps its body: V_CHECK_LIBRARY_BODIES=1 asks for them'], check.trace.str()
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
	dir := os.join_path(os.vtmp_dir(), 'v3_build_generic_instances_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, generic_prelude + source) or { panic(err) }
	return os.exec(['env', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-new-compiler', '-nocolor', '-o',
		os.join_path(dir, 'main'), path])
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
