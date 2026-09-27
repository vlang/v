import os

const selective_arg_vexe = @VEXE
const selective_arg_tests_dir = os.dir(@FILE)
const selective_arg_v3_dir = os.dir(selective_arg_tests_dir)
const selective_arg_vlib_dir = os.dir(selective_arg_v3_dir)
const selective_arg_v3_src = os.join_path(selective_arg_v3_dir, 'v.v')

fn selective_arg_v3_bin() string {
	bin := selective_arg_v3_bin_path()
	if os.exists(bin) {
		return bin
	}
	build := os.execute('${selective_arg_vexe} -gc none -path "${selective_arg_vlib_dir}|@vlib|@vmodules" -o ${bin} ${selective_arg_v3_src}')
	assert build.exit_code == 0, build.output
	return bin
}

fn selective_arg_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_selective_import_generic_arg_test')
}

fn testsuite_begin() {
	os.rm(selective_arg_v3_bin_path()) or {}
}

// A generic call resolves its type arguments in the file that writes the call.
// A bare `Token` there means the selectively imported `iam.Token`, not another
// imported module's same-named type: when the rewrite and the emitted
// specialization disagreed, V3 reported bogus `unknown function` diagnostics
// (vlang/v#28489) or failed to link.
fn test_selective_import_bare_type_arg_wins_over_imported_homonym() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_selective_import_generic_arg')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'json2')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'iam')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "selective_arg"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'json2', 'token.v'), 'module json2

pub struct Token {
	kind int
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'iam', 'token.v'), 'module iam

pub struct Token {
pub mut:
	id int
}

pub fn (mut t Token) bump() {
	t.id++
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'route.v'), 'module route

import json2
import iam { Token }

pub fn bump_ctrl[T](mut ctrl T) {
	ctrl.bump()
}

pub fn bump_token() int {
	mut t := Token{}
	bump_ctrl[Token](mut t)
	return t.id
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), "module main

import route

fn main() {
	assert route.bump_token() == 1
	println('ok')
}
") or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// The literal that is passed for a generic parameter must resolve the same way:
// `Token{}` in a file that imports `bbb { Token }` is bbb.Token even when another
// imported module declares a same-named Token with different fields. The call
// used to be rewritten with the homonym, leaving the specialization undefined.
fn test_selective_import_bare_literal_arg_wins_over_imported_homonym() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_selective_import_literal_arg')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'aaa')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'bbb')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "selective_literal"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'aaa', 'a.v'), 'module aaa

pub struct Token {
pub mut:
	lit  []u8
	kind int = 3
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'bbb', 'b.v'), 'module bbb

pub struct Token {
pub mut:
	id int
}

pub fn take[T](mut ctrl T) {
	println(T.name)
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import aaa
import bbb { Token }

fn main() {
	mut t := Token{}
	bbb.take[Token](mut t)
	_ := aaa.Token{}
}
') or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'bbb.Token'
}

// Import aliases decide which enum an `<Enum>__autostr` call belongs to as well.
// `import toml.token` names that module `toml.token` once the program also
// declares a module called `token`, and the writing file still spells the enum
// `token.Kind`. The helper lookup used to fall back to the bare `Kind` and emit
// `Kind__autostr(...)`, which cgen never defines, so formatting `[]token.Kind`
// failed the C compilation with `incompatible type for argument 2 of
// 'string__plus'` (the veb/orm RuoQi build hit this through vlib's toml parser).
fn test_path_import_alias_enum_autostr_uses_declaring_module() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_alias_enum_autostr')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'token')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "alias_enum"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'token', 'token.v'), 'module token

pub struct Token {
pub mut:
	id int
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), "module main

import toml
import token

fn main() {
	toml.parse_text('a = 1') or { panic(err) }
	t := token.Token{
		id: 7
	}
	println(t.id)
}
") or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7'
}

// The reverse case: an enum type that the checker already resolved keeps its module. With
// `import a as real_a` and `import b as a`, `real_a.make()` returns `a.Kind`, which must
// be formatted by module a's helper even though this file spells module b as `a`. A
// parameter declared as `[]a.Kind` in the same file still means module b's enum.
fn test_resolved_enum_type_is_not_retargeted_by_file_import_alias() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_resolved_enum_import_alias')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'b')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "resolved_enum_alias"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'a', 'a.v'), 'module a

pub enum Kind {
	from_a
}

pub fn make() Kind {
	return .from_a
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'b', 'b.v'), 'module b

pub enum Kind {
	from_b
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), r"module main

import a as real_a
import b as a

fn format_declared(kinds []a.Kind) string {
	return '${kinds}'
}

fn main() {
	value := real_a.make()
	other := a.Kind.from_b
	println('${value.str()} ${value} ${[value]} ${other} ${format_declared([other])}')
}
") or {
		panic(err)
	}
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'from_a from_a [from_a] from_b [from_b]', run.output
}

// A generic sum spelled with a selectively imported argument (`maybe.Maybe[Token]` with
// `import iam { Token }` next to another imported `Token`) is one specialization,
// `maybe.Maybe[iam.Token]`, in parameter types, casts and variant literals alike.
fn test_selective_import_generic_sum_arg_wins_over_imported_homonym() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_selective_import_generic_sum')
	os.rmdir_all(dir) or {}
	for sub in ['iam', 'other', 'maybe'] {
		os.mkdir_all(os.join_path(dir, sub)) or { panic(err) }
	}
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "selective_generic_sum"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'iam', 'iam.v'), 'module iam

pub struct Token {
pub:
	id int
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'other', 'other.v'), 'module other

pub struct Token {
pub:
	kind string
	lit  []u8
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'maybe', 'maybe.v'), 'module maybe

pub struct None {}

pub struct Some[T] {
pub:
	value T
}

pub type Maybe[T] = None | Some[T]
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import other
import iam { Token }
import maybe

fn get(m maybe.Maybe[Token]) int {
	return match m {
		maybe.Some[Token] { m.value.id }
		maybe.None { -1 }
	}
}

fn main() {
	m := maybe.Maybe[Token](maybe.Some[Token]{
		value: Token{
			id: 7
		}
	})
	println(get(m))
	println(get(maybe.Maybe[Token](maybe.None{})))
	_ := other.Token{}
}
') or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7\n-1', run.output
}

// A selective import can name a type alias (`type Alias = Real`). The literal
// `Alias{}` and the type argument in `take_id[Alias]` then mean `iam.Real`, not a
// same-named struct in another imported module.
fn test_selective_import_struct_alias_wins_over_imported_homonym() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_selective_alias_struct')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'aaa')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'iam')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "selective_alias"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'aaa', 'alias.v'), 'module aaa

pub struct Alias {
pub:
	name string
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'iam', 'token.v'), 'module iam

pub struct Real {
pub mut:
	id int
}

pub type Alias = Real
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import aaa
import iam { Alias }

fn take_id[T](value T) {
	println(value.id)
}

fn main() {
	value := Alias{
		id: 5
	}
	println(value.id)
	take_id[Alias](value)
	_ := aaa.Alias{}
}
') or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5\n5', run.output
}

// A generic parameter is lexical: inside `fn outer[T]`, the `T` in
// `inner[T](...)` is the parameter even when the file that writes the call also
// does `import pkg { T }`. The checker resolves the parameter first, so the
// transform must not rewrite the argument through the file's selective imports:
// that used to request a specialization of `inner` for `pkg.T` and fail the C
// compilation on the generic addition.
fn test_generic_parameter_wins_over_same_named_selective_import() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_generic_param_shadow')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'pkg')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "generic_param_shadow"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'pkg', 't.v'), 'module pkg

pub struct T {
pub mut:
	id int
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import pkg { T }

fn inner[X](a X, b X) X {
	return a + b
}

fn outer[T](a T, b T) T {
	return inner[T](a, b)
}

fn main() {
	println(outer[int](2, 3))
}
') or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5', run.output
}

// The same precedence applies to a generic function value (`f := inner[T]`)
// captured inside a generic function: the argument must stay the parameter so
// the clone substitutes the caller's type, not the selectively imported type.
fn test_generic_parameter_wins_over_same_named_selective_import_fn_value() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_generic_param_shadow_fn_value')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'pkg')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "generic_param_fn_value"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'pkg', 't.v'), 'module pkg

pub struct T {
pub mut:
	id int
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import pkg { T }

fn inner[X](a X, b X) X {
	return a + b
}

fn outer[T](a T, b T) T {
	f := inner[T]
	return f(a, b)
}

fn main() {
	println(outer[int](2, 3))
}
') or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5', run.output
}
