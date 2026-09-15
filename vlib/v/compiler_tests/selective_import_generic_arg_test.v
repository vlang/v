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

// A selective import of a name wins over a same-named declaration in the writing
// file's own module: the checker resolves the bare spelling through the file's
// selective imports (`qualify_type_text_impl`), so the transform and cgen have to
// keep that order as well. This program only compiles while every bare `Token` in
// `main.v` means `iam.Token` - the local `main.Token` has a `name` field, the
// imported one has `id` - so a local-first rewrite would break it again.
fn test_bare_name_prefers_file_selective_import_over_local_declaration() {
	v3_bin := selective_arg_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_local_vs_selective_import')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'iam')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "local_vs_import"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'iam', 'token.v'), 'module iam

pub struct Token {
pub mut:
	id int
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), "module main

import iam { Token }

struct Token {
	name string
}

fn take_id[T](value T) {
	println(value.id)
}

fn main() {
	mut t := Token{}
	t.id = 7
	println(t.id)
	take_id[iam.Token](t)
}
") or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7\n7', run.output
}
