import os
import v.tempname

// A `fn C.` declaration that no header can declare keeps its generated prototype:
// its file links a C source/object directly, or its module links a C library and
// includes nothing. Both are decided inside the disposable worker that renders the
// prototypes in scoped mode, which is what ordinary builds use.
fn test_linked_source_and_library_declarations_keep_their_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_linked_extern_prototype_codegen_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	os.mkdir(os.join_path(dir, 'libonly'))!
	os.mkdir(os.join_path(dir, 'inactive_probe'))!
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'linked_extern_prototype' }\n") or {
		panic(err)
	}

	// Check a plain linked C source and one whose V declaration shares a file with
	// an include for a different target.
	os.write_file(os.join_path(dir, 'helper.c'), 'int helper_fn(int x) {\n\treturn x * 2;\n}\n') or { panic(err) }
	inactive_target := if os.user_os() == 'windows' { 'linux' } else { 'windows' }
	os.write_file(os.join_path(dir, 'inactive_impl.c'), 'int inactive_fn(int x) {\n\treturn x * 2;\n}\n')!
	os.write_file(os.join_path(dir, 'inactive_probe', 'probe.v'), 'module inactive_probe

#flag @VMODROOT/inactive_impl.c
#include ${inactive_target} <inactive_probe.h>

fn C.inactive_fn(x int) int

pub fn double(x int) int {
	return C.inactive_fn(x)
}
')!
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import inactive_probe

#flag @VMODROOT/helper.c

fn C.helper_fn(x int) int

fn main() {
	println(C.helper_fn(21))
	println(inactive_probe.double(13))
}
')!
	program := os.join_path(dir, 'program')
	result := os.exec([@VEXE, '-b', 'c', '-o', '${program}', os.join_path(dir, 'main.v')])
	assert result.exit_code == 0, result.output
	source_c := os.read_file(program + '.c')!
	assert source_c.contains('int helper_fn(int x);'), source_c
	assert source_c.contains('int inactive_fn(int x);'), source_c
	run := os.exec([program])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42\n26', run.output

	// A module that only links a C library. The library itself is not built here, so
	// only the generated C is inspected for the prototype.
	os.write_file(os.join_path(dir, 'libonly', 'libonly.v'), 'module libonly\n\n#flag -L@VMODROOT -lmathhelper\n\nfn C.lib_triple(x int) int\n\npub fn triple(x int) int {\n\treturn C.lib_triple(x)\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'lib_main.v'), 'module main\n\nimport libonly\n\nfn main() {\n\tprintln(libonly.triple(14))\n}\n') or { panic(err) }
	c_path := os.join_path(dir, 'lib_main.c')
	lib_result := os.exec([@VEXE, '-b', 'c', '-o', c_path, os.join_path(dir, 'lib_main.v')])
	assert lib_result.exit_code == 0, lib_result.output
	c_code := os.read_file(c_path) or { panic(err) }
	assert c_code.contains('int lib_triple(int x);'), c_code
}

// `#postinclude` is emitted after every declaration and call site, so it can never
// declare anything for them and the prototype has to stay.
fn test_postinclude_keeps_its_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_postinclude_prototype_codegen_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	os.mkdir(os.join_path(dir, 'postmod'))!
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'postinclude_prototype' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'post_impl.c'), 'int post_fn(int x) {\n\treturn x + 1;\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'postmod', 'post_api.h'), 'int post_fn(int x);\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'postmod', 'postmod.v'), 'module postmod\n\n#postinclude "@VMODROOT/postmod/post_api.h"\n\nfn C.post_fn(x int) int\n\npub fn bump(x int) int {\n\treturn C.post_fn(x)\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\nimport postmod\n\n#flag @VMODROOT/post_impl.c\n\nfn main() {\n\tprintln(postmod.bump(41))\n}\n') or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.exec([@VEXE, '-b', 'c', '-o', '${program}', os.join_path(dir, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([program])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}
