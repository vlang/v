import os

fn v3_binary_for(dir string) string {
	v3_bin := os.join_path(dir, 'v3')
	v3_dir := os.dir(os.dir(@FILE))
	vlib_dir := os.dir(v3_dir)
	build := os.execute('${os.quoted_path(@VEXE)} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(v3_dir, 'v3.v'))}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `fn C.` declaration that no header can declare keeps its generated prototype:
// its file links a C source/object directly, or its module links a C library and
// includes nothing. Both are decided inside the disposable worker that renders the
// prototypes in scoped mode, which is what ordinary builds use.
fn test_linked_source_and_library_declarations_keep_their_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_linked_extern_prototype_codegen')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'libonly')) or { panic(err) }
	v3_bin := v3_binary_for(dir)
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'linked_extern_prototype' }\n") or {
		panic(err)
	}

	// A linked C source is compiled, linked and run, so a missing prototype shows up
	// as an implicit declaration error rather than a silently guessed signature.
	os.write_file(os.join_path(dir, 'helper.c'), 'int helper_fn(int x) {\n\treturn x * 2;\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n#flag @VMODROOT/helper.c\n\nfn C.helper_fn(x int) int\n\nfn main() {\n\tprintln(C.helper_fn(21))\n}\n') or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(program)} ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert result.exit_code == 0, result.output
	run := os.execute(program)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'

	// A module that only links a C library. The library itself is not built here, so
	// only the generated C is inspected for the prototype.
	os.write_file(os.join_path(dir, 'libonly', 'libonly.v'), 'module libonly\n\n#flag -L@VMODROOT -lmathhelper\n\nfn C.lib_triple(x int) int\n\npub fn triple(x int) int {\n\treturn C.lib_triple(x)\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'lib_main.v'), 'module main\n\nimport libonly\n\nfn main() {\n\tprintln(libonly.triple(14))\n}\n') or { panic(err) }
	c_path := os.join_path(dir, 'lib_main.c')
	lib_result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(c_path)} ${os.quoted_path(os.join_path(dir, 'lib_main.v'))}')
	assert lib_result.exit_code == 0, lib_result.output
	c_code := os.read_file(c_path) or { panic(err) }
	assert c_code.contains('int lib_triple(int x);'), c_code

	os.rmdir_all(dir) or {}
}

// An include for a target that is not being built contributes no header, so it must
// not take ownership of the declarations in its file.
fn test_target_inactive_include_keeps_the_linked_source_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_inactive_include_prototype_codegen')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	v3_bin := v3_binary_for(dir)
	inactive_target := if os.user_os() == 'windows' { 'linux' } else { 'windows' }
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'inactive_include_prototype' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'helper.c'), 'int helper_fn(int x) {\n\treturn x * 2;\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n#flag @VMODROOT/helper.c\n#include ${inactive_target} <inactive_probe.h>\n\nfn C.helper_fn(x int) int\n\nfn main() {\n\tprintln(C.helper_fn(21))\n}\n') or {
		panic(err)
	}
	program := os.join_path(dir, 'program')
	result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(program)} ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert result.exit_code == 0, result.output
	run := os.execute(program)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	os.rmdir_all(dir) or {}
}

// `#postinclude` is emitted after every declaration and call site, so it can never
// declare anything for them and the prototype has to stay.
fn test_postinclude_keeps_its_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_postinclude_prototype_codegen')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'postmod')) or { panic(err) }
	v3_bin := v3_binary_for(dir)
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'postinclude_prototype' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'post_impl.c'), 'int post_fn(int x) {\n\treturn x + 1;\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'postmod', 'post_api.h'), 'int post_fn(int x);\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'postmod', 'postmod.v'), 'module postmod\n\n#postinclude "@VMODROOT/postmod/post_api.h"\n\nfn C.post_fn(x int) int\n\npub fn bump(x int) int {\n\treturn C.post_fn(x)\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\nimport postmod\n\n#flag @VMODROOT/post_impl.c\n\nfn main() {\n\tprintln(postmod.bump(41))\n}\n') or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(program)} ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert result.exit_code == 0, result.output
	run := os.execute(program)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	os.rmdir_all(dir) or {}
}
