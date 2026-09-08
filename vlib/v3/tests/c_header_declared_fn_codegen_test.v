import os

// A `fn C.` declaration whose file includes a C header must not get a second,
// generated prototype: the V signature cannot spell `const char*`, so the two
// declarations conflict. Header only libraries (sokol, stb, fontstash, Xlib) are
// entirely made of such declarations.
fn test_header_declared_c_fn_gets_no_generated_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_declared_c_fn_codegen')
	v3_bin := os.join_path(dir, 'v3')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	v3_dir := os.dir(os.dir(@FILE))
	vlib_dir := os.dir(v3_dir)
	build := os.execute('${os.quoted_path(@VEXE)} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(v3_dir, 'v3.v'))}')
	assert build.exit_code == 0, build.output
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'header_declared_c_fn' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'api.h'), '#ifndef V3_TEST_API_H\n#define V3_TEST_API_H\n#include <string.h>\nstatic inline int api_call(const char* name) {\n\treturn (int)strlen(name);\n}\n#endif\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n#flag -I@VMODROOT\n#include "api.h"\n\nfn C.api_call(name &char) int\n\nfn main() {\n\tassert C.api_call(c\'abcd\') == 4\n\tprintln(\'ok\')\n}\n') or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(program)} ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert result.exit_code == 0, result.output
	run := os.execute(program)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
	os.rmdir_all(dir) or {}
}
