import os
import v.tempname

// A `fn C.` declaration whose file includes a C header must not get a second,
// generated prototype: the V signature cannot spell `const char*`, so the two
// declarations conflict. Header only libraries (sokol, stb, fontstash, Xlib) are
// entirely made of such declarations.
fn test_header_declared_c_fn_gets_no_generated_prototype() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_declared_c_fn_codegen_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'header_declared_c_fn' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'api.h'), '#ifndef V3_TEST_API_H\n#define V3_TEST_API_H\n#include <string.h>\nstatic inline int api_call(const char* name) {\n\treturn (int)strlen(name);\n}\n#endif\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n#flag -I@VMODROOT\n#include "api.h"\n\nfn C.api_call(name &char) int\n\nfn main() {\n\tassert C.api_call(c\'abcd\') == 4\n\tprintln(\'ok\')\n}\n') or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.exec([@VEXE, '-b', 'c', '-o', '${program}', os.join_path(dir, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([program])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
