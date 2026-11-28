import os
import v.tempname

// A `$tmpl(runtime_path)` whose argument is not a compile-time string in a plain string context
// must be DIAGNOSED, not silently lowered to an empty string. No `.veb_template` node remains for
// such an unresolved path, so compile_template_file is never called; without an explicit error the
// code would compile and render '' instead of rejecting the unsupported dynamic path.
fn test_dynamic_tmpl_path_is_diagnosed() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_dynamic_path_diag_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tdp' }\n") or { panic(err) }
	// A real template file exists, so the failure is specifically the dynamic PATH, not a
	// missing file: `p` is a runtime parameter, so `$tmpl(p)` cannot resolve at compile time.
	os.write_file(os.join_path(root, 'some.html'), 'REAL_TEMPLATE') or { panic(err) }
	source := "module main\n\nfn build(p string) string {\n\treturn \$tmpl(p)\n}\n\nfn main() {\n\tprintln(build('some.html'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	c_out := os.join_path(root, 'out.c')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-o', c_out])
	assert compile.exit_code != 0, 'a dynamic `\$tmpl(path)` must be diagnosed, got:\n${compile.output}'
	assert compile.output.contains('must be a compile-time string'), compile.output
	// It must not have silently compiled and rendered nothing.
	c_code := os.read_file(c_out) or { '' }
	assert !c_code.contains('REAL_TEMPLATE'), 'a dynamic path must not resolve to any template'
}
