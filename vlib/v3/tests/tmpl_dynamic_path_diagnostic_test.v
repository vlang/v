import os

const tdp_vexe = @VEXE
const tdp_tests_dir = os.dir(@FILE)
const tdp_v3_dir = os.dir(tdp_tests_dir)
const tdp_vlib_dir = os.dir(tdp_v3_dir)
const tdp_v3_src = os.join_path(tdp_v3_dir, 'v3.v')

fn tdp_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_dynamic_path_diag_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${tdp_vexe} -gc none -path "${tdp_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${tdp_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `$tmpl(runtime_path)` whose argument is not a compile-time string in a plain string context
// must be DIAGNOSED, not silently lowered to an empty string. No `.veb_template` node remains for
// such an unresolved path, so compile_template_file is never called; without an explicit error the
// code would compile and render '' instead of rejecting the unsupported dynamic path.
fn test_dynamic_tmpl_path_is_diagnosed() {
	v3_bin := tdp_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_dynamic_path_diag_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tdp' }\n") or { panic(err) }
	// A real template file exists, so the failure is specifically the dynamic PATH, not a
	// missing file: `p` is a runtime parameter, so `$tmpl(p)` cannot resolve at compile time.
	os.write_file(os.join_path(root, 'some.html'), 'REAL_TEMPLATE') or { panic(err) }
	source := "module main\n\nfn build(p string) string {\n\treturn \$tmpl(p)\n}\n\nfn main() {\n\tprintln(build('some.html'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	c_out := os.join_path(root, 'out.c')
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -o ${c_out}')
	assert compile.exit_code != 0, 'a dynamic `\$tmpl(path)` must be diagnosed, got:\n${compile.output}'
	assert compile.output.contains('must be a compile-time string'), compile.output
	// It must not have silently compiled and rendered nothing.
	c_code := os.read_file(c_out) or { '' }
	assert !c_code.contains('REAL_TEMPLATE'), 'a dynamic path must not resolve to any template'
}
