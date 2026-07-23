import os

const igcap_vexe = @VEXE
const igcap_tests_dir = os.dir(@FILE)
const igcap_v3_dir = os.dir(igcap_tests_dir)
const igcap_vlib_dir = os.dir(igcap_v3_dir)
const igcap_v3_src = os.join_path(igcap_v3_dir, 'v3.v')

fn igcap_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_capture_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${igcap_vexe} -gc none -path "${igcap_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${igcap_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A function-valued binding introduced by an if guard (`if render := handlers[key]`) is in
// scope inside the guarded block. A template there that calls it (`@{render(row)}`) is
// lowered to a nested immediately-invoked closure, which must capture `render` — v3 closures
// do not auto-capture. Here the guard binding shadows a top-level `render`; the guard binding
// must win (`LOCAL:`), proving if-guard names are registered as local bindings.
fn test_tmpl_iife_captures_if_guard_binding() {
	v3_bin := igcap_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_capture_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'igcap' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.html'), '@{render(row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn build(row string, key string) string {\n\thandlers := {\n\t\t'x': fn (s string) string {\n\t\t\treturn 'LOCAL:' + s\n\t\t}\n\t}\n\tif render := handlers[key] {\n\t\treturn '[' + \$tmpl('row.html') + ']'\n\t}\n\treturn 'none'\n}\n\nfn main() {\n\tprintln(build('abc', 'x').replace('\\n', ''))\n\tprintln(build('abc', 'missing').replace('\\n', ''))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_capture_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split('\n')
	assert lines.len == 2, run.output
	// The guarded template called the guard binding, not the top-level `render`.
	assert lines[0] == '[LOCAL:abc]', run.output
	// When the guard fails, the else path runs.
	assert lines[1] == 'none', run.output
}
