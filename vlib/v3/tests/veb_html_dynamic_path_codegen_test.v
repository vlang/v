import os

const vhd_vexe = @VEXE
const vhd_tests_dir = os.dir(@FILE)
const vhd_v3_dir = os.dir(vhd_tests_dir)
const vhd_vlib_dir = os.dir(vhd_v3_dir)
const vhd_v3_src = os.join_path(vhd_v3_dir, 'v3.v')

fn vhd_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_veb_html_dynamic_path_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${vhd_vexe} -gc none -path "${vhd_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${vhd_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn vhd_app_source(call string) string {
	return 'module main\n\nimport veb\n\npub struct Context {\n\tveb.Context\n}\n\npub struct App {}\n\npub fn (app &App) index(mut ctx Context, name string) veb.Result {\n\treturn ${call}\n}\n\nfn main() {\n\tmut app := &App{}\n\tmut ctx := Context{}\n\t_ := app.index(mut ctx, \'x\')\n}\n'
}

// A `$veb.html(expr)` whose argument is not a compile-time string (`$veb.html(name)`, a runtime
// parameter) is an unsupported dynamic path. Its unresolved '' must NOT reuse the same sentinel as
// a genuinely argument-less `$veb.html()`, or it would silently resolve to the handler template and
// render the wrong file. It must be rejected (diagnosed) instead, while the no-arg form still
// resolves to the handler template.
fn test_veb_html_dynamic_path_is_rejected_not_handler_template() {
	v3_bin := vhd_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_veb_html_dynamic_path_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vhd' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'index.html'), 'HANDLER_TEMPLATE') or { panic(err) }

	// A runtime path argument must be diagnosed, and must NOT render the handler template.
	os.write_file(os.join_path(root, 'dyn.v'), vhd_app_source('\$veb.html(name)')) or { panic(err) }
	dyn_c := os.join_path(root, 'dyn.c')
	os.rm(dyn_c) or {}
	dyn := os.execute('${v3_bin} ${os.join_path(root, 'dyn.v')} -o ${dyn_c}')
	assert dyn.exit_code != 0, 'a non-compile-time `\$veb.html(path)` must be rejected, got:\n${dyn.output}'
	dyn_code := os.read_file(dyn_c) or { '' }
	assert !dyn_code.contains('HANDLER_TEMPLATE'), 'dynamic path must not resolve to the handler template'

	// The genuinely no-arg form still resolves to and renders the handler template.
	os.write_file(os.join_path(root, 'noarg.v'), vhd_app_source('\$veb.html()')) or { panic(err) }
	noarg_c := os.join_path(root, 'noarg.c')
	os.rm(noarg_c) or {}
	noarg := os.execute('${v3_bin} ${os.join_path(root, 'noarg.v')} -o ${noarg_c}')
	assert noarg.exit_code == 0, noarg.output
	noarg_code := os.read_file(noarg_c) or { '' }
	assert noarg_code.contains('HANDLER_TEMPLATE'), 'the no-arg form should render the handler template'
}
