import os
import v.tempname

fn vhrc_app_source(call string) string {
	return 'module main\n\nimport veb\n\npub struct Context {\n\tveb.Context\n}\n\npub struct App {}\n\npub fn (app &App) index(mut ctx Context) veb.Result {\n\treturn ${call}\n}\n\nfn main() {\n\tmut app := &App{}\n\tmut ctx := Context{}\n\t_ := app.index(mut ctx)\n}\n'
}

// `$veb.html` must be written as a call. A no-arg `$veb.html()` legitimately resolves to the
// handler's own template (here `index.html`) and renders it, but a bare `$veb.html` WITHOUT
// parentheses is not a call: it must be diagnosed rather than silently resolving its empty
// argument to that same no-arg handler-template lookup (rendering an invalid comptime
// expression instead of reporting it).
fn test_veb_html_requires_call_syntax() {
	root := os.join_path(os.vtmp_dir(), 'v3_veb_html_requires_call_${tempname.unique_token()}_project')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vhrc' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'index.html'), 'HELLO_TMPL') or { panic(err) }

	// With parentheses: the no-arg call renders the handler template.
	os.write_file(os.join_path(root, 'ok.v'), vhrc_app_source('\$veb.html()')) or { panic(err) }
	ok_c := os.join_path(root, 'ok.c')
	ok := os.exec([@VEXE, '-no-memory-limit', os.join_path(root, 'ok.v'), '-o', '${ok_c}'])
	assert ok.exit_code == 0, ok.output
	ok_code := os.read_file(ok_c) or { '' }
	assert ok_code.contains('HELLO_TMPL'), 'the call form should render the handler template'

	// Without parentheses: not a call — must be diagnosed, and must NOT render the template.
	os.write_file(os.join_path(root, 'bad.v'), vhrc_app_source('\$veb.html')) or { panic(err) }
	bad_c := os.join_path(root, 'bad.c')
	bad := os.exec([@VEXE, '-no-memory-limit', os.join_path(root, 'bad.v'), '-o', '${bad_c}'])
	assert bad.exit_code != 0, 'bare `\$veb.html` without a call must be rejected, got:\n${bad.output}'
	bad_code := os.read_file(bad_c) or { '' }
	assert !bad_code.contains('HELLO_TMPL'), 'bare `\$veb.html` must not render the handler template'
}
