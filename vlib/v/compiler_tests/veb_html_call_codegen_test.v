import os
import v.tempname

const veb_html_app_source = 'module main

import veb

pub struct Context {
	veb.Context
}

pub struct App {}
'

fn veb_html_codegen_test_dir() !string {
	root := os.join_path(os.vtmp_dir(), 'v3_veb_html_${tempname.unique_token()}_project')
	os.mkdir(root)!
	return root
}

fn veb_html_compile(root string, handlers string) !os.Result {
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'veb_html_fixture' }\n")!
	source := os.join_path(root, 'main.v')
	os.write_file(source, veb_html_app_source + handlers)!
	return os.exec([@VEXE, '-no-memory-limit', source, '-o', os.join_path(root, 'app.c')])
}

fn test_veb_html_calls_without_arguments_use_handler_templates() {
	root := veb_html_codegen_test_dir()!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'index.html'), 'HELLO_TMPL')!
	// The show template depends on its handler's parameter.
	os.write_file(os.join_path(root, 'show.html'), 'HANDLER_TEMPLATE @name')!
	compile := veb_html_compile(root, '
pub fn (app &App) index(mut ctx Context) veb.Result {
	return \$veb.html()
}

pub fn (app &App) show(mut ctx Context, name string) veb.Result {
	return \$veb.html()
}

fn main() {
	mut app := &App{}
	mut ctx := Context{}
	_ := app.index(mut ctx)
	_ := app.show(mut ctx, "x")
}
')!
	assert compile.exit_code == 0, compile.output
	code := os.read_file(os.join_path(root, 'app.c'))!
	assert code.contains('HELLO_TMPL'), 'index should render its handler template'
	assert code.contains('HANDLER_TEMPLATE'), 'show should render its handler template'
}

fn test_veb_html_dynamic_path_is_rejected_not_handler_template() {
	root := veb_html_codegen_test_dir()!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'index.html'), 'HANDLER_TEMPLATE')!
	compile := veb_html_compile(root, '
pub fn (app &App) index(mut ctx Context, name string) veb.Result {
	return \$veb.html(name)
}

fn main() {
	mut app := &App{}
	mut ctx := Context{}
	_ := app.index(mut ctx, "x")
}
')!
	assert compile.exit_code != 0, 'a non-compile-time \$veb.html(path) must be rejected, got:\n${compile.output}'
	code := os.read_file(os.join_path(root, 'app.c')) or { '' }
	assert !code.contains('HANDLER_TEMPLATE'), 'dynamic path must not resolve to the handler template'
}

fn test_veb_html_requires_call_syntax() {
	root := veb_html_codegen_test_dir()!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'index.html'), 'HELLO_TMPL')!
	compile := veb_html_compile(root, '
pub fn (app &App) index(mut ctx Context) veb.Result {
	return \$veb.html
}

fn main() {
	mut app := &App{}
	mut ctx := Context{}
	_ := app.index(mut ctx)
}
')!
	assert compile.exit_code != 0, 'bare \$veb.html without a call must be rejected, got:\n${compile.output}'
	code := os.read_file(os.join_path(root, 'app.c')) or { '' }
	assert !code.contains('HELLO_TMPL'), 'bare \$veb.html must not render the handler template'
}
