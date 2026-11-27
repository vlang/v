import os
import v.tempname

fn veb_ctx_test_dir(name string) !string {
	root := os.join_path(os.vtmp_dir(), 'v3_veb_ctx_${name}_${tempname.unique_token()}')
	os.mkdir(root)!
	return root
}

// A `veb.Result` method that receives the implicit `Context` parameter can be
// called by another handler without passing ctx explicitly. The call must
// type-check (not report a missing argument) and forward the enclosing `ctx`,
// not a zero/default value.
fn test_veb_implicit_ctx_forwarded_at_direct_and_reflected_call_sites() {
	root := veb_ctx_test_dir('forwarded')!
	defer { os.rmdir_all(root) or {} }
	src := '
import veb

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	n int
}

pub fn (app &App) index() veb.Result {
	app.show(5)
	$for method in App.methods {
		if method.name == "helper" {
			return app.$method()
		}
	}
	return veb.no_result()
}

pub fn (app &App) helper() veb.Result {
	return veb.no_result()
}

pub fn (app &App) show(id int) veb.Result {
	return veb.no_result()
}

fn main() {
	mut app := &App{}
	_ := app.index()
}
'
	src_file := os.join_path(root, 'v3_veb_ctx.v')
	os.write_file(src_file, src) or { panic(err) }
	c_out := os.join_path(root, 'v3_veb_ctx.c')
	compile := os.exec([@VEXE, '-no-memory-limit', src_file, '-o', '${c_out}'])
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { '' }
	// No-arg reflected delegation forwards the enclosing ctx in the ctx slot.
	assert c_code.contains('App__helper(app, ctx)'), c_code
	// Delegation that also passes a real argument keeps ctx at its slot,
	// so the explicit argument still lines up with its parameter.
	assert c_code.contains('App__show(app, ctx, 5)'), c_code
}

// An explicitly named handler context is still the context forwarded to a handler
// whose context is implicit. Its call-site spelling must match its parameter
// declaration even when the source name shadows a runtime C type.
fn test_veb_explicit_ctx_named_like_runtime_type_is_forwarded() {
	root := veb_ctx_test_dir('renamed')!
	defer { os.rmdir_all(root) or {} }
	src_file := os.join_path(root, 'main.v')
	os.write_file(src_file, 'module main

import veb

pub struct Context {
	veb.Context
}

pub struct App {}

pub fn (app &App) index(mut array Context) veb.Result {
	return app.helper()
}

pub fn (app &App) helper() veb.Result {
	return veb.no_result()
}

fn main() {
	app := &App{}
	mut ctx := Context{}
	_ := app.index(mut ctx)
}
') or {
		panic(err)
	}
	bin_out := os.join_path(root, 'app')
	compile := os.exec([@VEXE, '-no-memory-limit', '-nocache', src_file, '-o', bin_out])
	assert compile.exit_code == 0, compile.output
}

// Handlers with unnamed parameters still receive an implicit ctx. An
// imported Context that aliases string is also an ordinary route parameter.
fn test_veb_implicit_ctx_with_unnamed_and_alias_route_params() {
	root := veb_ctx_test_dir('route_params')!
	defer { os.rmdir_all(root) or {} }
	os.mkdir(os.join_path(root, 'other'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vebimportedctx' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'other', 'other.v'), 'module other\n\npub type Context = string\n') or { panic(err) }
	// Keep the alias import scoped to the file that uses it.
	os.write_file(os.join_path(root, 'alias_route.v'), "module main

import veb
import other

pub struct AliasApp {}

pub fn (mut app AliasApp) index(mut ctx Context) veb.Result {
	return ctx.text('ok')
}

pub fn (mut app AliasApp) show(slug other.Context) veb.Result {
	return ctx.text('slug=\${slug}')
}
") or { panic(err) }

	main_src := '
module main

import veb

pub struct Context {
	veb.Context
}

pub struct App {}

fn (mut ctx Context) coming_soon() veb.Result {
	return ctx.text("coming soon")
}

@["/@/:author_slug/:post_slug/bookmark"; get]
pub fn (app App) bookmark(_ string, _ string) veb.Result {
	_ = app
	return ctx.coming_soon()
}

@["/change/:lang"; get]
pub fn (mut app App) change_lang(lang string) veb.Result {
	_ = app
	return ctx.text(lang)
}

fn main() {
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: 0) or { panic(err) }
}
'
	os.write_file(os.join_path(root, 'main.v'), main_src) or { panic(err) }
	bin_out := os.join_path(root, 'app')
	compile := os.exec([@VEXE, '-no-memory-limit', root, '-o', bin_out])
	assert compile.exit_code == 0, compile.output
}

// A program Context can embed veb.Context and declare the same hook name. The
// generated router must call the program method with the complete Context, not
// route its receiver to the embedded framework field just because both types
// share the short name `Context`.
fn test_veb_program_context_hook_keeps_complete_receiver() {
	root := veb_ctx_test_dir('complete_receiver')!
	defer { os.rmdir_all(root) or {} }
	src := '
import veb

pub struct Context {
	veb.Context
	msgs []string
}

pub struct App {}

pub fn (ctx &Context) before_request() {}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	info := struct {
		profile: map[string]string{
			"slug": "v3"
		}
		messages: ctx.msgs
	}
	return ctx.json_pretty(info)
}

fn main() {
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: 0) or { panic(err) }
}
'
	src_file := os.join_path(root, 'v3_veb_complete_context_receiver.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_out := os.join_path(root, 'v3_veb_complete_context_receiver')
	compile := os.exec([@VEXE, '-no-memory-limit', '-b', 'c', src_file, '-o', bin_out])
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert c_code.contains('Context__before_request(user_context)'), c_code
	assert !c_code.contains('Context__before_request(&user_context->veb__Context)'), c_code
	assert c_code.contains('AnonStruct_v3_inferred_'), c_code
	assert !c_code.contains('v_struct'), c_code
}

fn test_veb_reflected_context_arguments() {
	root := veb_ctx_test_dir('reflected_context_arguments')!
	defer { os.rmdir_all(root) or {} }
	src := '
import veb

pub struct Context {
	veb.Context
}

pub struct SpreadApp {}

fn (mut ctx Context) not_found() veb.Result {
	return ctx.text("program")
}

fn (app SpreadApp) bookmark(kind string, id string) veb.Result {
	_ = app
	println(kind + ":" + id)
	return veb.no_result()
}

// Reflected calls pass the hidden veb context explicitly. Their source method
// metadata still omits that hidden parameter, so argument spreading must use
// the checked ABI signature, which already includes the hidden context.
// The same program also ensures a framework method binds to `veb.Context`
// rather than a same-named program receiver.
fn dispatch_spread[A, X](app A, mut ctx X, args []string) {
	$for method in A.methods {
		$if method.return_type is veb.Result {
			app.$method(mut ctx, ...args)
		}
	}
}

fn check_spread() {
	mut framework_ctx := veb.Context{}
	_ := framework_ctx.file(@FILE)
	mut ctx := Context{}
	dispatch_spread(SpreadApp{}, mut ctx, ["first", "second"])
}

pub struct ContextOnlyApp {}

pub fn (mut app ContextOnlyApp) index() veb.Result {
	println("index called")
	return veb.no_result()
}

pub fn (mut app ContextOnlyApp) explicit(mut ctx Context) veb.Result {
	println("explicit called")
	return veb.no_result()
}

// A reflected `app.$method(mut ctx)` call passes the hidden veb context as its
// only argument. Handlers that declare no parameters of their own still accept
// it, so the specialization must be kept: dropping it as an arity mismatch made
// veb answer `HTTP/ 0` for every route whose handler omitted `ctx`. (A handler
// that needs more arguments is a reflected-call arity error, so none is here.)
fn dispatch_context_only[A, X](mut app A, mut ctx X) {
	$for method in A.methods {
		$if method.return_type is veb.Result {
			app.$method(mut ctx)
		}
	}
}

fn check_context_only() {
	mut app := ContextOnlyApp{}
	mut ctx := Context{}
	dispatch_context_only(mut app, mut ctx)
}

pub interface RouteArg {}

pub struct InterfaceApp {}

pub fn (mut app InterfaceApp) accepts(value RouteArg) veb.Result {
	_ = value
	println("interface called")
	return veb.no_result()
}

// Arity decides whether a reflected argument occupies the hidden context slot.
// A Context value can therefore bind to a declared interface route parameter
// when that interface accepts Context; it must not be consumed as an explicit
// copy of the hidden parameter merely because its concrete type is Context.
// (A `string` route parameter would leave the argument in the hidden slot and
// the route short of an argument, which is a reflected-call arity error.)
fn dispatch_interface_arg[A](mut app A, ctx &Context) {
	$for method in A.methods {
		$if method.return_type is veb.Result {
			app.$method(ctx)
		}
	}
}

fn check_interface_arg() {
	mut app := InterfaceApp{}
	ctx := &Context{}
	dispatch_interface_arg(mut app, ctx)
}

fn main() {
	check_spread()
	check_context_only()
	check_interface_arg()
}
'
	src_file := os.join_path(root, 'v3_veb_reflected_context_arguments.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_out := os.join_path(root, 'v3_veb_reflected_context_arguments')
	compile := os.exec([@VEXE, '-no-memory-limit', '-b', 'c', src_file, '-o', bin_out])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == [
		'first:second',
		'index called',
		'explicit called',
		'interface called',
	], run.output
}

// Reflected routes receive the context type they declare. The promoted
// generic json method on the custom route receives the embedded veb.Context.
fn test_veb_reflected_calls_use_the_declared_context_receiver() {
	root := veb_ctx_test_dir('context_receivers')!
	defer { os.rmdir_all(root) or {} }
	src := '
import veb

pub struct Context {
	veb.Context
	marker int
}

pub struct FrameworkApp {}

pub fn (mut app FrameworkApp) index(mut ctx veb.Context) veb.Result {
	return ctx.text("hello")
}

fn dispatch[A, X](mut app A, mut ctx X) {
	$for method in A.methods {
		$if method.return_type is veb.Result {
			app.$method(mut ctx)
		}
	}
}

fn check_framework_context() {
	mut app := FrameworkApp{}
	mut ctx := Context{}
	dispatch(mut app, mut ctx)
}

pub struct CustomApp {}

pub struct SomeData {
	id string
}

fn (mut ctx Context) handle_ok[T](payload T) veb.Result {
	ctx.res.set_status(.ok)
	return ctx.json(payload)
}

@["/some_data"; get]
pub fn (mut app CustomApp) some_data(mut ctx Context) veb.Result {
	return ctx.handle_ok(SomeData{ id: "v3" })
}

fn main() {
	check_framework_context()
	mut app := CustomApp{}
	veb.run[CustomApp, Context](mut app, 8080)
}
'
	src_file := os.join_path(root, 'v3_veb_context_receivers.v')
	os.write_file(src_file, src) or { panic(err) }
	c_out := os.join_path(root, 'v3_veb_context_receivers.c')
	compile := os.exec([@VEXE, '-no-memory-limit', '-nocache', src_file, '-o', '${c_out}'])
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { '' }
	assert c_code.contains('FrameworkApp__index(app, &ctx->veb__Context)'), c_code
	assert !c_code.contains('FrameworkApp__index(app, ctx)'), c_code
	assert c_code.contains('veb__Context_SomeData__json(&ctx->veb__Context, payload)'), c_code
	assert c_code.contains('CustomApp__some_data(app, user_context)'), c_code
	assert !c_code.contains('CustomApp__some_data(app, &user_context->veb__Context)'), c_code
}
