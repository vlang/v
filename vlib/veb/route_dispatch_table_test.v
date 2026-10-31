module veb

import net.http
import os

struct TableDispatchContext {
	Context
mut:
	events []string
}

fn (mut ctx TableDispatchContext) before_request() {
	ctx.events << 'before'
}

struct TableDispatchApp {
	Controller
}

fn (app &TableDispatchApp) index(mut ctx TableDispatchContext) Result {
	return ctx.text('root:${ctx.events.join(',')}')
}

@[heap]
struct TableDispatchFirst {
	Middleware[TableDispatchContext]
mut:
	calls int
}

@['/item/:id/:enabled'; get]
fn (mut app TableDispatchFirst) item(mut ctx TableDispatchContext, id int, enabled bool) Result {
	app.calls += id
	return ctx.text('first:${id}:${enabled}:${ctx.events.join(',')}')
}

@['/install-after'; get]
fn (mut app TableDispatchFirst) install_after(mut ctx TableDispatchContext) Result {
	app.use(
		after:   true
		handler: fn (mut ctx TableDispatchContext) bool {
			ctx.res.body += ':installed-after'
			return true
		}
	)
	return ctx.text('installed')
}

@[heap]
struct TableDispatchSecond {
mut:
	calls int
}

@['/item/:name'; get]
fn (mut app TableDispatchSecond) item(mut ctx TableDispatchContext, name string) !Result {
	app.calls++
	if name == 'fail' {
		return error('second failed')
	}
	return ctx.text('second:${name}:${ctx.events.join(',')}')
}

fn table_dispatch_request(mut app TableDispatchApp, url string) &Context {
	return render_request[TableDispatchApp, TableDispatchContext](mut app, http.Request{
		method: .get
		url:    url
	}) or { panic(err) }
}

fn test_table_dispatch_keeps_receiver_context_and_parameter_types() {
	mut app := TableDispatchApp{}
	mut first := &TableDispatchFirst{}
	first.use(
		handler: fn (mut ctx TableDispatchContext) bool {
			ctx.events << 'global'
			return true
		}
	)
	first.route_use('/item/:id/:enabled',
		handler: fn (mut ctx TableDispatchContext) bool {
			ctx.events << 'route'
			return true
		}
	)
	mut second := &TableDispatchSecond{}
	app.register_controller[TableDispatchFirst, TableDispatchContext]('/first', mut first) or {
		panic(err)
	}
	app.register_controller[TableDispatchSecond, TableDispatchContext]('/second', mut second) or {
		panic(err)
	}
	assert table_dispatch_request(mut app, '/').res.body == 'root:before'
	assert table_dispatch_request(mut app, '/first/item/7/true').res.body == 'first:7:true:before,global,route'
	assert table_dispatch_request(mut app, '/second/item/hello').res.body == 'second:hello:before'
	failed := table_dispatch_request(mut app, '/second/item/fail')
	assert failed.res.status_code == 500
	assert failed.res.body.contains('second failed')
	assert table_dispatch_request(mut app, '/first/item/3/false').res.body == 'first:3:false:before,global,route'
	assert first.calls == 10
	assert second.calls == 2
}

fn test_table_dispatch_reads_after_middleware_after_the_handler() {
	mut app := TableDispatchFirst{}
	ctx := render_request[TableDispatchFirst, TableDispatchContext](mut app, http.Request{
		method: .get
		url:    '/install-after'
	}) or { panic(err) }
	assert ctx.res.body == 'installed:installed-after'
}

fn test_table_dispatch_emits_one_dispatcher_for_a_shared_context() {
	dir := os.join_path(os.vtmp_dir(), 'veb_table_dispatch_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	output := os.join_path(dir, 'dispatch.c')
	result := os.exec([@VEXE, '-nocache', '-o', output, @FILE])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output) or { panic(err) }
	definitions := generated.split_into_lines().filter(it.starts_with('void veb__dispatch_route_T_')
		&& it.ends_with(' {'))
	assert definitions.len == 1, definitions.join('\n')
	static_definitions := generated.split_into_lines().filter(it.starts_with('bool veb__serve_if_static_T_')
		&& it.ends_with(' {'))
	assert static_definitions.len == 0, static_definitions.join('\n')
}
