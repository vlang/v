import x.vweb
import time
import os
import net.http

const port = 13006

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

pub struct Context {
	vweb.Context
}

pub struct App {
	vweb.Controller
}

pub fn (app &App) index(mut ctx Context) vweb.Result {
	return ctx.text('from app')
}

@['/conflict/test']
pub fn (app &App) conflicting(mut ctx Context) vweb.Result {
	return ctx.text('from conflicting')
}

pub struct Other {
	vweb.Controller
}

pub fn (app &Other) index(mut ctx Context) vweb.Result {
	return ctx.text('from other')
}

pub struct HiddenByOther {}

pub fn (app &HiddenByOther) index(mut ctx Context) vweb.Result {
	return ctx.text('from hidden')
}

pub struct SubController {}

pub fn (app &SubController) index(mut ctx Context) vweb.Result {
	return ctx.text('from sub')
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!

	spawn fn () ! {
		mut sub := &SubController{}
		mut other := &Other{}
		other.register_controller[SubController, Context]('/sub', mut sub)!
		mut hidden := &HiddenByOther{}

		mut app := &App{}
		app.register_controller[Other, Context]('/other', mut other)!
		// controllers should be sorted, so this controller should be accessible
		// even though it is declared last
		app.register_controller[HiddenByOther, Context]('/other/hide', mut hidden)!

		vweb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip) or {
			panic('could not start vweb app')
		}
	}()
	// app startup time
	time.sleep(time.second * 10)

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_app_home() {
	x := http.get(localserver)!
	assert x.body == 'from app'
}

fn test_other() {
	x := http.get('${localserver}/other')!
	assert x.body == 'from other'
}

fn test_sub_controller() {
	x := http.get('${localserver}/other/sub')!
	assert x.body == 'from sub'
}

fn test_hidden_route() {
	x := http.get('${localserver}/other/hide')!
	assert x.body == 'from hidden'
}

fn test_conflicting_controllers() {
	mut other := &Other{}

	mut app := &App{}
	app.register_controller[Other, Context]('/other', mut other) or {
		assert true == false, 'this should not fail'
	}

	app.register_controller[Other, Context]('/other', mut other) or {
		assert true == false, 'this should not fail'
	}

	vweb.run_at[App, Context](mut app, port: port) or {
		assert err.msg() == 'conflicting paths: duplicate controller handling the route "/other"'
		return
	}
	assert true == false, 'the previous call should have failed!'
}

fn test_conflicting_controller_routes() {
	mut other := &Other{}

	mut app := &App{}
	app.register_controller[Other, Context]('/conflict', mut other) or {
		assert true == false, 'this should not fail'
	}

	vweb.run_at[App, Context](mut app, port: port) or {
		assert err.msg() == 'conflicting paths: method "conflicting" with route "/conflict/test" should be handled by the Controller of path "/conflict"'
		return
	}
	assert true == false, 'the previous call should have failed!'
}
