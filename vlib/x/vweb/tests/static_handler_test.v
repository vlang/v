import x.vweb
import net.http
import os
import time

const port = 13003

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

pub struct App {
	vweb.StaticHandler
}

pub fn (mut app App) index(mut ctx Context) vweb.Result {
	return ctx.text('Hello V!')
}

@[post]
pub fn (mut app App) post_request(mut ctx Context) vweb.Result {
	return ctx.text(ctx.req.data)
}

pub struct Context {
	vweb.Context
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!
	spawn run_app_test()

	// app startup time
	time.sleep(time.second * 2)

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn run_app_test() {
	mut app := &App{}
	if _ := app.handle_static('testdata', true) {
		assert true == false, 'should throw unkown mime type error'
	} else {
		assert err.msg().starts_with('unkown MIME type for file extension ".what"'), 'throws error on unkown mime type'
	}

	app.static_mime_types['.what'] = vweb.mime_types['.txt']

	if _ := app.handle_static('not_found', true) {
		assert false, 'should throw directory not found error'
	} else {
		assert err.msg().starts_with('directory `not_found` does not exist') == true
	}

	app.handle_static('testdata', true) or { panic(err) }

	if _ := app.mount_static_folder_at('testdata', 'static') {
		assert true == false, 'should throw invalid mount path error'
	} else {
		assert err.msg() == 'invalid mount path! The path should start with `/`'
	}

	if _ := app.mount_static_folder_at('not_found', '/static') {
		assert true == false, 'should throw mount path does not exist error'
	} else {
		assert err.msg().starts_with('directory `not_found` does not exist') == true
	}

	app.mount_static_folder_at('testdata', '/static') or { panic(err) }

	vweb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip) or {
		panic('could not start vweb app')
	}
}

fn test_static_root() {
	x := http.get('${localserver}/root.txt')!

	assert x.status() == .ok
	assert x.body == 'root'
}

fn test_scans_subdirs() {
	x := http.get('${localserver}/sub_folder/sub.txt')!

	assert x.status() == .ok
	assert x.body == 'sub'
}

fn test_custom_mime_types() {
	x := http.get('${localserver}/unkown_mime.what')!

	assert x.status() == .ok
	assert x.header.get(.content_type)! == vweb.mime_types['.txt']
	assert x.body == 'unkown_mime'
}

fn test_custom_folder_mount() {
	x := http.get('${localserver}/static/root.txt')!

	assert x.status() == .ok
	assert x.body == 'root'
}
