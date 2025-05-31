import veb
import net.http
import os
import time

const port = 13003

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

pub struct App {
	veb.StaticHandler
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('Hello V!')
}

@[post]
pub fn (mut app App) post_request(mut ctx Context) veb.Result {
	return ctx.text(ctx.req.data)
}

pub struct Context {
	veb.Context
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()

	run_app_test()
}

fn run_app_test() {
	mut app := &App{}
	if _ := app.handle_static('testdata', true) {
		assert true == false, 'should throw unknown mime type error'
	} else {
		assert err.msg().starts_with('unknown MIME type for file extension ".what"'), 'throws error on unknown mime type'
	}

	app.static_mime_types['.what'] = veb.mime_types['.txt']

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

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip)
	// app startup time
	_ := <-app.started
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

fn test_index_subdirs() {
	x := http.get('${localserver}/sub_folder/')!
	y := http.get('${localserver}/sub.folder/sub_folder')!

	assert x.status() == .ok
	assert x.body.trim_space() == 'OK'

	assert y.status() == .ok
	assert y.body.trim_space() == 'OK'
}

fn test_custom_mime_types() {
	x := http.get('${localserver}/unknown_mime.what')!

	assert x.status() == .ok
	assert x.header.get(.content_type)! == veb.mime_types['.txt']
	assert x.body.trim_space() == 'unknown_mime'
}

fn test_custom_folder_mount() {
	x := http.get('${localserver}/static/root.txt')!

	assert x.status() == .ok
	assert x.body == 'root'
}

fn test_upper_case_mime_type() {
	x := http.get('${localserver}/upper_case.TXT')!

	assert x.status() == .ok
	assert x.body == 'body'
}
