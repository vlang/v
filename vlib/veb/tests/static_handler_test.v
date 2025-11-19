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

	// Enable markdown content negotiation for testing
	app.enable_markdown_negotiation = true

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

// Content negotiation tests - Priority order
// Tests verify: path.md > path.html.md > path/index.html.md

fn test_markdown_negotiation_priority_first() {
	// When all three variants exist, path.md (priority 1) is served
	config := http.FetchConfig{
		url:    '${localserver}/about'
		header: http.new_header(key: .accept, value: 'text/markdown')
	}
	x := http.fetch(config)!

	assert x.status() == .ok
	assert x.header.get(.content_type)! == 'text/markdown'
	assert x.body.contains('This is the about page in markdown format.')
	assert !x.body.contains('about.html.md variant')
	assert !x.body.contains('about/index.html.md variant')
}

fn test_markdown_negotiation_priority_second() {
	// When only path.html.md exists (priority 2), it is served
	config := http.FetchConfig{
		url:    '${localserver}/page'
		header: http.new_header(key: .accept, value: 'text/markdown')
	}
	x := http.fetch(config)!

	assert x.status() == .ok
	assert x.header.get(.content_type)! == 'text/markdown'
	assert x.body.contains('# Page HTML Markdown')
}

fn test_markdown_negotiation_directory_index() {
	// For directories, index.html.md is served when Accept: text/markdown
	config := http.FetchConfig{
		url:    '${localserver}/sub_folder/'
		header: http.new_header(key: .accept, value: 'text/markdown')
	}
	x := http.fetch(config)!

	assert x.status() == .ok
	assert x.header.get(.content_type)! == 'text/markdown'
	assert x.body.contains('# Index HTML Markdown')
}

// Direct access tests - Verifies backward compatibility

fn test_markdown_direct_access() {
	// Without Accept header
	x_no_header := http.get('${localserver}/test.md')!
	assert x_no_header.status() == .ok
	assert x_no_header.header.get(.content_type)! == 'text/markdown'
	assert x_no_header.body.contains('# Test Markdown')

	// With Accept: text/markdown header - same result
	config := http.FetchConfig{
		url:    '${localserver}/test.md'
		header: http.new_header(key: .accept, value: 'text/markdown')
	}
	x_with_header := http.fetch(config)!
	assert x_with_header.status() == .ok
	assert x_with_header.header.get(.content_type)! == 'text/markdown'
	assert x_with_header.body.contains('# Test Markdown')
}

fn test_markdown_variants_direct_access() {
	// All markdown variants remain accessible via their full paths
	x_html_md := http.get('${localserver}/about.html.md')!
	assert x_html_md.status() == .ok
	assert x_html_md.body.contains('about.html.md variant')

	x_index := http.get('${localserver}/about/index.html.md')!
	assert x_index.status() == .ok
	assert x_index.body.contains('about/index.html.md variant')
}

// Negative tests - Verifies correct behavior without Accept header

fn test_markdown_no_negotiation_without_header() {
	// Without Accept: text/markdown, content is not found for directories with no index.html
	x := http.get('${localserver}/about')!
	assert x.status() == .not_found
}
