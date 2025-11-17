import veb
import net.http
import os
import time
import compress.gzip

const port = 14013
const port_no_auto = 14014 // Port for static_gzip_max_size = 0 test

const localserver = 'http://127.0.0.1:${port}'
const localserver_no_auto = 'http://127.0.0.1:${port_no_auto}'

const exit_after = time.second * 30

const test_file_content = 'This is a test file for gzip compression. It contains enough text to make compression worthwhile. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'

pub struct App {
	veb.StaticHandler
	veb.Middleware[Context]
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('Hello V!')
}

pub struct Context {
	veb.Context
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!

	// Create test directory and files
	os.mkdir_all('testdata_gzip')!
	os.write_file('testdata_gzip/test.txt', test_file_content)!
	os.write_file('testdata_gzip/large.txt', test_file_content.repeat(100))!

	// Create readonly directory and file for readonly filesystem test
	os.mkdir_all('testdata_gzip/readonly')!
	os.write_file('testdata_gzip/readonly/readonly.txt', 'This is a readonly file test')!

	// Create pre-compressed file for manual .gz test
	large_content := 'X'.repeat(2000)
	os.write_file('testdata_gzip/precompressed.txt', large_content)!
	compressed := gzip.compress(large_content.bytes()) or { panic(err) }
	os.write_file('testdata_gzip/precompressed.txt.gz', compressed.bytestr())!

	// Create file for testing max_size = 0 (no auto-compression)
	os.write_file('testdata_gzip/no_auto.txt', 'This file should not be auto-compressed')!

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()

	run_app_test()
	run_no_auto_compression_test()
}

fn testsuite_end() {
	// Clean up test files
	os.rmdir_all('testdata_gzip') or {}
}

fn run_app_test() {
	mut app := &App{}

	// Enable static gzip compression
	app.enable_static_gzip = true
	app.static_gzip_max_size = 1048576 // 1MB

	app.handle_static('testdata_gzip', true) or { panic(err) }

	// Add gzip middleware
	app.use(veb.encode_gzip[Context]())

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 25, family: .ip)
	_ := <-app.started
}

fn run_no_auto_compression_test() {
	mut app := &App{}

	// Enable static gzip but disable auto-compression (max_size = 0)
	app.enable_static_gzip = true
	app.static_gzip_max_size = 0 // Disable auto-compression

	app.handle_static('testdata_gzip', true) or { panic(err) }

	// Add gzip middleware
	app.use(veb.encode_gzip[Context]())

	spawn veb.run_at[App, Context](mut app,
		port:               port_no_auto
		timeout_in_seconds: 25
		family:             .ip
	)
	_ := <-app.started
}

fn test_gzip_compression_with_accept_encoding() {
	// Request with Accept-Encoding: gzip
	mut req := http.new_request(.get, '${localserver}/test.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	assert x.status() == .ok
	assert x.header.get(.content_encoding)! == 'gzip'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	// Verify Content-Length header matches actual body size
	content_length := x.header.get(.content_length)!.int()
	assert content_length == x.body.len, 'Content-Length should match actual body size'

	// Verify the body is compressed
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_no_compression_without_accept_encoding() {
	// Request without Accept-Encoding header
	x := http.get('${localserver}/test.txt')!

	assert x.status() == .ok
	// Should not have content-encoding header when client doesn't accept gzip
	_ := x.header.get(.content_encoding) or {
		// Expected: no content-encoding header
		assert x.body == test_file_content
		return
	}
	assert false, 'should not compress without Accept-Encoding: gzip'
}

fn test_gz_file_cache_creation() {
	// First request creates .gz cache file
	mut req := http.new_request(.get, '${localserver}/test.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	_ := req.do()!

	// Check that .gz file was created
	gz_path := 'testdata_gzip/test.txt.gz'
	assert os.exists(gz_path), '.gz cache file should be created'

	// Second request should use cached .gz file
	y := req.do()!
	assert y.status() == .ok
	assert y.header.get(.content_encoding)! == 'gzip'

	// Verify Content-Length matches .gz file size (tests os.file_size() code path)
	gz_file_size := os.file_size(gz_path)
	content_length := y.header.get(.content_length)!.u64()
	assert content_length == gz_file_size, 'Content-Length should match .gz file size'
}

fn test_large_file_not_auto_compressed() {
	// Configure app with very small max size to test threshold
	// The large.txt file is ~20KB (200 chars * 100), which exceeds a 1KB threshold
	// But we set it to 1MB, so it should still be compressed
	// Let's test by checking if it gets compressed

	mut req := http.new_request(.get, '${localserver}/large.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	assert x.status() == .ok
	// File should be compressed as it's under 1MB threshold
	assert x.header.get(.content_encoding)! == 'gzip'
}

fn test_already_compressed_flag() {
	// Request a file that will be compressed and cached
	mut req := http.new_request(.get, '${localserver}/test.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	assert x.status() == .ok
	// The file should be compressed only once (in send_file, not by middleware)
	// We can't directly test the already_compressed flag, but we can verify
	// that the response is valid gzip
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'response should be valid gzip: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_readonly_filesystem_fallback() {
	// Test that compression works even on readonly filesystems (fallback to memory)
	// Skip on Windows as readonly permissions work differently (ACL vs chmod)
	$if windows {
		eprintln('Skipping readonly filesystem test on Windows')
		return
	}

	// Make readonly directory readonly (no write permissions)
	readonly_dir := 'testdata_gzip/readonly'
	readonly_file := '${readonly_dir}/readonly.txt'

	os.chmod(readonly_dir, 0o555)! // r-xr-xr-x

	mut req := http.new_request(.get, '${localserver}/readonly/readonly.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	// Restore permissions before assertions (for cleanup)
	os.chmod(readonly_dir, 0o755) or {} // rwxr-xr-x

	assert x.status() == .ok
	// Should be compressed (served from memory as fallback)
	assert x.header.get(.content_encoding)! == 'gzip'

	// Verify that .gz file was NOT created (readonly filesystem)
	gz_path := '${readonly_file}.gz'
	assert !os.exists(gz_path), '.gz cache file should not be created on readonly filesystem'

	// Verify content is valid gzip
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'response should be valid gzip even on readonly fs: ${err}'
		return
	}
	assert decompressed.bytestr() == 'This is a readonly file test'
}

fn test_precompressed_gz_file_served() {
	// Test that manually pre-compressed .gz files are always served
	// This validates the manual pre-compression workflow (useful with static_gzip_max_size = 0)

	// Request the pre-compressed file
	mut req := http.new_request(.get, '${localserver}/precompressed.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	assert x.status() == .ok
	// Should serve the manually pre-compressed .gz file
	assert x.header.get(.content_encoding)! == 'gzip'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	// Verify it's the pre-compressed content
	large_content := 'X'.repeat(2000)
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'manual .gz should be valid: ${err}'
		return
	}
	assert decompressed.bytestr() == large_content
}

fn test_no_auto_compression_with_max_size_zero() {
	// Test that static_gzip_max_size = 0 disables auto-compression
	// but still serves manually pre-compressed .gz files

	// 1. Verify manually pre-compressed .gz files are still served
	mut req1 := http.new_request(.get, '${localserver_no_auto}/precompressed.txt', '')
	req1.add_header(.accept_encoding, 'gzip')
	x := req1.do()!

	assert x.status() == .ok
	// Should serve the manually pre-compressed .gz file
	assert x.header.get(.content_encoding)! == 'gzip'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	large_content := 'X'.repeat(2000)
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'manual .gz should be valid with max_size=0: ${err}'
		return
	}
	assert decompressed.bytestr() == large_content

	// 2. Verify auto-compression is disabled for files without .gz
	mut req2 := http.new_request(.get, '${localserver_no_auto}/no_auto.txt', '')
	req2.add_header(.accept_encoding, 'gzip')
	y := req2.do()!

	assert y.status() == .ok
	// Should NOT have content-encoding header (no auto-compression)
	_ := y.header.get(.content_encoding) or {
		// Expected: no content-encoding header
		assert y.body == 'This file should not be auto-compressed'
		return
	}
	assert false, 'should not auto-compress with static_gzip_max_size = 0'

	// 3. Verify that .gz file was NOT created
	gz_path := 'testdata_gzip/no_auto.txt.gz'
	assert !os.exists(gz_path), '.gz cache file should not be created with max_size = 0'
}
