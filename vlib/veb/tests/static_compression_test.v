// vtest build: !docker-ubuntu-musl // failing assert static_compression_test.v:234 -> `.gz cache file should not be created on readonly filesystem`, because of the Docker container
import veb
import net.http
import os
import time
import compress.gzip
import compress.zstd

const port = 14013
const port_no_auto = 14014 // Port for static_compression_max_size = 0 test
const port_gzip_only = 14015 // Port for enable_static_gzip only test
const port_zstd_only = 14016 // Port for enable_static_zstd only test

const localserver = 'http://127.0.0.1:${port}'
const localserver_no_auto = 'http://127.0.0.1:${port_no_auto}'
const localserver_gzip_only = 'http://127.0.0.1:${port_gzip_only}'
const localserver_zstd_only = 'http://127.0.0.1:${port_zstd_only}'

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
	os.mkdir_all('testdata_compression')!
	os.write_file('testdata_compression/test.txt', test_file_content)!
	os.write_file('testdata_compression/large.txt', test_file_content.repeat(100))!

	// Create readonly directory and file for readonly filesystem test
	os.mkdir_all('testdata_compression/readonly')!
	os.write_file('testdata_compression/readonly/readonly.txt', 'This is a readonly file test')!

	// Create pre-compressed file for manual .gz test
	large_content := 'X'.repeat(2000)
	os.write_file('testdata_compression/precompressed.txt', large_content)!
	compressed_gz := gzip.compress(large_content.bytes()) or { panic(err) }
	os.write_file('testdata_compression/precompressed.txt.gz', compressed_gz.bytestr())!

	// Create pre-compressed file for manual .zst test
	os.write_file('testdata_compression/precompressed_zstd.txt', large_content)!
	compressed_zst := zstd.compress(large_content.bytes()) or { panic(err) }
	os.write_file('testdata_compression/precompressed_zstd.txt.zst', compressed_zst.bytestr())!

	// Create test file for zstd auto-compression
	os.write_file('testdata_compression/zstd_test.txt', test_file_content)!

	// Create file for testing max_size = 0 (no auto-compression)
	os.write_file('testdata_compression/no_auto.txt', 'This file should not be auto-compressed')!

	// Create files for gzip-only and zstd-only tests
	os.write_file('testdata_compression/gzip_only_test.txt', test_file_content)!
	os.write_file('testdata_compression/zstd_only_test.txt', test_file_content)!

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()

	run_app_test()
	run_no_auto_compression_test()
	run_gzip_only_test()
	run_zstd_only_test()
}

fn testsuite_end() {
	// Clean up test files
	os.rmdir_all('testdata_compression') or {}
}

fn run_app_test() {
	mut app := &App{}

	// Enable static compression (zstd/gzip)
	app.enable_static_compression = true
	app.static_compression_max_size = 1048576 // 1MB

	app.handle_static('testdata_compression', true) or { panic(err) }

	// Add compression middleware (gzip for this test app)
	app.use(veb.encode_gzip[Context]())

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 25, family: .ip)
	_ := <-app.started
}

fn run_no_auto_compression_test() {
	mut app := &App{}

	// Enable static compression but disable auto-compression (max_size = 0)
	app.enable_static_compression = true
	app.static_compression_max_size = 0 // Disable auto-compression

	app.handle_static('testdata_compression', true) or { panic(err) }

	// Add compression middleware (gzip for this test app)
	app.use(veb.encode_gzip[Context]())

	spawn veb.run_at[App, Context](mut app,
		port:               port_no_auto
		timeout_in_seconds: 25
		family:             .ip
	)
	_ := <-app.started
}

fn run_gzip_only_test() {
	mut app := &App{}

	// Enable ONLY gzip compression (not zstd, not auto)
	app.enable_static_gzip = true
	app.static_compression_max_size = 1048576 // 1MB

	app.handle_static('testdata_compression', true) or { panic(err) }

	spawn veb.run_at[App, Context](mut app,
		port:               port_gzip_only
		timeout_in_seconds: 25
		family:             .ip
	)
	_ := <-app.started
}

fn run_zstd_only_test() {
	mut app := &App{}

	// Enable ONLY zstd compression (not gzip, not auto)
	app.enable_static_zstd = true
	app.static_compression_max_size = 1048576 // 1MB

	app.handle_static('testdata_compression', true) or { panic(err) }

	spawn veb.run_at[App, Context](mut app,
		port:               port_zstd_only
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
	gz_path := 'testdata_compression/test.txt.gz'
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
	readonly_dir := 'testdata_compression/readonly'
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

fn test_readonly_filesystem_fallback_zstd() {
	// Test that zstd compression works even on readonly filesystems (fallback to memory)
	// Skip on Windows as readonly permissions work differently (ACL vs chmod)
	$if windows {
		eprintln('Skipping readonly filesystem test on Windows')
		return
	}

	// Make readonly directory readonly (no write permissions)
	readonly_dir := 'testdata_compression/readonly'
	readonly_file := '${readonly_dir}/readonly.txt'

	os.chmod(readonly_dir, 0o555)! // r-xr-xr-x

	mut req := http.new_request(.get, '${localserver}/readonly/readonly.txt', '')
	req.add_header(.accept_encoding, 'zstd')
	x := req.do()!

	// Restore permissions before assertions (for cleanup)
	os.chmod(readonly_dir, 0o755) or {} // rwxr-xr-x

	assert x.status() == .ok
	// Should be compressed (served from memory as fallback)
	assert x.header.get(.content_encoding)! == 'zstd'

	// Verify that .zst file was NOT created (readonly filesystem)
	zst_path := '${readonly_file}.zst'
	assert !os.exists(zst_path), '.zst cache file should not be created on readonly filesystem'

	// Verify content is valid zstd
	decompressed := zstd.decompress(x.body.bytes()) or {
		assert false, 'response should be valid zstd even on readonly fs: ${err}'
		return
	}
	assert decompressed.bytestr() == 'This is a readonly file test'
}

fn test_precompressed_gz_file_served() {
	// Test that manually pre-compressed .gz files are always served
	// This validates the manual pre-compression workflow (useful with static_compression_max_size = 0)

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
	// Test that static_compression_max_size = 0 disables auto-compression
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
	assert false, 'should not auto-compress with static_compression_max_size = 0'

	// 3. Verify that .gz file was NOT created
	gz_path := 'testdata_compression/no_auto.txt.gz'
	assert !os.exists(gz_path), '.gz cache file should not be created with max_size = 0'
}

// Zstd tests

fn test_zstd_preferred_over_gzip() {
	// When client supports both zstd and gzip, zstd should be preferred
	mut req := http.new_request(.get, '${localserver}/zstd_test.txt', '')
	req.add_header(.accept_encoding, 'gzip, zstd, br')
	x := req.do()!

	assert x.status() == .ok
	assert x.header.get(.content_encoding)! == 'zstd', 'zstd should be preferred over gzip'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	// Verify the body is valid zstd
	decompressed := zstd.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress zstd response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_zst_file_cache_creation() {
	// First request should create .zst cache file
	mut req := http.new_request(.get, '${localserver}/zstd_test.txt', '')
	req.add_header(.accept_encoding, 'zstd')
	_ := req.do()!

	// Check that .zst file was created
	zst_path := 'testdata_compression/zstd_test.txt.zst'
	assert os.exists(zst_path), '.zst cache file should be created'

	// Second request should use cached .zst file
	y := req.do()!
	assert y.status() == .ok
	assert y.header.get(.content_encoding)! == 'zstd'

	// Verify Content-Length matches .zst file size
	zst_file_size := os.file_size(zst_path)
	content_length := y.header.get(.content_length)!.u64()
	assert content_length == zst_file_size, 'Content-Length should match .zst file size'
}

fn test_precompressed_zst_file_served() {
	// Test that manually pre-compressed .zst files are served
	mut req := http.new_request(.get, '${localserver}/precompressed_zstd.txt', '')
	req.add_header(.accept_encoding, 'zstd')
	x := req.do()!

	assert x.status() == .ok
	assert x.header.get(.content_encoding)! == 'zstd'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	// Verify it's the pre-compressed content
	large_content := 'X'.repeat(2000)
	decompressed := zstd.decompress(x.body.bytes()) or {
		assert false, 'manual .zst should be valid: ${err}'
		return
	}
	assert decompressed.bytestr() == large_content
}

fn test_gzip_fallback_when_zstd_not_supported() {
	// When client only supports gzip, gzip should be used
	mut req := http.new_request(.get, '${localserver}/zstd_test.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	assert x.status() == .ok
	assert x.header.get(.content_encoding)! == 'gzip', 'should fallback to gzip when zstd not supported'

	// Verify the body is valid gzip
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress gzip response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

// Tests for enable_static_gzip only (backward compatibility)

fn test_gzip_only_serves_gzip() {
	// Test that enable_static_gzip alone works (backward compatibility)
	mut req := http.new_request(.get, '${localserver_gzip_only}/gzip_only_test.txt', '')
	req.add_header(.accept_encoding, 'gzip')
	x := req.do()!

	assert x.status() == .ok
	assert x.header.get(.content_encoding)! == 'gzip', 'gzip-only mode should serve gzip'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	// Verify the body is valid gzip
	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress gzip response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_gzip_only_ignores_zstd_request() {
	// Test that enable_static_gzip does NOT serve zstd even if client supports it
	mut req := http.new_request(.get, '${localserver_gzip_only}/gzip_only_test.txt', '')
	req.add_header(.accept_encoding, 'zstd, gzip')
	x := req.do()!

	assert x.status() == .ok
	// Should serve gzip, NOT zstd (because only enable_static_gzip is set)
	assert x.header.get(.content_encoding)! == 'gzip', 'gzip-only mode should serve gzip even when client supports zstd'

	decompressed := gzip.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress gzip response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_gzip_only_no_compression_without_gzip_header() {
	// Test that enable_static_gzip does not compress when client doesn't accept gzip
	mut req := http.new_request(.get, '${localserver_gzip_only}/gzip_only_test.txt', '')
	req.add_header(.accept_encoding, 'zstd') // Only zstd, no gzip
	x := req.do()!

	assert x.status() == .ok
	// Should not have content-encoding header (no compression)
	_ := x.header.get(.content_encoding) or {
		// Expected: no content-encoding header
		assert x.body == test_file_content
		return
	}
	assert false, 'gzip-only mode should not compress when client only accepts zstd'
}

// Tests for enable_static_zstd only

fn test_zstd_only_serves_zstd() {
	// Test that enable_static_zstd alone works
	mut req := http.new_request(.get, '${localserver_zstd_only}/zstd_only_test.txt', '')
	req.add_header(.accept_encoding, 'zstd')
	x := req.do()!

	assert x.status() == .ok
	assert x.header.get(.content_encoding)! == 'zstd', 'zstd-only mode should serve zstd'
	assert x.header.get(.vary)! == 'Accept-Encoding'

	// Verify the body is valid zstd
	decompressed := zstd.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress zstd response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_zstd_only_ignores_gzip_request() {
	// Test that enable_static_zstd does not serve gzip even if client supports it
	mut req := http.new_request(.get, '${localserver_zstd_only}/zstd_only_test.txt', '')
	req.add_header(.accept_encoding, 'gzip, zstd')
	x := req.do()!

	assert x.status() == .ok
	// Should serve zstd, not gzip (because only enable_static_zstd is set)
	assert x.header.get(.content_encoding)! == 'zstd', 'zstd-only mode should serve zstd even when client supports gzip'

	decompressed := zstd.decompress(x.body.bytes()) or {
		assert false, 'failed to decompress zstd response: ${err}'
		return
	}
	assert decompressed.bytestr() == test_file_content
}

fn test_zstd_only_no_compression_without_zstd_header() {
	// Test that enable_static_zstd does not compress when client doesn't accept zstd
	mut req := http.new_request(.get, '${localserver_zstd_only}/zstd_only_test.txt', '')
	req.add_header(.accept_encoding, 'gzip') // Only gzip, no zstd
	x := req.do()!

	assert x.status() == .ok
	// Should not have content-encoding header (no compression)
	_ := x.header.get(.content_encoding) or {
		// Expected: no content-encoding header
		assert x.body == test_file_content
		return
	}
	assert false, 'zstd-only mode should not compress when client only accepts gzip'
}
