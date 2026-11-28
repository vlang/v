// vtest flaky: true
// vtest retry: 3
// vtest build: !windows && !docker-ubuntu-musl && !sanitize-memory-clang && !sanitize-address-clang-without-gc
import net
import net.http
import os
import time
import veb

const memory_server_probe = '--vtest-veb-memory-server'

struct MemoryTestContext {
	veb.Context
}

struct MemoryTestApp {}

struct MemoryTestServer {
mut:
	process &os.Process = unsafe { nil }
	address string
}

const memory_server = &MemoryTestServer{}

fn run_memory_test_server(port int) {
	spawn fn () {
		time.sleep(30 * time.second)
		exit(1)
	}()
	mut app := &MemoryTestApp{}
	veb.run_at[MemoryTestApp, MemoryTestContext](mut app,
		host:               '127.0.0.1'
		port:               port
		family:             .ip
		timeout_in_seconds: 10
	) or { panic(err) }
}

fn testsuite_begin() {
	if os.args.len == 3 && os.args[1] == memory_server_probe {
		run_memory_test_server(os.args[2].int())
		exit(0)
	}
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	listener.close()!

	mut child := os.new_process(os.executable())
	child.set_args([memory_server_probe, port.str()])
	child.run()
	// testsuite_begin and testsuite_end share ownership of the child process.
	mut server := unsafe { memory_server }
	server.process = child
	server.address = 'http://127.0.0.1:${port}'
}

fn testsuite_end() {
	mut server := unsafe { memory_server }
	server.process.signal_kill()
	server.process.wait()
	server.process.close()
}

fn test_server_runs_in_background() {
	sw := time.new_stopwatch()
	for sw.elapsed() < 5 * time.second {
		resp := http.fetch(
			url:          '${memory_server.address}/heap'
			max_retries:  1
			read_timeout: 250 * time.millisecond
		) or {
			time.sleep(10 * time.millisecond)
			continue
		}
		if resp.status_code == 200 {
			return
		}
		time.sleep(10 * time.millisecond)
	}
	assert false, 'Timed out waiting for background server to start'
}

fn test_large_responses_work_correctly() {
	// Make requests with large response bodies and verify they work
	request_count := 20
	mut successful := 0
	for _ in 0 .. request_count {
		resp := http.get('${memory_server.address}/large') or { continue }
		if resp.status_code == 200 && resp.body.len > 50000 {
			successful += 1
		}
	}
	// Ensure most requests succeeded (allows for some network variability)
	assert successful >= request_count - 2, 'Only ${successful}/${request_count} requests succeeded'
}

fn test_heap_usage_endpoint_works() {
	resp := http.get('${memory_server.address}/heap') or {
		assert false, 'Failed to get heap usage: ${err}'
		return
	}
	heap := resp.body.i64()
	// Just verify we get a reasonable value (more than 1MB, less than 1GB)
	assert heap > 1024 * 1024, 'Heap usage ${heap} seems too low'
	assert heap < 1024 * 1024 * 1024, 'Heap usage ${heap} seems too high'
}

fn test_gc_collect_endpoint_works() {
	resp := http.get('${memory_server.address}/gc') or {
		assert false, 'Failed to trigger GC: ${err}'
		return
	}
	assert resp.status_code == 200
}

fn test_multipart_upload_does_not_expand_heap_excessively() {
	http.get('${memory_server.address}/gc') or {
		assert false, 'Failed to trigger GC before measuring heap: ${err}'
		return
	}
	before_resp := http.get('${memory_server.address}/heap') or {
		assert false, 'Failed to get baseline heap usage: ${err}'
		return
	}
	before_heap := before_resp.body.i64()
	payload_size := 1024 * 1024
	mut files := []http.FileData{}
	files << http.FileData{
		filename:     'payload.bin'
		content_type: 'application/octet-stream'
		data:         'X'.repeat(payload_size)
	}
	resp := http.post_multipart_form('${memory_server.address}/upload', http.PostMultipartFormConfig{
		files: {
			'file': files
		}
	}) or {
		assert false, 'Failed to post multipart upload: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == payload_size.str()
	http.get('${memory_server.address}/gc') or {
		assert false, 'Failed to trigger GC after upload: ${err}'
		return
	}
	after_resp := http.get('${memory_server.address}/heap') or {
		assert false, 'Failed to get post-upload heap usage: ${err}'
		return
	}
	after_heap := after_resp.body.i64()
	heap_growth := if after_heap > before_heap { after_heap - before_heap } else { i64(0) }
	assert heap_growth < 8 * 1024 * 1024, 'Multipart upload grew heap by ${heap_growth} bytes'
}

// index returns a basic response from the memory test server.
pub fn (mut app MemoryTestApp) index(mut ctx MemoryTestContext) veb.Result {
	return ctx.text('OK')
}

// heap reports the server process's GC heap size.
@['/heap']
pub fn (mut app MemoryTestApp) heap(mut ctx MemoryTestContext) veb.Result {
	usage := gc_heap_usage()
	return ctx.text('${usage.heap_size}')
}

// gc_collect triggers a collection in the server process.
@['/gc']
pub fn (mut app MemoryTestApp) gc_collect(mut ctx MemoryTestContext) veb.Result {
	gc_collect()
	return ctx.text('GC collected')
}

// large returns a response with a 100 KiB body.
@['/large']
pub fn (mut app MemoryTestApp) large(mut ctx MemoryTestContext) veb.Result {
	data := 'X'.repeat(100 * 1024)
	return ctx.text(data)
}

// upload reports the number of bytes received for the multipart file.
@['/upload'; post]
pub fn (mut app MemoryTestApp) upload(mut ctx MemoryTestContext) veb.Result {
	if 'file' !in ctx.files || ctx.files['file'].len == 0 {
		ctx.res.set_status(.bad_request)
		return ctx.text('no file')
	}
	return ctx.text(ctx.files['file'][0].data.len.str())
}
