// Benchmark: []u8 body vs string body conversion overhead on the server hot path.
//
// Run with: ./v test vlib/net/http/bench_body_test.v
module http

import net.http.common
import time

const bench_iterations = 5000

fn test_bench_body_round_trip_by_size() {
	sizes := [64, 1024, 16384, 65536, 262144, 1048576]

	println('--- Body round-trip benchmark (${bench_iterations} iterations per scenario) ---')
	println('      Size |       old (string round-trip) |    new ([]u8 direct) | speedup')
	println('---------- | ---------------------------- | -------------------- | -------')

	for size in sizes {
		raw := []u8{len: size, init: u8(index % 256)}

		// OLD: []u8 → .bytestr() → handler sees string → .bytes() → wire
		mut sw_old := time.new_stopwatch()
		for _ in 0 .. bench_iterations {
			body_str := raw.bytestr()
			resp_bytes := body_str.bytes()
			_ = resp_bytes.len
		}
		sw_old.pause()
		old_us := sw_old.elapsed().microseconds()

		// NEW: []u8 passed through directly — no conversion
		mut sw_new := time.new_stopwatch()
		for _ in 0 .. bench_iterations {
			body_u8 := raw
			resp_bytes := body_u8
			_ = resp_bytes.len
		}
		sw_new.pause()
		new_us := sw_new.elapsed().microseconds()

		ratio := if new_us > 0 { f64(old_us) / f64(new_us) } else { f64(0) }
		size_kb := f64(size) / 1024.0
		println('${size_kb:9.1f}K | ${old_us:26} us | ${new_us:18} us | ${ratio:7.1f}x')
	}
}

fn test_bench_server_request_construction() {
	body_data := []u8{len: 16384, init: u8(index % 256)}
	mut header := common.new_header()
	header.add(.content_type, 'application/octet-stream') or {}
	header.add(.host, 'localhost:8080') or {}

	mut sw_old := time.new_stopwatch()
	for _ in 0 .. bench_iterations {
		_ := common.ServerRequest{
			method:  .post
			path:    '/upload'
			host:    'localhost'
			header:  header
			body:    body_data.bytestr().bytes()
			version: .v2_0
		}
	}
	sw_old.pause()

	mut sw_new := time.new_stopwatch()
	for _ in 0 .. bench_iterations {
		_ := common.ServerRequest{
			method:  .post
			path:    '/upload'
			host:    'localhost'
			header:  header
			body:    body_data
			version: .v2_0
		}
	}
	sw_new.pause()

	old_us := sw_old.elapsed().microseconds()
	new_us := sw_new.elapsed().microseconds()
	ratio := if new_us > 0 { f64(old_us) / f64(new_us) } else { f64(0) }
	println('')
	println('--- ServerRequest construction (16KB body, ${bench_iterations} iter) ---')
	println('  old (bytestr + bytes): ${old_us} us')
	println('  new ([]u8 direct):     ${new_us} us')
	println('  speedup:               ${ratio:.1f}x')
}

fn test_bench_server_response_body() {
	body_data := []u8{len: 16384, init: u8(index % 256)}
	body_str := body_data.bytestr()

	mut sw_old := time.new_stopwatch()
	for _ in 0 .. bench_iterations {
		resp := common.ServerResponse{
			status_code: 200
			body:        body_str.bytes()
		}
		_ = resp.body.len
	}
	sw_old.pause()

	mut sw_new := time.new_stopwatch()
	for _ in 0 .. bench_iterations {
		resp := common.ServerResponse{
			status_code: 200
			body:        body_data
		}
		_ = resp.body.len
	}
	sw_new.pause()

	old_us := sw_old.elapsed().microseconds()
	new_us := sw_new.elapsed().microseconds()
	ratio := if new_us > 0 { f64(old_us) / f64(new_us) } else { f64(0) }
	println('')
	println('--- ServerResponse body (16KB, ${bench_iterations} iter) ---')
	println('  old (string.bytes()): ${old_us} us')
	println('  new ([]u8 direct):    ${new_us} us')
	println('  speedup:              ${ratio:.1f}x')
}
