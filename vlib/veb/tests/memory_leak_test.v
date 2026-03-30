// vtest build: !docker-ubuntu-musl && !sanitize-memory-clang && !sanitize-address-clang-without-gc
import os
import time
import net.http

const sport = 13085
const localserver = '127.0.0.1:${sport}'
const exit_after_time = 30000
const vexe = os.getenv('VEXE')
const vroot = os.dir(vexe)
const serverexe = os.join_path(os.cache_dir(), 'veb_memory_test_server.exe')

fn testsuite_begin() {
	os.chdir(vroot) or {}
	if os.exists(serverexe) {
		os.rm(serverexe) or {}
	}
}

fn testsuite_end() {
	if os.exists(serverexe) {
		os.rm(serverexe) or {}
	}
}

fn test_server_compiles() {
	did_compile :=
		os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(serverexe)} vlib/veb/tests/memory_leak_test_server.v')
	assert did_compile == 0
	assert os.exists(serverexe)
}

fn test_server_runs_in_background() {
	spawn os.system('${os.quoted_path(serverexe)} ${sport} ${exit_after_time}')
	for _ in 0 .. 50 {
		resp := http.get('http://${localserver}/heap') or {
			time.sleep(100 * time.millisecond)
			continue
		}
		if resp.status_code == 200 {
			return
		}
		time.sleep(100 * time.millisecond)
	}
	assert false, 'Timed out waiting for background server to start'
}

fn test_large_responses_work_correctly() {
	// Make requests with large response bodies and verify they work
	request_count := 20
	mut successful := 0
	for _ in 0 .. request_count {
		resp := http.get('http://${localserver}/large') or { continue }
		if resp.status_code == 200 && resp.body.len > 50000 {
			successful += 1
		}
	}
	// Ensure most requests succeeded (allows for some network variability)
	assert successful >= request_count - 2, 'Only ${successful}/${request_count} requests succeeded'
}

fn test_heap_usage_endpoint_works() {
	resp := http.get('http://${localserver}/heap') or {
		assert false, 'Failed to get heap usage: ${err}'
		return
	}
	heap := resp.body.i64()
	// Just verify we get a reasonable value (more than 1MB, less than 1GB)
	assert heap > 1024 * 1024, 'Heap usage ${heap} seems too low'
	assert heap < 1024 * 1024 * 1024, 'Heap usage ${heap} seems too high'
}

fn test_gc_collect_endpoint_works() {
	resp := http.get('http://${localserver}/gc') or {
		assert false, 'Failed to trigger GC: ${err}'
		return
	}
	assert resp.status_code == 200
}

fn test_multipart_upload_does_not_expand_heap_excessively() {
	http.get('http://${localserver}/gc') or {
		assert false, 'Failed to trigger GC before measuring heap: ${err}'
		return
	}
	before_resp := http.get('http://${localserver}/heap') or {
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
	resp := http.post_multipart_form('http://${localserver}/upload', http.PostMultipartFormConfig{
		files: {
			'file': files
		}
	}) or {
		assert false, 'Failed to post multipart upload: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == payload_size.str()
	http.get('http://${localserver}/gc') or {
		assert false, 'Failed to trigger GC after upload: ${err}'
		return
	}
	after_resp := http.get('http://${localserver}/heap') or {
		assert false, 'Failed to get post-upload heap usage: ${err}'
		return
	}
	after_heap := after_resp.body.i64()
	heap_growth := if after_heap > before_heap { after_heap - before_heap } else { i64(0) }
	assert heap_growth < 8 * 1024 * 1024, 'Multipart upload grew heap by ${heap_growth} bytes'
}
