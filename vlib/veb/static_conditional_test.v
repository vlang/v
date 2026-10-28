module veb

import net.http
import os
import time

// Tests for the static conditional/range support in static_conditional.v and
// send_file. The send_file cases drive the real function in-process (no
// network, no server): streaming responses keep return_type == .file while
// status, headers and bodies assert the full wiring.

// ---------- pure helper tests ----------

fn test_etag_matches_exact_and_list() {
	etag := '"10-20"'
	assert etag_matches(etag, etag) == true
	assert etag_matches('"other", "10-20"', etag) == true
	assert etag_matches('"other"', etag) == false
}

fn test_etag_matches_star_and_weak() {
	etag := '"10-20"'
	assert etag_matches('*', etag) == true
	assert etag_matches('W/"10-20"', etag) == true
	assert etag_matches('W/"other"', etag) == false
}

fn test_parse_range_header_forms() {
	r1 := parse_range_header('bytes=0-99', 1000) or { panic('none') }
	assert r1.start == 0 && r1.end == 99
	r2 := parse_range_header('bytes=900-', 1000) or { panic('none') }
	assert r2.start == 900 && r2.end == 999
	r3 := parse_range_header('bytes=-100', 1000) or { panic('none') }
	assert r3.start == 900 && r3.end == 999
	r4 := parse_range_header('bytes=-2000', 1000) or { panic('none') }
	assert r4.start == 0 && r4.end == 999
	r5 := parse_range_header('bytes=500-9999', 1000) or { panic('none') }
	assert r5.start == 500 && r5.end == 999
}

fn test_parse_range_header_invalid_is_ignored() {
	for bad in ['', 'bytes=', 'items=0-1', 'bytes=0-1,2-3', 'bytes=abc-def', 'bytes=5-3', 'bytes=-0',
		'bytes=--', 'bytes', 'bytes=18446744073709551615-', 'bytes=-18446744073709551615'] {
		if _ := parse_range_header(bad, 1000) {
			assert false, 'range `${bad}` must be ignored'
		}
	}
}

fn test_static_not_modified_since() {
	mtime := i64(1_700_000_000)
	assert static_not_modified_since('Wed, 21 Oct 2026 07:28:00 GMT', mtime) == true
	assert static_not_modified_since('Tue, 01 Jan 1980 00:00:00 GMT', mtime) == false
	assert static_not_modified_since('not a date', mtime) == false
}

fn test_if_range_holds() {
	etag := '"10-20"'
	mtime := i64(1_700_000_000)
	assert if_range_holds('', etag, mtime) == true
	assert if_range_holds(etag, etag, mtime) == true
	assert if_range_holds('"other"', etag, mtime) == false
	assert !if_range_holds(time.unix(mtime).http_header_string(), etag, mtime)
	assert !if_range_holds('Wed, 21 Oct 2026 07:28:00 GMT', etag, mtime)
	assert !if_range_holds('W/' + etag, etag, mtime)
	assert !if_range_holds(etag + ', "other"', etag, mtime)
	assert !if_range_holds('*', etag, mtime)
	assert !if_range_holds(etag, 'W/' + etag, mtime)
	assert if_range_holds('Tue, 01 Jan 1980 00:00:00 GMT', etag, mtime) == false
	assert if_range_holds('garbage', etag, mtime) == false
}

// ---------- send_file integration tests (in-process) ----------

fn conditional_fixture() string {
	path := os.join_path(os.temp_dir(), 'veb_conditional_test_fixture.txt')
	os.write_file(path, '0123456789abcde') or { panic(err) }
	return path
}

fn conditional_test_request(headers map[string]string) http.Request {
	mut all := {
		'Host': '127.0.0.1'
	}
	for k, v in headers {
		all[k] = v
	}
	return http.Request{
		method: .get
		url:    '/file.txt'
		header: http.new_custom_header_from_map(all) or { panic(err) }
	}
}

fn conditional_send_file(path string, headers map[string]string, use_takeover bool) &Context {
	return conditional_send_file_method(path, headers, use_takeover, .get)
}

fn conditional_send_file_method(path string, headers map[string]string, use_takeover bool, method http.Method) &Context {
	mut req := conditional_test_request(headers)
	req.method = method
	mut ctx := &Context{
		req: req
	}
	if use_takeover {
		ctx.takeover_mode = .manual
	}
	ctx.send_file('text/plain', path)
	return ctx
}

fn test_static_preconditions_and_ranges_respect_request_method() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	first := conditional_send_file(path, {}, false)
	etag := first.res.header.get(.etag) or { panic('missing etag') }
	modified := first.res.header.get(.last_modified) or { panic('missing last-modified') }
	for takeover in [false, true] {
		matched := conditional_send_file_method(path, {
			'If-None-Match': etag
		}, takeover, .post)
		assert matched.res.status_code == 412
		assert matched.res.body == ''
		for headers in [{
			'If-Modified-Since': modified
		}, {
			'If-None-Match':     '"missing"'
			'If-Modified-Since': modified
		}] {
			post := conditional_send_file_method(path, headers, takeover, .post)
			assert post.res.status_code == 200
		}
	}
	for method in [http.Method.head, .post] {
		ctx := conditional_send_file_method(path, {
			'Range': 'bytes=1-3'
		}, true, method)
		assert ctx.res.status_code == 200
		assert !ctx.res.header.contains(.content_range)
		assert ctx.res.header.get(.content_length) or { '' } == '15'
	}
	head := conditional_send_file_method(path, {
		'If-None-Match': etag
	}, true, .head)
	assert head.res.status_code == 304
	assert !head.res.header.contains(.content_length)
}

fn test_static_uncompressed_carries_validators() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	ctx := conditional_send_file(path, {}, false)
	assert ctx.res.status_code == 200
	assert ctx.return_type == .file
	etag := ctx.res.header.get(.etag) or { '' }
	assert etag.starts_with('"') && etag.ends_with('"')
	last_modified := ctx.res.header.get(.last_modified) or { '' }
	assert last_modified.ends_with('GMT')
	assert (ctx.res.header.get(.accept_ranges) or { '' }) == 'bytes'
}

fn test_static_if_none_match_returns_304() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	first := conditional_send_file(path, {}, false)
	etag := first.res.header.get(.etag) or { panic('missing etag') }
	ctx := conditional_send_file(path, {
		'If-None-Match': etag
	}, false)
	assert ctx.res.status_code == 304
	assert ctx.res.body == ''
	assert (ctx.res.header.get(.etag) or { '' }) == etag
	assert !ctx.res.header.contains(.content_length)
}

fn test_static_if_modified_since_returns_304() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	first := conditional_send_file(path, {}, false)
	last_modified := first.res.header.get(.last_modified) or { panic('missing last-modified') }
	ctx := conditional_send_file(path, {
		'If-Modified-Since': last_modified
	}, false)
	assert ctx.res.status_code == 304
}

fn test_static_stale_validators_serve_full_body() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	ctx := conditional_send_file(path, {
		'If-None-Match':     '"0-0"'
		'If-Modified-Since': 'Tue, 01 Jan 1980 00:00:00 GMT'
	}, false)
	assert ctx.res.status_code == 200
	// If-None-Match takes precedence: a miss ignores If-Modified-Since entirely.
	ctx2 := conditional_send_file(path, {
		'If-None-Match':     '"0-0"'
		'If-Modified-Since': 'Wed, 21 Oct 2026 07:28:00 GMT'
	}, false)
	assert ctx2.res.status_code == 200
}

fn test_static_takeover_range_returns_206() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	ctx := conditional_send_file(path, {
		'Range': 'bytes=0-3'
	}, true)
	assert ctx.res.status_code == 206
	assert ctx.res.body == '0123'
	assert (ctx.res.header.get(.content_range) or { '' }) == 'bytes 0-3/15'
	assert (ctx.res.header.get(.content_length) or { '' }) == '4'
	suffix := conditional_send_file(path, {
		'Range': 'bytes=-5'
	}, true)
	assert suffix.res.status_code == 206
	assert suffix.res.body == 'abcde'
	assert (suffix.res.header.get(.content_range) or { '' }) == 'bytes 10-14/15'
}

fn test_static_unsatisfiable_range_returns_416() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	ctx := conditional_send_file(path, {
		'Range': 'bytes=99-'
	}, true)
	assert ctx.res.status_code == 416
	assert (ctx.res.header.get(.content_range) or { '' }) == 'bytes */15'
}

fn test_static_range_falls_back_to_200_when_unsupported() {
	path := conditional_fixture()
	defer {
		os.rm(path) or {}
	}
	// Streaming transports have no offset support: full body, still legal.
	full := conditional_send_file(path, {
		'Range': 'bytes=0-3'
	}, false)
	assert full.res.status_code == 200
	// A stale If-Range also falls back to the full body.
	stale := conditional_send_file(path, {
		'Range':    'bytes=0-3'
		'If-Range': '"0-0"'
	}, true)
	assert stale.res.status_code == 200
	assert stale.res.body == '0123456789abcde'
}

fn test_static_etag_changes_for_same_size_and_mtime_rewrite() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	mtime := os.file_last_mod_unix(path)
	first := conditional_send_file(path, {}, false)
	old_tag := first.res.header.get(.etag) or { panic('missing etag') }
	os.write_file(path, 'edcba9876543210') or { panic(err) }
	os.utime(path, mtime, mtime) or { panic(err) }
	assert os.file_last_mod_unix(path) == mtime
	ctx := conditional_send_file(path, {
		'If-None-Match': old_tag
	}, false)
	assert ctx.res.status_code == 200
	assert ctx.res.header.get(.etag) or { '' } != old_tag
}

fn test_static_if_range_requires_exact_strong_validator() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	first := conditional_send_file(path, {}, false)
	tag := first.res.header.get(.etag) or { panic('missing etag') }
	for invalid in ['W/' + tag, tag + ', "other"', '*', 'Wed, 21 Oct 2099 07:28:00 GMT'] {
		ctx := conditional_send_file(path, {
			'Range':    'bytes=0-3'
			'If-Range': invalid
		}, true)
		assert ctx.res.status_code == 200
		assert ctx.res.body == '0123456789abcde'
	}
	ctx := conditional_send_file(path, {
		'Range':    'bytes=0-3'
		'If-Range': tag
	}, true)
	assert ctx.res.status_code == 206
	assert ctx.res.body == '0123'
}

fn test_identity_validator_does_not_short_circuit_compressed_response() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	identity := conditional_send_file(path, {}, false)
	tag := identity.res.header.get(.etag) or { panic('missing etag') }
	mut ctx := &Context{
		req:                         conditional_test_request({
			'Accept-Encoding': 'gzip'
			'If-None-Match':   tag
		})
		enable_static_gzip:          true
		static_compression_max_size: 1_000_000
	}
	ctx.send_file('text/plain', path)
	assert ctx.res.status_code == 200
	assert ctx.res.header.get(.content_encoding) or { '' } == 'gzip'
	assert !ctx.res.header.contains(.etag)
}

fn test_static_vary_preserves_origin_for_identity_and_not_modified() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	mut first := &Context{
		req: conditional_test_request({})
	}
	first.res.header.set(.vary, 'Origin')
	first.send_file('text/plain', path)
	assert first.res.header.values(.vary).join(',') == 'Origin,Accept-Encoding'
	etag := first.res.header.get(.etag) or { panic('missing etag') }
	mut cached := &Context{
		req: conditional_test_request({
			'If-None-Match': 'W/' + etag
		})
	}
	cached.res.header.set(.vary, 'Origin')
	cached.res.header.set(.content_length, '999')
	cached.send_file('text/plain', path)
	assert cached.res.status_code == 304
	assert !cached.res.header.contains(.content_length)
	assert cached.res.header.values(.vary).join(',') == 'Origin,Accept-Encoding'
}

fn test_static_if_range_date_cannot_validate_same_second_rewrite() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	mtime := os.file_last_mod_unix(path)
	first := conditional_send_file(path, {}, true)
	last_modified := first.res.header.get(.last_modified) or { panic('missing date') }
	os.write_file(path, 'edcba9876543210') or { panic(err) }
	os.utime(path, mtime, mtime) or { panic(err) }
	ctx := conditional_send_file(path, {
		'Range':    'bytes=0-3'
		'If-Range': last_modified
	}, true)
	assert ctx.res.status_code == 200
	assert ctx.res.body == 'edcba9876543210'
}

fn test_static_takeover_not_modified_keeps_encoding_validator_weak() {
	path := conditional_fixture()
	defer { os.rm(path) or {} }
	first := conditional_send_file(path, {}, true)
	etag := first.res.header.get(.etag) or { panic('missing etag') }
	ctx := conditional_send_file(path, {
		'If-None-Match':   'W/' + etag
		'Accept-Encoding': 'gzip'
	}, true)
	assert ctx.res.status_code == 304
	assert ctx.res.header.get(.etag) or { '' } == 'W/' + etag
	assert !ctx.res.header.contains(.content_length)
	assert ctx.res.body == ''
}
