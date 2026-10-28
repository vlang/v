module veb

import net.http
import os

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
		'bytes=--', 'bytes'] {
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
	assert if_range_holds('Wed, 21 Oct 2026 07:28:00 GMT', etag, mtime) == true
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
	mut ctx := &Context{
		req: conditional_test_request(headers)
	}
	if use_takeover {
		ctx.takeover_mode = .manual
	}
	ctx.send_file('text/plain', path)
	return ctx
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
