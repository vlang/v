module veb

import os
import time

// Helpers for conditional and range Static file responses, ported from
// Tuntii/viltrum `http1/file.v` (`ETag`, `If-None-Match` / `If-Modified-Since`,
// single `Range`). The validators describe the identity (uncompressed) body.

// HttpRange is an inclusive, zero-based byte range of a representation body.
pub struct HttpRange {
pub:
	start i64
	end   i64
}

// static_file_etag builds a strong validator for a static file body from its
// modification time and size. Any byte change to a static file also changes
// at least one of the two, so the tag only matches identical bodies.
pub fn static_file_etag(mtime i64, size i64) string {
	return '"${mtime}-${size}"'
}

// etag_matches reports whether an If-None-Match or If-Range header value
// matches etag. `*` matches anything, `W/` prefixes are stripped (weak
// comparison), entries are comma-separated, anything else must match exactly.
pub fn etag_matches(header_value string, etag string) bool {
	want := etag.trim('"')
	for entry in header_value.split(',') {
		mut candidate := entry.trim_space()
		if candidate == '*' {
			return true
		}
		if candidate.starts_with('W/') {
			candidate = candidate[2..]
		}
		if candidate.trim('"') == want {
			return true
		}
	}
	return false
}

// static_not_modified_since reports whether an If-Modified-Since value is at
// least as recent as the file mtime (second granularity). Unparseable values
// never count as unmodified, so the caller serves the full response.
pub fn static_not_modified_since(header_value string, mtime i64) bool {
	since := time.parse_http_header_string(header_value.trim_space()) or { return false }
	return since.unix() >= mtime
}

// parse_range_int parses a non-negative decimal range bound.
fn parse_range_int(s string) ?i64 {
	if s == '' {
		return none
	}
	for c in s.bytes() {
		if !c.is_digit() {
			return none
		}
	}
	return s.i64()
}

// parse_range_header parses a single `Range` header value against a body of
// size bytes. It returns none for values the server must ignore (not a
// bytes-range, multiple ranges, malformed bounds). Whether the range is
// satisfiable (start < size) is checked by the caller, which answers 416 for
// out-of-bounds ranges.
pub fn parse_range_header(header_value string, size i64) ?HttpRange {
	spec := header_value.trim_space()
	if !spec.starts_with('bytes=') {
		return none
	}
	ranges := spec.all_after('=').trim_space()
	if ranges == '' || ranges.contains(',') {
		return none
	}
	bounds := ranges.split('-')
	if bounds.len != 2 {
		return none
	}
	first := bounds[0].trim_space()
	last := bounds[1].trim_space()
	if first == '' {
		// Suffix range: the last N bytes.
		suffix := parse_range_int(last) or { return none }
		if suffix <= 0 {
			return none
		}
		start := if suffix >= size { i64(0) } else { size - suffix }
		return HttpRange{
			start: start
			end:   size - 1
		}
	}
	start := parse_range_int(first) or { return none }
	if last == '' {
		return HttpRange{
			start: start
			end:   size - 1
		}
	}
	mut end := parse_range_int(last) or { return none }
	if end < start {
		return none
	}
	if end >= size {
		end = size - 1
	}
	return HttpRange{
		start: start
		end:   end
	}
}

// if_range_holds reports whether an If-Range value still matches the current
// representation, identified by etag and mtime. An absent value holds; an
// ETag value is compared weakly; a date value must be at least as recent as
// the file; anything else (including an unparseable date) fails, so the
// caller serves the full 200 response instead of a stale range.
pub fn if_range_holds(header_value string, etag string, mtime i64) bool {
	v := header_value.trim_space()
	if v == '' {
		return true
	}
	if v.starts_with('"') || v.starts_with('W/') {
		return etag_matches(v, etag)
	}
	modified := time.parse_http_header_string(v) or { return false }
	return modified.unix() >= mtime
}

// send_not_modified answers a conditional static request with 304 Not Modified.
fn (mut ctx Context) send_not_modified(etag string, last_modified string) Result {
	ctx.res.set_status(.not_modified)
	ctx.res.header.set(.etag, etag)
	ctx.res.header.set(.last_modified, last_modified)
	return ctx.send_response_to_client('text/plain', '')
}

// send_range_not_satisfiable answers with 416 and the bounds that would satisfy.
fn (mut ctx Context) send_range_not_satisfiable(file_size i64) Result {
	ctx.res.set_status(.requested_range_not_satisfiable)
	ctx.res.header.set(.content_range, 'bytes */${file_size}')
	return ctx.send_response_to_client('text/plain', '')
}

// send_file_range serves rng bytes of the file at file_path as a 206 Partial
// Content response. Only in-memory callers use it: the streaming transports
// have no offset support, so ranges there fall back to the full 200 body.
fn (mut ctx Context) send_file_range(content_type string, file_path string, rng HttpRange, etag string, last_modified string) Result {
	data := os.read_file(file_path) or { return ctx.server_error('could not read resource') }
	size := i64(data.len)
	mut end := rng.end
	if end >= size {
		end = size - 1
	}
	if rng.start > end {
		return ctx.send_range_not_satisfiable(size)
	}
	part := data[int(rng.start)..int(end) + 1]
	ctx.res.set_status(.partial_content)
	ctx.res.header.set(.etag, etag)
	ctx.res.header.set(.last_modified, last_modified)
	ctx.res.header.set(.accept_ranges, 'bytes')
	ctx.res.header.set(.content_range, 'bytes ${rng.start}-${end}/${size}')
	ctx.res.header.set(.content_length, (end - rng.start + 1).str())
	// A 206 response describes identity bytes; never re-compress it downstream.
	ctx.already_compressed = true
	return ctx.send_response_to_client(content_type, part)
}
