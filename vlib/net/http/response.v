// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import compress.brotli
import compress.deflate
import compress.gzip
import compress.zlib
import net.http.chunked
import strconv
import strings

pub const default_max_total_decompressed_bytes = i64(16 * 1024 * 1024)

const max_content_encoding_layers = 8

const response_decompression_limit_error_code = -20016
const response_content_encoding_layer_limit_error_code = -20017

// Response represents the result of the request
pub struct Response {
pub mut:
	body   string
	header Header
	// trailers are emitted as a trailing HEADERS block by the HTTP/2 server
	// path (net.http.h2_server.v) after the body, e.g. for `grpc-status`.
	// Ignored by the HTTP/1.1 write path — H1 has no wire mechanism for this
	// enabled here (chunked-encoding trailers are a separate, unimplemented
	// feature), so fields set here simply do not appear on an H1 response.
	trailers     Header
	status_code  int
	status_msg   string
	http_version string
	// stream, when a `Handler` sets it, keeps the connection for the response's
	// body (HTTP/1.1): the server writes the status line and the headers, without
	// `Content-Length` and with `Connection: close`, calls `stream` on the worker
	// thread, and closes the connection when it returns. `body` is not sent. A
	// stream holds its worker while it runs, so size `worker_num` for them. Its
	// use is server-sent events (`StreamWriter.write_event`). The HTTP/2 path
	// does not stream: it sends the response without a body.
	stream StreamFn = unsafe { nil }
}

fn (mut resp Response) free() {
	unsafe { resp.header.free() }
}

// Formats resp to bytes suitable for HTTP response transmission
pub fn (resp Response) bytes() []u8 {
	mut sb := strings.new_builder(resp.response_buffer_cap())
	resp.write_into_builder(mut sb)
	return unsafe { sb.reuse_as_plain_u8_array() }
}

// write_to appends the raw HTTP response bytes (status line, headers and body)
// to `sb`, letting a caller serialize directly into an existing buffer instead of
// allocating a fresh one via bytes()/bytestr(). Since strings.Builder is a []u8,
// a server can reuse its per-connection write buffer as the target and avoid a
// per-response allocation and copy.
pub fn (resp Response) write_to(mut sb strings.Builder) {
	resp.write_into_builder(mut sb)
}

// Formats resp to a string suitable for HTTP response transmission
pub fn (resp Response) bytestr() string {
	mut sb := strings.new_builder(resp.response_buffer_cap())
	resp.write_into_builder(mut sb)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

fn (resp Response) response_buffer_cap() int {
	return resp.body.len + 64 + resp.header.cur_pos * 48
}

fn (resp Response) write_into_builder(mut sb strings.Builder) {
	sb.write_string('HTTP/')
	sb.write_string(resp.http_version)
	sb.write_u8(` `)
	sb.write_decimal(resp.status_code)
	sb.write_u8(` `)
	sb.write_string(resp.status_msg)
	sb.write_string('\r\n')
	resp.header.render_into_sb(mut sb,
		version: resp.version()
	)
	sb.write_string('\r\n')
	sb.write_string(resp.body)
}

// parse_response parses an HTTP response and decodes its transfer and content encodings.
pub fn parse_response(resp string) !Response {
	return parse_response_with_options(resp)
}

@[params]
struct ResponseParseOptions {
	raw_body bool
	// headers_only parses only the status line and headers. The caller already
	// holds the decoded body, so chunk framing and content encoding are skipped.
	headers_only                 bool
	max_total_decompressed_bytes i64 = default_max_total_decompressed_bytes
	stop_copying_limit           i64 = -1
	// A transport stopped before receiving the complete encoded body.
	input_truncated bool
}

// ResponseDecompressionLimitError is returned when decoded output exceeds its configured or
// representational limit.
pub struct ResponseDecompressionLimitError {
	Error
pub:
	max_total_decompressed_bytes i64
	// max_stage_decompressed_bytes is nonzero when a stage's array representation
	// ceiling, rather than the configured total budget, stopped decoding.
	max_stage_decompressed_bytes i64
}

// msg returns which decoded-output limit was exceeded.
pub fn (err ResponseDecompressionLimitError) msg() string {
	if err.max_stage_decompressed_bytes > 0 {
		return 'http: decoded response exceeded the ${err.max_stage_decompressed_bytes}-byte per-stage representation limit'
	}
	return 'http: decoded response exceeded the ${err.max_total_decompressed_bytes}-byte limit'
}

// code marks a deterministic decompression limit error as non-retryable.
pub fn (_ ResponseDecompressionLimitError) code() int {
	return response_decompression_limit_error_code
}

// ResponseContentEncodingLayerLimitError is returned when a response has too many encoding layers.
pub struct ResponseContentEncodingLayerLimitError {
	Error
pub:
	max_layers int
}

// msg returns the maximum number of accepted Content-Encoding layers.
pub fn (err ResponseContentEncodingLayerLimitError) msg() string {
	return 'http: response has more than ${err.max_layers} Content-Encoding layers'
}

// code marks a deterministic encoding-layer limit error as non-retryable.
pub fn (_ ResponseContentEncodingLayerLimitError) code() int {
	return response_content_encoding_layer_limit_error_code
}

// parse_response_with_options leaves content encodings intact for a reverse proxy.
fn parse_response_with_options(resp string, opts ResponseParseOptions) !Response {
	version, status_code, status_msg := parse_status_line(resp.all_before('\r\n'))!
	// Build resp header map and separate the body
	start_idx, end_idx := find_headers_range(resp)!
	header := parse_headers(resp.substr(start_idx, end_idx))!
	mut body := resp.substr(end_idx, resp.len)
	if !opts.headers_only && has_header_token(header.get(.transfer_encoding) or { '' }, 'chunked') {
		body = chunked.decode(body)!
	}
	if !opts.raw_body && !opts.headers_only {
		body = decode_response_body(body, header.get(.content_encoding) or { '' },
			opts.max_total_decompressed_bytes, opts.stop_copying_limit, opts.input_truncated)!
	}
	if opts.stop_copying_limit > 0 && body.len > opts.stop_copying_limit {
		body = body[..int(opts.stop_copying_limit)]
	}
	return Response{
		http_version: version
		status_code:  status_code
		status_msg:   status_msg
		header:       header
		body:         body
	}
}

fn has_header_token(header_value string, expected_token string) bool {
	for token in parse_header_tokens(header_value) {
		if token == expected_token.to_lower() {
			return true
		}
	}
	return false
}

fn parse_header_tokens(header_value string) []string {
	mut tokens := []string{}
	for part in header_value.split(',') {
		token := part.all_before(';').trim_space().to_lower()
		if token != '' {
			tokens << token
		}
	}
	return tokens
}

struct ResponseDecompressionSink {
mut:
	data           []u8
	max_output_len int
	max_stored_len int
	output_len     int
	over_limit     bool
}

fn collect_response_decompressed_chunk(chunk []u8, userdata voidptr) int {
	mut sink := unsafe { &ResponseDecompressionSink(userdata) }
	remaining := sink.max_output_len - sink.output_len
	if chunk.len > remaining {
		store_remaining := sink.max_stored_len - sink.data.len
		store_len := if store_remaining < remaining { store_remaining } else { remaining }
		if store_len > 0 {
			sink.data << chunk[..store_len]
		}
		sink.over_limit = true
		return 0
	}
	sink.output_len += chunk.len
	store_remaining := sink.max_stored_len - sink.data.len
	store_len := if store_remaining < chunk.len { store_remaining } else { chunk.len }
	if store_len > 0 {
		sink.data << chunk[..store_len]
	}
	return chunk.len
}

struct ResponseDecompressionStageBudget {
	output_limit_bytes i64
	total_budget       bool
}

fn response_decompression_stage_budget(max_total_decompressed_bytes i64, total_decompressed_bytes i64,
	max_stage_output_bytes i64) ResponseDecompressionStageBudget {
	if max_total_decompressed_bytes <= 0 {
		return ResponseDecompressionStageBudget{
			output_limit_bytes: max_stage_output_bytes
		}
	}
	remaining_budget := max_total_decompressed_bytes - total_decompressed_bytes
	if remaining_budget <= max_stage_output_bytes {
		return ResponseDecompressionStageBudget{
			output_limit_bytes: remaining_budget
			total_budget:       true
		}
	}
	return ResponseDecompressionStageBudget{
		output_limit_bytes: max_stage_output_bytes
	}
}

fn response_decompression_stage_output_limit(encoding string, max_array_len i64,
	deflate_output_limit i64) i64 {
	if encoding != 'br' {
		return deflate_output_limit
	}
	if max_array_len == max_i64 {
		return max_array_len / 2 + 1
	}
	return max_array_len - 1
}

fn response_decompression_limit_error(total_limit i64, stage_limit i64) ResponseDecompressionLimitError {
	return ResponseDecompressionLimitError{
		max_total_decompressed_bytes: total_limit
		max_stage_decompressed_bytes: stage_limit
	}
}

fn decode_response_body(body string, content_encoding string, max_total_decompressed_bytes i64,
	stop_copying_limit i64, input_truncated bool) !string {
	if body.len == 0 {
		return body
	}
	if input_truncated {
		return body
	}
	encodings := parse_header_tokens(content_encoding)
	if encodings.len == 0 {
		return body
	}
	if encodings.len > max_content_encoding_layers {
		return ResponseContentEncodingLayerLimitError{
			max_layers: max_content_encoding_layers
		}
	}
	for encoding in encodings {
		if encoding != 'identity' && encoding !in ['gzip', 'x-gzip', 'br', 'deflate'] {
			return body
		}
	}
	mut final_encoding_index := -1
	for i, encoding in encodings {
		if encoding != 'identity' {
			final_encoding_index = i
			break
		}
	}
	mut decoded := body.bytes()
	mut total_decompressed_bytes := i64(0)
	deflate_output_limit := i64(deflate.max_safe_inflate_output_len())
	callback := deflate.ChunkCallback(collect_response_decompressed_chunk)
	for i := encodings.len - 1; i >= 0; i-- {
		encoding := encodings[i]
		if encoding == 'identity' {
			continue
		}
		max_stage_output_bytes := response_decompression_stage_output_limit(encoding,
			i64(max_int), deflate_output_limit)
		stage_budget := response_decompression_stage_budget(max_total_decompressed_bytes,
			total_decompressed_bytes, max_stage_output_bytes)
		if stage_budget.output_limit_bytes < 0 {
			unsafe { decoded.free() }
			return response_decompression_limit_error(max_total_decompressed_bytes, 0)
		}
		stage_limit := int(stage_budget.output_limit_bytes)
		mut output_limit := stage_limit
		mut stored_limit := stage_limit
		mut preview_limit := false
		if i == final_encoding_index && stop_copying_limit > 0
			&& stop_copying_limit < stage_budget.output_limit_bytes {
			output_limit = int(stop_copying_limit)
			stored_limit = int(stop_copying_limit)
			preview_limit = true
		}
		mut sink := ResponseDecompressionSink{
			max_output_len: output_limit
			max_stored_len: stored_limit
		}
		mut decode_failed := false
		match encoding {
			'gzip', 'x-gzip' {
				gzip.decompress_with_callback(decoded, callback, voidptr(&sink)) or {
					decode_failed = true
					0
				}
			}
			'br' {
				brotli.decompress_with_callback(decoded, callback, voidptr(&sink)) or {
					decode_failed = true
					0
				}
			}
			'deflate' {
				zlib.decompress_with_callback(decoded, callback, voidptr(&sink)) or {
					decode_failed = true
					0
				}
			}
			else {
				decode_failed = true
			}
		}
		if sink.over_limit {
			if preview_limit {
				preview := sink.data.bytestr()
				unsafe {
					decoded.free()
					sink.data.free()
				}
				return preview
			}
			unsafe {
				decoded.free()
				sink.data.free()
			}
			stage_error_limit := if stage_budget.total_budget {
				i64(0)
			} else {
				stage_budget.output_limit_bytes
			}
			return response_decompression_limit_error(max_total_decompressed_bytes,
				stage_error_limit)
		}
		if decode_failed {
			unsafe {
				decoded.free()
				sink.data.free()
			}
			return body
		}
		if max_total_decompressed_bytes > 0 {
			total_decompressed_bytes += i64(sink.output_len)
		}
		unsafe { decoded.free() }
		decoded = sink.data
		sink.data = []u8{}
	}
	result := decoded.bytestr()
	unsafe { decoded.free() }
	return result
}

// parse_status_line parses the first HTTP response line into the HTTP
// version, status code, and reason phrase
fn parse_status_line(line string) !(string, int, string) {
	if line.len < 5 || line[..5].to_lower() != 'http/' {
		return error('response does not start with HTTP/, line: `${line}`')
	}
	data := line.split_nth(' ', 3)
	if data.len < 2 {
		return error('expected at least 2 tokens, but found: ${data.len}')
	}
	version := data[0].substr(5, data[0].len)
	// validate version is 1*DIGIT "." 1*DIGIT
	digits := version.split_nth('.', 3)
	if digits.len != 2 {
		return error('HTTP version malformed, found: `${digits}`')
	}
	for digit in digits {
		strconv.atoi(digit) or {
			return error('HTTP version must contain only integers, found: `${digit}`')
		}
	}
	// RFC 9112 §4: the reason-phrase is optional, and servers that omit it
	// sometimes omit the SP before it too.
	reason := if data.len == 3 { data[2] } else { '' }
	return version, strconv.atoi(data[1])!, reason
}

// cookies parses the Set-Cookie headers into Cookie objects
pub fn (r Response) cookies() []Cookie {
	mut cookies := []Cookie{}
	for cookie in r.header.values(.set_cookie) {
		cookies << parse_cookie(cookie) or { continue }
	}
	return cookies
}

// status parses the status_code and returns a corresponding enum field of Status
pub fn (r Response) status() Status {
	return status_from_int(r.status_code)
}

// set_status sets the status_code and status_msg of the response
pub fn (mut r Response) set_status(s Status) {
	r.status_code = s.int()
	r.status_msg = s.str()
}

// version parses the version
pub fn (r Response) version() Version {
	return match r.http_version {
		'1.0' { .v1_0 }
		'1.1' { .v1_1 }
		'2.0' { .v2_0 }
		'3.0' { .v3_0 }
		else { .unknown }
	}
}

// set_version sets the http_version string of the response
pub fn (mut r Response) set_version(v Version) {
	if v == .unknown {
		r.http_version = ''
		return
	}
	maj, min := v.protos()
	r.http_version = '${maj}.${min}'
}

pub struct ResponseConfig {
pub:
	version Version = .v1_1
	status  Status  = .ok
	header  Header
	body    string
}

// new_response creates a Response object from the configuration. This
// function will add a Content-Length header if body is not empty.
pub fn new_response(conf ResponseConfig) Response {
	mut resp := Response{
		body:   conf.body
		header: conf.header
	}
	if resp.body != '' && !resp.header.contains(.content_length) {
		resp.header.add(.content_length, resp.body.len.str())
	}
	resp.set_status(conf.status)
	resp.set_version(conf.version)
	return resp
}

// find_headers_range returns the start (inclusive) and end (exclusive)
// index of the headers in the string, including the trailing newlines. This
// helper function expects the first line in `data` to be the HTTP status line
// (HTTP/1.1 200 OK).
fn find_headers_range(data string) !(int, int) {
	start_idx := data.index('\n') or { return error('no start index found') } + 1
	mut count := 0
	for i := start_idx; i < data.len; i++ {
		if data[i] == `\n` {
			count++
		} else if data[i] != `\r` {
			count = 0
		}
		if count == 2 {
			return start_idx, i + 1
		}
	}
	return error('no end index found')
}
