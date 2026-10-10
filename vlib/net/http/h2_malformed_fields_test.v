module http

import sync
import time

// A decoded HTTP/2 field is copied into an `http.Header` before the message
// reaches the application: by `h2_response_to_http` on the client, and by
// `H2ServerConn.build_request` on the server. `Header.add_custom` refuses a
// name that is not a token and a field past `max_headers`, and both callers
// used to ignore that, so the message was delivered without the field.
//
// These tests check that such a message is refused instead (RFC 9113
// sections 8.1.1 and 8.2.1), together with a value that contains CR, LF or
// NUL, and that unusual but legal fields still arrive unchanged. The client
// is driven through the glue that `fetch` uses for both of its HTTP/2 paths:
// `Request.h2_exchange` over an `H2Conn`, and `do_h2` over an `H2MuxConn`.
// The server is `serve_h2_conn`. No socket is involved.

// pipe_read_timeout fails a read that waits this long, so that a missing
// answer fails the test instead of blocking it.
const pipe_read_timeout = 20 * time.second

// FieldPipeBuf is a one-way in-memory byte queue with blocking reads.
struct FieldPipeBuf {
mut:
	mu     &sync.Mutex = sync.new_mutex()
	data   []u8
	closed bool
}

fn (mut p FieldPipeBuf) write(buf []u8) !int {
	p.mu.lock()
	defer {
		p.mu.unlock()
	}
	if p.closed {
		return error('pipe: write to a closed pipe')
	}
	p.data << buf
	return buf.len
}

fn (mut p FieldPipeBuf) read(mut buf []u8) !int {
	start := time.now()
	for {
		p.mu.lock()
		if p.data.len > 0 {
			n := if p.data.len > buf.len { buf.len } else { p.data.len }
			for i in 0 .. n {
				buf[i] = p.data[i]
			}
			p.data = p.data[n..].clone()
			p.mu.unlock()
			return n
		}
		closed := p.closed
		p.mu.unlock()
		if closed {
			return error('pipe: closed')
		}
		if time.since(start) > pipe_read_timeout {
			return error('pipe: nothing to read after ${pipe_read_timeout}')
		}
		time.sleep(time.millisecond)
	}
	return 0
}

fn (mut p FieldPipeBuf) close() {
	p.mu.lock()
	p.closed = true
	p.mu.unlock()
}

// FieldPipeEnd is one end of a two-way pipe. It satisfies H2Transport.
@[heap]
struct FieldPipeEnd {
mut:
	incoming &FieldPipeBuf
	outgoing &FieldPipeBuf
}

fn (mut p FieldPipeEnd) read(mut buf []u8) !int {
	return p.incoming.read(mut buf)!
}

fn (mut p FieldPipeEnd) write(buf []u8) !int {
	return p.outgoing.write(buf)!
}

fn (mut p FieldPipeEnd) close() {
	p.incoming.close()
	p.outgoing.close()
}

fn new_field_pipe() (&FieldPipeEnd, &FieldPipeEnd) {
	mut a := &FieldPipeBuf{}
	mut b := &FieldPipeBuf{}
	return &FieldPipeEnd{
		incoming: b
		outgoing: a
	}, &FieldPipeEnd{
		incoming: a
		outgoing: b
	}
}

// FrameSource reads HTTP/2 frames from a pipe end.
struct FrameSource {
mut:
	end &FieldPipeEnd
	buf []u8
}

fn (mut s FrameSource) fill(n int) ! {
	for s.buf.len < n {
		mut tmp := []u8{len: 4096}
		got := s.end.read(mut tmp)!
		s.buf << tmp[..got]
	}
}

fn (mut s FrameSource) skip(n int) ! {
	s.fill(n)!
	s.buf = s.buf[n..].clone()
}

fn (mut s FrameSource) next() !H2Frame {
	s.fill(h2_frame_header_len)!
	header := h2_parse_frame_header(s.buf)!
	total := h2_frame_header_len + int(header.length)
	s.fill(total)!
	frame := h2_parse_frame(header, s.buf[h2_frame_header_len..total])!
	s.buf = s.buf[total..].clone()
	return frame
}

// message_frames returns the frames of one message on stream `id`: a HEADERS
// block with `fields`, a DATA frame when `body` is not empty, and a trailing
// HEADERS block when `trailers` is not empty.
fn message_frames(mut enc H2HpackEncoder, id u32, fields []H2HeaderField, body string, trailers []H2HeaderField) []u8 {
	mut out := H2Frame(H2HeadersFrame{
		stream_id:   id
		fragment:    enc.encode(fields)
		end_headers: true
		end_stream:  body == '' && trailers.len == 0
	}).encode()
	if body != '' {
		out << H2Frame(H2DataFrame{
			stream_id:  id
			data:       body.bytes()
			end_stream: trailers.len == 0
		}).encode()
	}
	if trailers.len > 0 {
		out << H2Frame(H2HeadersFrame{
			stream_id:   id
			fragment:    enc.encode(trailers)
			end_headers: true
			end_stream:  true
		}).encode()
	}
	return out
}

// with_byte returns `prefix`, the byte `b`, and `suffix`.
fn with_byte(prefix string, b u8, suffix string) string {
	mut bytes := prefix.bytes()
	bytes << b
	bytes << suffix.bytes()
	return bytes.bytestr()
}

// undeliverable_names returns field names that pass the lowercase and
// non-empty checks, and that are not tokens (RFC 9110 section 5.1), so
// that a `Header` can not store them.
fn undeliverable_names() []string {
	return ['x bad', 'x:bad', 'x@bad', 'x(bad)', with_byte('x', 0, 'bad'), 'x\rbad', 'x\nbad',
		with_byte('x', 0x7f, 'bad'), with_byte('x-caf', 0xe9, '')]
}

// forbidden_values returns field values that contain CR, LF or NUL, at the
// start, in the middle and at the end.
fn forbidden_values() []string {
	mut values := []string{}
	for b in [u8(`\r`), `\n`, 0] {
		values << with_byte('a', b, 'x-injected: 1')
		values << with_byte('', b, 'a')
		values << with_byte('a', b, '')
	}
	return values
}

// legal_fields returns fields that are unusual and well-formed: a TAB inside
// a value, bytes above 0x7f, an empty value, and a name made of the token
// characters that are not letters.
fn legal_fields() []H2HeaderField {
	return [
		H2HeaderField{'x-tab', 'a\tb'},
		H2HeaderField{'x-utf8', 'caf\xc3\xa9'},
		H2HeaderField{'x-obs-text', [u8(0x80), 0xe9, 0xff].bytestr()},
		H2HeaderField{'x-empty', ''},
		H2HeaderField{"x!#$%&'*+-.^_`|~0", 'token'},
	]
}

// numbered_fields returns `n` fields `<prefix>0` .. `<prefix><n-1>`, each
// with its number as the value.
fn numbered_fields(prefix string, n int) []H2HeaderField {
	return []H2HeaderField{len: n, init: H2HeaderField{'${prefix}${index}', index.str()}}
}

// assert_in_header checks that `h` holds each of `fields`, with the value unchanged.
fn assert_in_header(h Header, fields []H2HeaderField, label string) {
	for f in fields {
		assert h.custom_values(f.name) == [f.value], '${label}: field `${f.name}`'
	}
}

// --- client -------------------------------------------------------------------

// ScriptedTransport plays a fixed script of server bytes to the synchronous client.
struct ScriptedTransport {
mut:
	inbound []u8
	rpos    int
}

fn (mut t ScriptedTransport) read(mut buf []u8) !int {
	if t.rpos >= t.inbound.len {
		return error('eof')
	}
	n := if t.inbound.len - t.rpos > buf.len { buf.len } else { t.inbound.len - t.rpos }
	for i in 0 .. n {
		buf[i] = t.inbound[t.rpos + i]
	}
	t.rpos += n
	return n
}

fn (mut t ScriptedTransport) write(buf []u8) !int {
	return buf.len
}

// fetch_sync runs a GET over an `H2Conn`, the client of the one-shot https
// path, against a server that answers with the given response.
fn fetch_sync(fields []H2HeaderField, body string, trailers []H2HeaderField) !Response {
	mut enc := H2HpackEncoder{}
	mut inbound := H2Frame(H2SettingsFrame{}).encode()
	inbound << H2Frame(H2SettingsFrame{
		ack: true
	}).encode()
	inbound << message_frames(mut enc, 1, fields, body, trailers)
	mut conn := new_h2_conn(&ScriptedTransport{
		inbound: inbound
	})
	req := Request{}
	return req.h2_exchange(mut conn, .get, 'example.com', 443, '/', '', new_header())
}

// answer_request plays the server of one multiplexed connection: it answers
// the request HEADERS with the given response, and reads what the client
// sends until the pipe is closed.
fn answer_request(mut end FieldPipeEnd, fields []H2HeaderField, body string, trailers []H2HeaderField) {
	mut frames := FrameSource{
		end: end
	}
	frames.skip(h2_client_preface.len) or { return }
	end.write(H2Frame(H2SettingsFrame{}).encode()) or { return }
	mut enc := H2HpackEncoder{}
	for {
		frame := frames.next() or { return }
		match frame {
			H2SettingsFrame {
				if !frame.ack {
					end.write(H2Frame(H2SettingsFrame{
						ack: true
					}).encode()) or { return }
				}
			}
			H2HeadersFrame {
				end.write(message_frames(mut enc, frame.stream_id, fields, body, trailers)) or {
					return
				}
			}
			else {}
		}
	}
}

// fetch_mux runs a GET over an `H2MuxConn`, the client of the pooled https
// path, against a server that answers with the given response.
fn fetch_mux(fields []H2HeaderField, body string, trailers []H2HeaderField) !Response {
	mut client_end, mut server_end := new_field_pipe()
	mut conn := new_h2_mux_conn(client_end, fn [mut client_end] () {
		client_end.close()
	})
	server := spawn answer_request(mut server_end, fields, body, trailers)
	defer {
		// Closes the pipe, which ends the reader of `conn` and the server.
		conn.release()
		server.wait()
	}
	req := &Request{}
	return do_h2(req, mut conn, .get, 'example.com', 443, '/', '', new_header())
}

// ClientResult is what a GET returned: a response, or an error.
struct ClientResult {
	client   string // which client ran the request
	ok       bool
	resp     Response
	err_msg  string
	too_many bool // the error is a HeaderLimitError
}

// summary describes the result for an assert message.
fn (r ClientResult) summary() string {
	if r.ok {
		return '${r.client}: got a ${r.resp.status_code} response with the fields ${r.resp.header.keys()}'
	}
	return '${r.client}: ${r.err_msg}'
}

// client_error returns the result of `client` for a request that failed with `err`.
fn client_error(client string, err IError) ClientResult {
	return ClientResult{
		client:   client
		err_msg:  err.msg()
		too_many: err is HeaderLimitError
	}
}

// fetch_both runs the same exchange over both clients.
fn fetch_both(fields []H2HeaderField, body string, trailers []H2HeaderField) []ClientResult {
	mut results := []ClientResult{}
	if resp := fetch_sync(fields, body, trailers) {
		results << ClientResult{
			client: 'H2Conn'
			ok:     true
			resp:   resp
		}
	} else {
		results << client_error('H2Conn', err)
	}
	if resp := fetch_mux(fields, body, trailers) {
		results << ClientResult{
			client: 'H2MuxConn'
			ok:     true
			resp:   resp
		}
	} else {
		results << client_error('H2MuxConn', err)
	}
	return results
}

// response_with returns the header block of a 200 response with `fields`.
fn response_with(fields ...H2HeaderField) []H2HeaderField {
	mut block := [H2HeaderField{':status', '200'}, H2HeaderField{'content-type', 'text/plain'}]
	block << fields
	return block
}

fn test_client_fails_for_a_response_field_with_an_invalid_name() {
	for name in undeliverable_names() {
		for r in fetch_both(response_with(H2HeaderField{name, 'v'}), 'body', []) {
			assert !r.ok, '${name.bytes()}: ${r.summary()}'
			assert r.err_msg.contains('malformed response: invalid header field name'), '${name.bytes()}: ${r.summary()}'
		}
	}
}

fn test_client_fails_for_a_response_field_value_with_cr_lf_or_nul() {
	for value in forbidden_values() {
		for r in fetch_both(response_with(H2HeaderField{'x-note', value}), 'body', []) {
			assert !r.ok, '${value.bytes()}: ${r.summary()}'
			assert r.err_msg.contains('malformed response: forbidden NUL/CR/LF octet in value of "x-note"'), '${value.bytes()}: ${r.summary()}'
		}
	}
}

fn test_client_fails_for_an_invalid_field_in_a_trailer_section() {
	for name in undeliverable_names() {
		for r in fetch_both(response_with(), 'body', [H2HeaderField{name, 'v'}]) {
			assert !r.ok, '${name.bytes()}: ${r.summary()}'
			assert r.err_msg.contains('malformed trailers: invalid header field name'), '${name.bytes()}: ${r.summary()}'
		}
	}
	for value in forbidden_values() {
		for r in fetch_both(response_with(), 'body', [H2HeaderField{'x-note', value}]) {
			assert !r.ok, '${value.bytes()}: ${r.summary()}'
			assert r.err_msg.contains('malformed trailers: forbidden NUL/CR/LF octet in value of "x-note"'), '${value.bytes()}: ${r.summary()}'
		}
	}
}

fn test_client_fails_for_a_response_with_more_fields_than_a_header_holds() {
	status := [H2HeaderField{':status', '200'}]
	// one field too many in the header section
	mut fields := status.clone()
	fields << numbered_fields('x-f', max_headers + 1)
	for r in fetch_both(fields, 'body', []) {
		assert !r.ok, r.summary()
		assert r.too_many, r.summary()
	}
	// the trailer section is delivered in the same Header
	fields = status.clone()
	fields << numbered_fields('x-f', max_headers)
	for r in fetch_both(fields, 'body', [H2HeaderField{'x-checksum', 'abc'}]) {
		assert !r.ok, r.summary()
		assert r.too_many, r.summary()
	}
}

fn test_client_delivers_unusual_legal_fields_unchanged() {
	legal := legal_fields()
	for r in fetch_both(response_with(...legal), 'body', []) {
		assert r.ok, r.summary()
		assert r.resp.status_code == 200
		assert r.resp.body == 'body'
		assert_in_header(r.resp.header, legal, r.client)
	}
	for r in fetch_both(response_with(), 'body', legal) {
		assert r.ok, r.summary()
		assert r.resp.body == 'body'
		assert_in_header(r.resp.header, legal, '${r.client} trailers')
	}
}

fn test_client_delivers_a_response_that_fills_the_header() {
	mut fields := [H2HeaderField{':status', '200'}]
	fields << numbered_fields('x-f', max_headers - 1)
	trailers := [H2HeaderField{'x-checksum', 'abc'}]
	for r in fetch_both(fields, 'body', trailers) {
		assert r.ok, r.summary()
		assert r.resp.header.keys().len == max_headers, r.summary()
		assert_in_header(r.resp.header, fields[1..], r.client)
		assert_in_header(r.resp.header, trailers, r.client)
	}
	// without a body and trailers
	fields << H2HeaderField{'x-last', 'z'}
	for r in fetch_both(fields, '', []) {
		assert r.ok, r.summary()
		assert r.resp.header.keys().len == max_headers, r.summary()
		assert_in_header(r.resp.header, fields[1..], r.client)
	}
}

// --- server -------------------------------------------------------------------

// HandledRequests is a Handler that records the requests it is given.
@[heap]
struct HandledRequests {
mut:
	mu   &sync.Mutex = sync.new_mutex()
	reqs []Request
}

fn (mut h HandledRequests) handle(req Request) Response {
	h.mu.lock()
	h.reqs << req
	h.mu.unlock()
	return Response{
		status_code: 200
		body:        'handled ${req.url}'
	}
}

fn (mut h HandledRequests) all() []Request {
	h.mu.lock()
	defer {
		h.mu.unlock()
	}
	return h.reqs.clone()
}

// Answer is what the server did with one request stream.
struct Answer {
	rst_code i64 = -1 // the error code of its RST_STREAM, -1 if there was a response
	status   int // the :status of its response
	body     string
}

// str describes the answer for an assert message.
fn (a Answer) str() string {
	if a.rst_code >= 0 {
		return 'RST_STREAM(${h2_error_code_name(u32(a.rst_code))})'
	}
	return 'a ${a.status} response'
}

// ServerSession is a client that writes raw frames to a `serve_h2_conn`.
struct ServerSession {
mut:
	end     &FieldPipeEnd
	handled &HandledRequests
	frames  FrameSource
	enc     H2HpackEncoder
	dec     H2HpackDecoder
}

// new_server_session starts the server on one end of a pipe, and sends the
// connection preface from the other end.
fn new_server_session() !&ServerSession {
	mut client_end, mut server_end := new_field_pipe()
	mut handled := &HandledRequests{}
	mut handler := Handler(handled)
	spawn fn [mut server_end, mut handler] () {
		mut transport := H2Transport(server_end)
		serve_h2_conn(mut transport, mut handler, '127.0.0.1:1234') or {}
	}()
	mut preface := h2_client_preface.bytes()
	preface << H2Frame(H2SettingsFrame{}).encode()
	client_end.write(preface)!
	return &ServerSession{
		end:     client_end
		handled: handled
		frames:  FrameSource{
			end: client_end
		}
	}
}

// request sends one request on stream `id`, and returns what the server
// answered on that stream.
fn (mut s ServerSession) request(id u32, fields []H2HeaderField, body string, trailers []H2HeaderField) !Answer {
	s.end.write(message_frames(mut s.enc, id, fields, body, trailers))!
	mut status := 0
	mut data := []u8{}
	for {
		frame := s.frames.next()!
		match frame {
			H2RstStreamFrame {
				if frame.stream_id == id {
					return Answer{
						rst_code: i64(frame.error_code)
					}
				}
			}
			H2HeadersFrame {
				// The response blocks of this server fit in one frame.
				for f in s.dec.decode(frame.fragment)! {
					if f.name == ':status' && frame.stream_id == id {
						status = f.value.int()
					}
				}
				if frame.stream_id == id && frame.end_stream {
					break
				}
			}
			H2DataFrame {
				if frame.stream_id == id {
					data << frame.data
					if frame.end_stream {
						break
					}
				}
			}
			H2GoawayFrame {
				return error('the server closed the connection: GOAWAY(${h2_error_code_name(frame.error_code)})')
			}
			else {}
		}
	}
	return Answer{
		status: status
		body:   data.bytestr()
	}
}

// expect_only_the_next_request checks that the connection still serves a
// request, on stream `id`, and that it is the only one the handler was given.
fn (mut s ServerSession) expect_only_the_next_request(id u32, label string) ! {
	answer := s.request(id, request_with('/next'), '', [])!
	assert answer.status == 200, '${label}: the next request got ${answer}'
	assert answer.body == 'handled /next', label
	handled := s.handled.all()
	assert handled.map(it.url) == ['/next'], '${label}: the handler was given ${handled.map(it.url)}'
}

fn (mut s ServerSession) close() {
	s.end.close()
}

// request_with returns the header block of a request for `path` with `fields`.
fn request_with(path string, fields ...H2HeaderField) []H2HeaderField {
	mut block := [
		H2HeaderField{':method', 'POST'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'h.example'},
		H2HeaderField{':path', path},
	]
	block << fields
	return block
}

// assert_refused checks that the server reset the stream of a malformed
// request, the way it does for the other malformed requests.
fn assert_refused(answer Answer, label string) {
	assert answer.rst_code == i64(u32(H2ErrorCode.protocol_error)), '${label}: expected RST_STREAM(PROTOCOL_ERROR), got ${answer}'
}

fn test_server_refuses_a_request_field_with_an_invalid_name() {
	for name in undeliverable_names() {
		label := 'field name ${name.bytes()}'
		mut s := new_server_session()!
		answer := s.request(1, request_with('/bad', H2HeaderField{name, 'v'}), 'body', [])!
		assert_refused(answer, label)
		s.expect_only_the_next_request(3, label)!
		s.close()
	}
}

fn test_server_refuses_a_request_field_value_with_cr_lf_or_nul() {
	for value in forbidden_values() {
		label := 'field value ${value.bytes()}'
		mut s := new_server_session()!
		answer := s.request(1, request_with('/bad', H2HeaderField{'x-note', value}), 'body', [])!
		assert_refused(answer, label)
		s.expect_only_the_next_request(3, label)!
		s.close()
	}
}

fn test_server_refuses_an_invalid_field_in_a_trailer_section() {
	mut bad := []H2HeaderField{}
	for name in undeliverable_names() {
		bad << H2HeaderField{name, 'v'}
	}
	for value in forbidden_values() {
		bad << H2HeaderField{'x-note', value}
	}
	for f in bad {
		label := 'trailer field ${f.name.bytes()}: ${f.value.bytes()}'
		mut s := new_server_session()!
		answer := s.request(1, request_with('/bad'), 'body', [f])!
		assert_refused(answer, label)
		s.expect_only_the_next_request(3, label)!
		s.close()
	}
}

fn test_server_answers_431_to_a_request_with_more_fields_than_a_header_holds() {
	for with_body in [false, true] {
		label := 'with_body: ${with_body}'
		body := if with_body { 'body' } else { '' }
		mut s := new_server_session()!
		answer := s.request(1, request_with('/many', ...numbered_fields('x-f', max_headers + 1)),
			body, [])!
		assert answer.status == 431, '${label}: got ${answer}'
		s.expect_only_the_next_request(3, label)!
		s.close()
	}
	// `:authority` is delivered as the Host field, which needs a place in the Header too.
	mut s := new_server_session()!
	mut answer := s.request(1, request_with('/many', ...numbered_fields('x-f', max_headers)),
		'', [])!
	assert answer.status == 431, 'no place for Host: got ${answer}'
	// A request that sends its own `host` field as one of the `max_headers` fields fits.
	mut fields := numbered_fields('x-f', max_headers - 1)
	fields << H2HeaderField{'host', 'h.example'}
	answer = s.request(3, request_with('/fits', ...fields), '', [])!
	assert answer.status == 200, 'got ${answer}'
	assert s.handled.all().map(it.url) == ['/fits']
	s.close()
}

fn test_server_delivers_unusual_legal_fields_unchanged() {
	legal := legal_fields()
	mut s := new_server_session()!
	answer := s.request(1, request_with('/legal', ...legal), 'body', legal)!
	assert answer.status == 200, 'got ${answer}'
	assert answer.body == 'handled /legal'
	handled := s.handled.all()
	assert handled.len == 1
	assert handled[0].data == 'body'
	assert_in_header(handled[0].header, legal, 'request')
	s.close()
}

fn test_server_delivers_a_request_that_fills_the_header() {
	fields := numbered_fields('x-f', max_headers - 1)
	mut s := new_server_session()!
	answer := s.request(1, request_with('/full', ...fields), 'body', [])!
	assert answer.status == 200, 'got ${answer}'
	handled := s.handled.all()
	assert handled.len == 1
	assert handled[0].header.keys().len == max_headers
	assert handled[0].header.custom_values('host') == ['h.example']
	assert_in_header(handled[0].header, fields, 'request')
	assert handled[0].remote_addr == '127.0.0.1:1234'
	s.close()
}
