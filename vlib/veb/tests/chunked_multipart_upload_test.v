// vtest retry: 3
import crypto.sha256
import io
import net
import time
import veb

const chunked_multipart_port = 13005

const chunked_multipart_boundary = '----VChunkedMultipartBoundary'
const chunked_multipart_chunk_size = 4096

const chunked_multipart_localserver = '127.0.0.1:${chunked_multipart_port}'

pub struct ChunkedMultipartContext {
	veb.Context
}

pub struct ChunkedMultipartApp {
pub mut:
	started chan bool
}

pub fn (mut app ChunkedMultipartApp) before_accept_loop() {
	app.started <- true
}

@['/upload'; post]
pub fn (mut app ChunkedMultipartApp) upload(mut ctx ChunkedMultipartContext) veb.Result {
	if 'file' !in ctx.files || ctx.files['file'].len == 0 {
		ctx.res.set_status(.bad_request)
		return ctx.text('missing file')
	}
	file := ctx.files['file'][0]
	return ctx.text(sha256.sum256(file.data.bytes()).hex())
}

fn testsuite_begin() {
	mut app := &ChunkedMultipartApp{}
	spawn veb.run_at[ChunkedMultipartApp, ChunkedMultipartContext](mut app,
		port:               chunked_multipart_port
		family:             .ip
		timeout_in_seconds: 10
	)
	_ := <-app.started
}

fn test_large_chunked_multipart_form_upload() {
	payload := make_chunked_multipart_payload(2 * 1024 * 1024)
	expected_hash := sha256.sum256(payload).hex()
	response := send_chunked_multipart_request(payload) or {
		assert false, 'failed to send chunked multipart request: ${err}'
		return
	}
	assert response.starts_with('HTTP/1.1 200 OK')
	assert response.all_after('\r\n\r\n') == expected_hash
}

fn make_chunked_multipart_payload(size int) []u8 {
	mut payload := []u8{len: size}
	for i in 0 .. size {
		payload[i] = u8(i % 256)
	}
	return payload
}

fn send_chunked_multipart_request(payload []u8) !string {
	mut client := net.dial_tcp(chunked_multipart_localserver)!
	defer {
		client.close() or {}
	}
	client.set_read_timeout(10 * time.second)
	client.set_write_timeout(10 * time.second)
	headers := 'POST /upload HTTP/1.1\r\nHost: ${chunked_multipart_localserver}\r\nTransfer-Encoding: chunked\r\nContent-Type: multipart/form-data; boundary=${chunked_multipart_boundary}\r\nConnection: close\r\n\r\n'
	client.write_string(headers)!
	body := build_multipart_body(payload)
	write_chunked_body(mut client, body)!
	return read_response(mut client)
}

fn build_multipart_body(payload []u8) []u8 {
	mut body := []u8{}
	body << '--${chunked_multipart_boundary}\r\n'.bytes()
	body << 'Content-Disposition: form-data; name="file"; filename="payload.bin"\r\n'.bytes()
	body << 'Content-Type: application/octet-stream\r\n\r\n'.bytes()
	body << payload
	body << '\r\n--${chunked_multipart_boundary}--\r\n'.bytes()
	return body
}

fn write_chunked_body(mut client net.TcpConn, body []u8) ! {
	for start := 0; start < body.len; start += chunked_multipart_chunk_size {
		end := if start + chunked_multipart_chunk_size < body.len {
			start + chunked_multipart_chunk_size
		} else {
			body.len
		}
		chunk := body[start..end]
		client.write_string('${chunk.len:x}\r\n')!
		client.write(chunk)!
		client.write_string('\r\n')!
	}
	client.write_string('0\r\n\r\n')!
}

fn read_response(mut client net.TcpConn) !string {
	mut response := []u8{}
	mut buf := []u8{len: 4096}
	for {
		n := client.read(mut buf) or {
			if err is io.Eof {
				break
			}
			return err
		}
		if n == 0 {
			break
		}
		response << buf[..n]
	}
	return response.bytestr()
}
