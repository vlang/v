module websocket

import net
import net.urllib
import encoding.base64
import eventbus
import sync
import net.websocket.logger

const (
	l = logger.new('ws')
)

pub struct Client {
	retry      int
	eb         &eventbus.EventBus
	is_ssl     bool
	lock       &sync.Mutex = sync.new_mutex()
	write_lock &sync.Mutex = sync.new_mutex()
	// subprotocol_len int
	// cwebsocket_subprotocol *subprotocol;
	// cwebsocket_subprotocol *subprotocols[];
mut:
	state      State
	socket     net.Socket
	flags      []Flag
	sslctx     &C.SSL_CTX
	ssl        &C.SSL
	fragments  []Fragment
pub mut:
	uri        string
	subscriber &eventbus.Subscriber
}

struct Fragment {
	data voidptr
	len  u64
	code OPCode
}

pub struct Message {
pub:
	opcode      OPCode
	payload     voidptr
	payload_len int
}

pub enum OPCode {
	continuation = 0x00
	text_frame = 0x01
	binary_frame = 0x02
	close = 0x08
	ping = 0x09
	pong = 0x0A
}

enum State {
	connecting = 0
	connected
	open
	closing
	closed
}

struct Uri {
mut:
	hostname    string
	port        string
	resource    string
	querystring string
}

enum Flag {
	has_accept
	has_connection
	has_upgrade
}

struct Frame {
mut:
	fin         bool
	rsv1        bool
	rsv2        bool
	rsv3        bool
	opcode      OPCode
	mask        bool
	payload_len u64
	masking_key [4]byte
}

pub fn new(uri string) &Client {
	eb := eventbus.new()
	ws := &Client{
		uri: uri
		state: .closed
		eb: eb
		subscriber: eb.subscriber
		is_ssl: uri.starts_with('wss')
		ssl: 0
		sslctx: 0
	}
	return ws
}

fn C.sscanf() int

fn (ws &Client) parse_uri() &Uri {
	u := urllib.parse(ws.uri) or {
		panic(err)
	}
	v := u.request_uri().split('?')
	querystring := if v.len > 1 { '?' + v[1] } else { '' }
	return &Uri{
		hostname: u.hostname()
		port: u.port()
		resource: v[0]
		querystring: querystring
	}
}

pub fn (mut ws Client) connect() int {
	match ws.state {
		.connected {
			l.f('connect: websocket already connected')
		}
		.connecting {
			l.f('connect: websocket already connecting')
		}
		.open {
			l.f('connect: websocket already open')
		}
		else {
			// do nothing
		}
	}
	ws.lock.lock()
	ws.state = .connecting
	ws.lock.unlock()
	uri := ws.parse_uri()
	nonce := get_nonce()
	seckey := base64.encode(nonce)
	ai_family := C.AF_INET
	ai_socktype := C.SOCK_STREAM
	l.d('handshake header:')
	handshake := 'GET ${uri.resource}${uri.querystring} HTTP/1.1\r\nHost: ${uri.hostname}:${uri.port}\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: ${seckey}\r\nSec-WebSocket-Version: 13\r\n\r\n'
	l.d(handshake)
	socket := net.new_socket(ai_family, ai_socktype, 0) or {
		l.f(err)
		return -1
	}
	ws.socket = socket
	ws.socket.connect(uri.hostname, uri.port.int()) or {
		l.f(err)
		return -1
	}
	optval := 1
	ws.socket.setsockopt(C.SOL_SOCKET, C.SO_KEEPALIVE, &optval) or {
		l.f(err)
		return -1
	}
	if ws.is_ssl {
		ws.connect_ssl()
	}
	ws.lock.lock()
	ws.state = .connected
	ws.lock.unlock()
	res := ws.write_to_server(handshake.str, handshake.len)
	if res <= 0 {
		l.f('Handshake failed.')
	}
	ws.read_handshake(seckey)
	ws.lock.lock()
	ws.state = .open
	ws.lock.unlock()
	ws.send_open_event()
	unsafe {
		handshake.free()
		nonce.free()
		free(uri)
	}
	return 0
}

pub fn (mut ws Client) close(code int, message string) {
	if ws.state != .closed && ws.socket.sockfd > 1 {
		ws.lock.lock()
		ws.state = .closing
		ws.lock.unlock()
		mut code32 := 0
		if code > 0 {
			code_ := C.htons(code)
			message_len := message.len + 2
			mut close_frame := [`0`].repeat(message_len)
			close_frame[0] = byte(code_ & 0xFF)
			close_frame[1] = byte(code_ >> 8)
			code32 = (close_frame[0] << 8) + close_frame[1]
			for i in 0 .. message.len {
				close_frame[i + 2] = message[i]
			}
			ws.send_control_frame(.close, 'CLOSE', close_frame)
		} else {
			ws.send_control_frame(.close, 'CLOSE', [])
		}
		if ws.ssl != 0 {
			C.SSL_shutdown(ws.ssl)
			C.SSL_free(ws.ssl)
			if ws.sslctx != 0 {
				C.SSL_CTX_free(ws.sslctx)
			}
		} else {
			if C.shutdown(ws.socket.sockfd, C.SHUT_WR) == -1 {
				l.e('Unabled to shutdown websocket.')
			}
			mut buf := [`0`]
			for ws.read_from_server(buf.data, 1) > 0 {
				buf[0] = `\0`
			}
			unsafe {
				buf.free()
			}
			if C.close(ws.socket.sockfd) == -1 {
				// ws.send_close_event()(websocket, 1011, strerror(C.errno));
			}
		}
		ws.fragments = []
		ws.send_close_event()
		ws.lock.lock()
		ws.state = .closed
		ws.lock.unlock()
		unsafe {
		}
		// TODO impl autoreconnect
	}
}

pub fn (mut ws Client) write(payload byteptr, payload_len int, code OPCode) int {
	mut bytes_written := -1
	if ws.state != .open {
		ws.send_error_event('WebSocket closed. Cannot write.')
		unsafe {
			free(payload)
		}
		return -1
	}
	header_len := 6 + if payload_len > 125 { 2 } else { 0 } + if payload_len > 0xffff { 6 } else { 0 }
	frame_len := header_len + payload_len
	mut frame_buf := [`0`].repeat(frame_len)
	fbdata := byteptr( frame_buf.data )
	masking_key := create_masking_key()
	mut header := [`0`].repeat(header_len)
	header[0] = byte(code) | 0x80
	if payload_len <= 125 {
		header[1] = byte(payload_len | 0x80)
		header[2] = masking_key[0]
		header[3] = masking_key[1]
		header[4] = masking_key[2]
		header[5] = masking_key[3]
	} else if payload_len > 125 && payload_len <= 0xffff {
		len16 := C.htons(payload_len)
		header[1] = (126 | 0x80)
		C.memcpy(header.data + 2, &len16, 2)
		header[4] = masking_key[0]
		header[5] = masking_key[1]
		header[6] = masking_key[2]
		header[7] = masking_key[3]
	} else if payload_len > 0xffff && payload_len <= 0xffffffffffffffff { // 65535 && 18446744073709551615
		len64 := htonl64(u64(payload_len))
		header[1] = (127 | 0x80)
		C.memcpy(header.data + 2, len64, 8)
		header[10] = masking_key[0]
		header[11] = masking_key[1]
		header[12] = masking_key[2]
		header[13] = masking_key[3]
	} else {
		l.c('write: frame too large')
		ws.close(1009, 'frame too large')
		goto free_data
		return -1
	}
	C.memcpy(fbdata, header.data, header_len)
	C.memcpy(fbdata + header_len, payload, payload_len)
	for i in 0 .. payload_len {
		frame_buf[header_len + i] ^= masking_key[i % 4] & 0xff
	}
	bytes_written = ws.write_to_server(fbdata, frame_len)
	if bytes_written == -1 {
		err := string(byteptr(C.strerror(C.errno)))
		l.e('write: there was an error writing data: ${err}')
		ws.send_error_event('Error writing data')
		goto free_data
		return -1
	}
	l.d('write: ${bytes_written} bytes written.')
	free_data:
	unsafe {
		free(payload)
		frame_buf.free()
		header.free()
		masking_key.free()
	}
	return bytes_written
}

pub fn (mut ws Client) listen() {
	l.i('Starting listener...')
	for ws.state == .open {
		ws.read()
	}
	l.i('Listener stopped as websocket was closed.')
}

pub fn (mut ws Client) read() int {
	mut bytes_read := u64(0)
	initial_buffer := u64(256)
	mut header_len := 2
	header_len_offset := 2
	extended_payload16_end_byte := 4
	extended_payload64_end_byte := 10
	mut payload_len := u64(0)
	mut data := C.calloc(initial_buffer, 1) // [`0`].repeat(int(max_buffer))
	mut frame := Frame{}
	mut frame_size := u64(header_len)
	for bytes_read < frame_size && ws.state == .open {
		byt := ws.read_from_server(data + int(bytes_read), 1)
		match byt {
			0 {
				error := 'server closed the connection.'
				l.e('read: ${error}')
				ws.send_error_event(error)
				ws.close(1006, error)
				goto free_data
				return -1
			}
			-1 {
				err := string(byteptr(C.strerror(C.errno)))
				l.e('read: error reading frame. ${err}')
				ws.send_error_event('error reading frame')
				goto free_data
				return -1
			}
			else {
				bytes_read++
			}
		}
		if bytes_read == u64(header_len_offset) {
			frame.fin = (data[0] & 0x80) == 0x80
			frame.rsv1 = (data[0] & 0x40) == 0x40
			frame.rsv2 = (data[0] & 0x20) == 0x20
			frame.rsv3 = (data[0] & 0x10) == 0x10
			frame.opcode = OPCode(int(data[0] & 0x7F))
			frame.mask = (data[1] & 0x80) == 0x80
			frame.payload_len = u64(data[1] & 0x7F)
			// masking key
			if frame.mask {
				frame.masking_key[0] = data[2]
				frame.masking_key[1] = data[3]
				frame.masking_key[2] = data[4]
				frame.masking_key[3] = data[5]
			}
			payload_len = frame.payload_len
			frame_size = u64(header_len) + payload_len
		}
		if frame.payload_len == u64(126) && bytes_read == u64(extended_payload16_end_byte) {
			header_len += 2
			mut extended_payload_len := 0
			extended_payload_len |= data[2] << 8
			extended_payload_len |= data[3] << 0
			// masking key
			if frame.mask {
				frame.masking_key[0] = data[4]
				frame.masking_key[1] = data[5]
				frame.masking_key[2] = data[6]
				frame.masking_key[3] = data[7]
			}
			payload_len = u64(extended_payload_len)
			frame_size = u64(header_len) + payload_len
			if frame_size > initial_buffer {
				l.d('reallocating: ${frame_size}')
				data = C.realloc(data, frame_size)
			}
		} else if frame.payload_len == u64(127) && bytes_read == u64(extended_payload64_end_byte) {
			header_len += 8 // TODO Not sure...
			mut extended_payload_len := u64(0)
			extended_payload_len |= u64(data[2]) << 56
			extended_payload_len |= u64(data[3]) << 48
			extended_payload_len |= u64(data[4]) << 40
			extended_payload_len |= u64(data[5]) << 32
			extended_payload_len |= u64(data[6]) << 24
			extended_payload_len |= u64(data[7]) << 16
			extended_payload_len |= u64(data[8]) << 8
			extended_payload_len |= u64(data[9]) << 0
			// masking key
			if frame.mask {
				frame.masking_key[0] = data[10]
				frame.masking_key[1] = data[11]
				frame.masking_key[2] = data[12]
				frame.masking_key[3] = data[13]
			}
			payload_len = extended_payload_len
			frame_size = u64(header_len) + payload_len
			if frame_size > initial_buffer {
				l.d('reallocating: ${frame_size}')
				data = C.realloc(data, frame_size)
			}
		}
	}
	// unmask the payload
	if frame.mask {
		for i in 0 .. payload_len {
			data[header_len + i] ^= frame.masking_key[i % 4] & 0xff
		}
	}
	if ws.fragments.len > 0 && frame.opcode in [.text_frame, .binary_frame] {
		ws.close(0, '')
		goto free_data
		return -1
	} else if frame.opcode in [.text_frame, .binary_frame] {
		data_node:
		l.d('read: recieved text_frame or binary_frame')
		mut payload := malloc(sizeof(byte) * u32(payload_len) + 1)
		if payload == 0 {
			l.f('out of memory')
		}
		C.memcpy(payload, &data[header_len], payload_len)
		if frame.fin {
			if ws.fragments.len > 0 {
				// join fragments
				ws.fragments << Fragment{
					data: payload
					len: payload_len
				}
				mut frags := []Fragment{}
				mut size := u64(0)
				for f in ws.fragments {
					if f.len > 0 {
						frags << f
						size += f.len
					}
				}
				mut pl := malloc(sizeof(byte) * u32(size))
				if pl == 0 {
					l.f('out of memory')
				}
				mut by := 0
				for f in frags {
					C.memcpy(pl + by, f.data, f.len)
					by += int(f.len)
					unsafe {
						free(f.data)
					}
				}
				payload = pl
				frame.opcode = ws.fragments[0].code
				payload_len = size
				// clear the fragments
				unsafe {
					ws.fragments.free()
				}
				ws.fragments = []
			}
			payload[payload_len] = `\0`
			if frame.opcode == .text_frame && payload_len > 0 {
				if !utf8_validate(payload, int(payload_len)) {
					l.e('malformed utf8 payload')
					ws.send_error_event('Recieved malformed utf8.')
					ws.close(1007, 'malformed utf8 payload')
					goto free_data
					return -1
				}
			}
			message := Message{
				opcode: frame.opcode
				payload: payload
				payload_len: int(payload_len)
			}
			ws.send_message_event(message)
		} else {
			// fragment start.
			ws.fragments << Fragment{
				data: payload
				len: payload_len
				code: frame.opcode
			}
		}
		unsafe {
			free(data)
		}
		return int(bytes_read)
	} else if frame.opcode == .continuation {
		l.d('read: continuation')
		if ws.fragments.len <= 0 {
			l.e('Nothing to continue.')
			ws.close(1002, 'nothing to continue')
			goto free_data
			return -1
		}
		goto data_node
		return 0
	} else if frame.opcode == .ping {
		l.d('read: ping')
		if !frame.fin {
			ws.close(1002, 'control message must not be fragmented')
			goto free_data
			return -1
		}
		if frame.payload_len > 125 {
			ws.close(1002, 'control frames must not exceed 125 bytes')
			goto free_data
			return -1
		}
		mut payload := []byte{}
		if payload_len > 0 {
			payload = [`0`].repeat(int(payload_len))
			C.memcpy(payload.data, &data[header_len], payload_len)
		}
		unsafe {
			free(data)
		}
		return ws.send_control_frame(.pong, 'PONG', payload)
	} else if frame.opcode == .pong {
		if !frame.fin {
			ws.close(1002, 'control message must not be fragmented')
			goto free_data
			return -1
		}
		unsafe {
			free(data)
		}
		// got pong
		return 0
	} else if frame.opcode == .close {
		l.d('read: close')
		if frame.payload_len > 125 {
			ws.close(1002, 'control frames must not exceed 125 bytes')
			goto free_data
			return -1
		}
		mut code := 0
		mut reason := ''
		if payload_len > 2 {
			code = (int(data[header_len]) << 8) + int(data[header_len + 1])
			header_len += 2
			payload_len -= 2
			reason = string(&data[header_len])
			l.i('Closing with reason: ${reason} & code: ${code}')
			if reason.len > 1 && !utf8_validate(reason.str, reason.len) {
				l.e('malformed utf8 payload')
				ws.send_error_event('Recieved malformed utf8.')
				ws.close(1007, 'malformed utf8 payload')
				goto free_data
				return -1
			}
		}
		unsafe {
			free(data)
		}
		ws.close(code, reason)
		return 0
	}
	l.e('read: Recieved unsupported opcode: ${frame.opcode} fin: ${frame.fin} uri: ${ws.uri}')
	ws.send_error_event('Recieved unsupported opcode: ${frame.opcode}')
	ws.close(1002, 'Unsupported opcode')
	free_data:
	unsafe {
		free(data)
	}
	return -1
}

fn (mut ws Client) send_control_frame(code OPCode, frame_typ string, payload []byte) int {
	mut bytes_written := -1
	if ws.socket.sockfd <= 0 {
		l.e('No socket opened.')
		unsafe { 
			payload.free()
		}
		l.c('send_control_frame: error sending ${frame_typ} control frame.')
		return -1
	}
	header_len := 6
	frame_len := header_len + payload.len
	mut control_frame := [`0`].repeat(frame_len)
	masking_key := create_masking_key()
	control_frame[0] = byte(code | 0x80)
	control_frame[1] = byte(payload.len | 0x80)
	control_frame[2] = masking_key[0]
	control_frame[3] = masking_key[1]
	control_frame[4] = masking_key[2]
	control_frame[5] = masking_key[3]
	if code == .close {
		if payload.len > 2 {
			mut parsed_payload := [`0`].repeat(payload.len + 1)
			C.memcpy(parsed_payload.data, &payload[0], payload.len)
			parsed_payload[payload.len] = `\0`
			for i in 0 .. payload.len {
				control_frame[6 + i] = (parsed_payload[i] ^ masking_key[i % 4]) & 0xff
			}
			unsafe {
				parsed_payload.free()
			}
		}
	} else {
		for i in 0 .. payload.len {
			control_frame[header_len + i] = (payload[i] ^ masking_key[i % 4]) & 0xff
		}
	}
	bytes_written = ws.write_to_server(control_frame.data, frame_len)
	free_data:
	unsafe {
		control_frame.free()
		payload.free()
		masking_key.free()
	}
	match bytes_written {
		0 {
			l.d('send_control_frame: remote host closed the connection.')
			return 0
		}
		-1 {
			l.c('send_control_frame: error sending ${frame_typ} control frame.')
			return -1
		}
		else {
			l.d('send_control_frame: wrote ${bytes_written} byte ${frame_typ} frame.')
			return bytes_written
		}
	}
}
