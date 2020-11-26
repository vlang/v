// The websocket client implements the websocket capabilities
// it is a refactor of the original V-websocket client class
// from @thecoderr.
// There are quite a few manual memory management free() going on
// int the code. This will be refactored once the memory management
// is done. For now there are no leaks on message levels. Please
// check with valgrind if you do any changes in the free calls
module websocket

import net
import x.openssl
import net.urllib
import time
import log
import sync
import rand

const (
	empty_bytearr = []byte{}
)

// Client represents websocket client state
pub struct Client {
	is_server         bool
mut:
	ssl_conn          &openssl.SSLConn
	flags             []Flag
	fragments         []Fragment
	message_callbacks []MessageEventHandler
	error_callbacks   []ErrorEventHandler
	open_callbacks    []OpenEventHandler
	close_callbacks   []CloseEventHandler
pub:
	is_ssl            bool
	uri               Uri
	id                string
pub mut:
	conn              net.TcpConn
	nonce_size        int = 16 // you can try 18 too
	panic_on_callback bool
	state             State
	logger            &log.Log
	resource_name     string
	last_pong_ut      u64
}

enum Flag {
	has_accept
	has_connection
	has_upgrade
}

// State of the websocket connection.
// Messages should be sent only on state .open
enum State {
	connecting = 0
	open
	closing
	closed
}

// Message, represents a whole message conbined from 1 to n frames
pub struct Message {
pub:
	opcode  OPCode
	payload []byte
}

// OPCode, the supported websocket frame types
pub enum OPCode {
	continuation = 0x00
	text_frame = 0x01
	binary_frame = 0x02
	close = 0x08
	ping = 0x09
	pong = 0x0A
}

// new_client, instance a new websocket client
pub fn new_client(address string) ?&Client {
	uri := parse_uri(address)?
	return &Client{
		is_server: false
		ssl_conn: openssl.new_ssl_conn()
		is_ssl: address.starts_with('wss')
		logger: &log.Log{
			level: .info
		}
		uri: uri
		state: .closed
		id: rand.uuid_v4()
	}
}

// connect, connects and do handshake procedure with remote server
pub fn (mut ws Client) connect() ? {
	ws.assert_not_connected()?
	ws.set_state(.connecting)
	ws.logger.info('connecting to host $ws.uri')
	ws.conn = ws.dial_socket()?
	// Todo: make setting configurable
	ws.conn.set_read_timeout(time.second * 30)
	ws.conn.set_write_timeout(time.second * 30)
	ws.handshake()?
	ws.set_state(.open)
	ws.logger.info('successfully connected to host $ws.uri')
	ws.send_open_event()
}

// listen, listens to incoming messages and handles them
pub fn (mut ws Client) listen() ? {
	ws.logger.info('Starting client listener, server($ws.is_server)...')
	defer {
		ws.logger.info('Quit client listener, server($ws.is_server)...')
		if ws.state == .open {
			ws.close(1000, 'closed by client')
		}
	}
	for ws.state == .open {
		msg := ws.read_next_message() or {
			if ws.state in [.closed, .closing] {
				return
			}
			ws.debug_log('failed to read next message: $err')
			ws.send_error_event('failed to read next message: $err')
			return error(err)
		}
		if ws.state in [.closed, .closing] {
				return
		}
		ws.debug_log('got message: $msg.opcode') // , payload: $msg.payload') leaks
		match msg.opcode {
			.text_frame {
				ws.debug_log('read: text')
				ws.send_message_event(msg)
				unsafe {
					msg.free()
				}
			}
			.binary_frame {
				ws.debug_log('read: binary')
				ws.send_message_event(msg)
				unsafe {
					msg.free()
				}
			}
			.ping {
				ws.debug_log('read: ping, sending pong')
				ws.send_control_frame(.pong, 'PONG', msg.payload) or {
					ws.logger.error('error in message callback sending PONG: $err')
					ws.send_error_event('error in message callback sending PONG: $err')
					if ws.panic_on_callback {
						panic(err)
					}
					continue
				}
				if msg.payload.len > 0 {
					unsafe {
						msg.free()
					}
				}
			}
			.pong {
				ws.debug_log('read: pong')
				ws.last_pong_ut = time.now().unix
				ws.send_message_event(msg)
				if msg.payload.len > 0 {
					unsafe {
						msg.free()
					}
				}
			}
			.close {
				ws.debug_log('read: close')
				defer {
					ws.manage_clean_close()
				}
				if msg.payload.len > 0 {
					if msg.payload.len == 1 {
						ws.close(1002, 'close payload cannot be 1 byte')?
						return error('close payload cannot be 1 byte')
					}
					code := (int(msg.payload[0]) << 8) + int(msg.payload[1])
					if code in invalid_close_codes {
						ws.close(1002, 'invalid close code: $code')?
						return error('invalid close code: $code')
					}
					reason := if msg.payload.len > 2 { msg.payload[2..] } else { []byte{} }
					if reason.len > 0 {
						ws.validate_utf_8(.close, reason)?
					}
					if ws.state !in [.closing, .closed] {
						// sending close back according to spec
						ws.debug_log('close with reason, code: $code, reason: $reason')
						r := reason.bytestr()
						ws.close(code, r)?
					}
					unsafe {
						msg.free()
					}
				} else {
					if ws.state !in [.closing, .closed] {
						ws.debug_log('close with reason, no code')
						// sending close back according to spec
						ws.close(1000, 'normal')?
					}
					unsafe {
						msg.free()
					}
				}
				return
			}
			.continuation {
				ws.logger.error('unexpected opcode continuation, nothing to continue')
				ws.send_error_event('unexpected opcode continuation, nothing to continue')
				ws.close(1002, 'nothing to continue')?
				return error('unexpected opcode continuation, nothing to continue')
			}
		}
	}
}

// this function was needed for defer
fn (mut ws Client) manage_clean_close() {
	ws.send_close_event(1000, 'closed by client')
}

// ping, sends ping message to server,
// ping response will be pushed to message callback
pub fn (mut ws Client) ping() ? {
	ws.send_control_frame(.ping, 'PING', [])?
}

// pong, sends pog message to server,
// pongs are normally automatically sent back to server
pub fn (mut ws Client) pong() ? {
	ws.send_control_frame(.pong, 'PONG', [])?
}

// write_ptr, writes len bytes provided a byteptr with a websocket messagetype
pub fn (mut ws Client) write_ptr(bytes byteptr, payload_len int, code OPCode) ? {
	// Temporary, printing bytes are leaking
	ws.debug_log('write code: $code')
	// ws.debug_log('write code: $code, payload: $bytes')
	if ws.state != .open || ws.conn.sock.handle < 1 {
		// send error here later
		return error('trying to write on a closed socket!')
	}
	// payload_len := bytes.len
	mut header_len := 2 + if payload_len > 125 { 2 } else { 0 } + if payload_len > 0xffff { 6 } else { 0 }
	if !ws.is_server {
		header_len += 4
	}
	mut header := []byte{len: header_len, init: `0`} // [`0`].repeat(header_len)
	header[0] = byte(int(code)) | 0x80
	masking_key := create_masking_key()
	defer {
		unsafe {
		}
	}
	if ws.is_server {
		if payload_len <= 125 {
			header[1] = byte(payload_len)
			// 0x80
		} else if payload_len > 125 && payload_len <= 0xffff {
			len16 := C.htons(payload_len)
			header[1] = 126
			// 0x80
			// todo: fix v style copy instead
			unsafe {
				C.memcpy(&header[2], &len16, 2)
			}
		} else if payload_len > 0xffff && payload_len <= 0xffffffffffffffff {
			len_bytes := htonl64(u64(payload_len))
			header[1] = 127 // 0x80
			// todo: fix v style copy instead
			unsafe {
				C.memcpy(&header[2], len_bytes.data, 8)
			}
		}
	} else {
		if payload_len <= 125 {
			header[1] = byte(payload_len | 0x80)
			header[2] = masking_key[0]
			header[3] = masking_key[1]
			header[4] = masking_key[2]
			header[5] = masking_key[3]
		} else if payload_len > 125 && payload_len <= 0xffff {
			len16 := C.htons(payload_len)
			header[1] = (126 | 0x80)
			// todo: fix v style copy instead
			unsafe {
				C.memcpy(&header[2], &len16, 2)
			}
			header[4] = masking_key[0]
			header[5] = masking_key[1]
			header[6] = masking_key[2]
			header[7] = masking_key[3]
		} else if payload_len > 0xffff && payload_len <= 0xffffffffffffffff { // 65535 && 18446744073709551615
			len64 := htonl64(u64(payload_len))
			header[1] = (127 | 0x80)
			// todo: fix v style copy instead
			unsafe {
				C.memcpy(&header[2], len64.data, 8)
			}
			header[10] = masking_key[0]
			header[11] = masking_key[1]
			header[12] = masking_key[2]
			header[13] = masking_key[3]
		} else {
			// l.c('write: frame too large')
			ws.close(1009, 'frame too large')?
			return error('frame too large')
		}
	}
	len := header.len + payload_len
	mut frame_buf := []byte{len: len}
	unsafe {
		C.memcpy(&frame_buf[0], byteptr(header.data), header.len)
		if payload_len > 0 {
			C.memcpy(&frame_buf[header.len], bytes, payload_len)
		}
	}
	if !ws.is_server {
		for i in 0 .. payload_len {
			frame_buf[header_len + i] ^= masking_key[i % 4] & 0xff
		}
	}
	ws.socket_write(frame_buf)?
	// Temporary hack until memory management is done
	unsafe {
		frame_buf.free()
		masking_key.free()
		header.free()
	}
}

// write, writes a byte array with a websocket messagetype
pub fn (mut ws Client) write(bytes []byte, code OPCode) ? {
	ws.write_ptr(byteptr(bytes.data), bytes.len, code)?
}

pub fn (mut ws Client) write_str(str string) ? {
	ws.write_ptr(str.str, str.len, .text_frame)
}

// close, closes the websocket connection
pub fn (mut ws Client) close(code int, message string) ? {
	ws.debug_log('sending close, $code, $message')
	if ws.state in [.closed, .closing] || ws.conn.sock.handle <= 1 {
		ws.debug_log('close: Websocket allready closed ($ws.state), $message, $code handle($ws.conn.sock.handle)')
		err_msg := 'Socket allready closed: $code'
		ret_err := error(err_msg)
		return ret_err
	}
	defer {
		ws.shutdown_socket()
		ws.reset_state()
	}
	ws.set_state(.closing)
	mut code32 := 0
	if code > 0 {
		code_ := C.htons(code)
		message_len := message.len + 2
		mut close_frame := []byte{len: message_len} // [`0`].repeat(message_len)
		close_frame[0] = byte(code_ & 0xFF)
		close_frame[1] = byte(code_ >> 8)
		code32 = (close_frame[0] << 8) + close_frame[1]
		for i in 0 .. message.len {
			close_frame[i + 2] = message[i]
		}
		ws.send_control_frame(.close, 'CLOSE', close_frame)?
		ws.send_close_event(code, message)
		unsafe {
			close_frame.free()
		}
	} else {
		ws.send_control_frame(.close, 'CLOSE', [])?
		ws.send_close_event(code, '')
	}
	ws.fragments = []
}

// send_control_frame, sends a control frame to the server
fn (mut ws Client) send_control_frame(code OPCode, frame_typ string, payload []byte) ? {
	ws.debug_log('send control frame $code, frame_type: $frame_typ') // , payload: $payload')
	if ws.state !in [.open, .closing] && ws.conn.sock.handle > 1 {
		return error('socket is not connected')
	}
	header_len := if ws.is_server { 2 } else { 6 }
	frame_len := header_len + payload.len
	mut control_frame := []byte{len: frame_len} // [`0`].repeat(frame_len)
	mut masking_key := if !ws.is_server { create_masking_key() } else { empty_bytearr }
	defer {
		unsafe {
			control_frame.free()
			if masking_key.len > 0 {
				masking_key.free()
			}
		}
	}
	control_frame[0] = byte(int(code) | 0x80)
	if !ws.is_server {
		control_frame[1] = byte(payload.len | 0x80)
		control_frame[2] = masking_key[0]
		control_frame[3] = masking_key[1]
		control_frame[4] = masking_key[2]
		control_frame[5] = masking_key[3]
	} else {
		control_frame[1] = byte(payload.len)
	}
	if code == .close {
		if payload.len >= 2 {
			if !ws.is_server {
				mut parsed_payload := []byte{len: payload.len + 1}
				unsafe {
					C.memcpy(parsed_payload.data, &payload[0], payload.len)
				}
				parsed_payload[payload.len] = `\0`
				for i in 0 .. payload.len {
					control_frame[6 + i] = (parsed_payload[i] ^ masking_key[i % 4]) & 0xff
				}
				unsafe {
					parsed_payload.free()
				}
			} else {
				unsafe {
					C.memcpy(&control_frame[2], &payload[0], payload.len)
				}
			}
		}
	} else {
		if !ws.is_server {
			if payload.len > 0 {
				for i in 0 .. payload.len {
					control_frame[header_len + i] = (payload[i] ^ masking_key[i % 4]) & 0xff
				}
			}
		} else {
			if payload.len > 0 {
				unsafe {
					C.memcpy(&control_frame[2], &payload[0], payload.len)
				}
			}
		}
	}
	ws.socket_write(control_frame) or {
		return error('send_control_frame: error sending $frame_typ control frame.')
	}
}

// parse_uri, parses the url string to it's components
fn parse_uri(url string) ?&Uri {
	u := urllib.parse(url)?
	v := u.request_uri().split('?')
	mut port := u.port()
	if port == '' {
		port = if u.str().starts_with('ws://') {
				'80'
			} else if u.str().starts_with('wss://') {
				'443'
			} else {
				u.port()
			}
	}
	querystring := if v.len > 1 { '?' + v[1] } else { '' }
	return &Uri{
		url: url
		hostname: u.hostname()
		port: port
		resource: v[0]
		querystring: querystring
	}
}

// set_state sets current state in a thread safe way
fn (mut ws Client) set_state(state State) {
	lock  {
		ws.state = state
	}
}

[inline]
fn (ws Client) assert_not_connected() ? {
	match ws.state {
		.connecting { return error('connect: websocket is connecting') }
		.open { return error('connect: websocket already open') }
		.closing { return error('connect: reconnect on closing websocket not supported, please use new client') }
		else {}
	}
}

// reset_state, resets the websocket and can connect again
fn (mut ws Client) reset_state() {
	lock  {
		ws.state = .closed
		ws.ssl_conn = openssl.new_ssl_conn()
		ws.flags = []
		ws.fragments = []
	}
}

fn (mut ws Client) debug_log(text string) {
	if ws.is_server {
		ws.logger.debug('server-> $text')
	} else {
		ws.logger.debug('client-> $text')
	}
}

[unsafe]
pub fn (m &Message) free() {
	unsafe {
		m.payload.free()
	}
}

[unsafe]
pub fn (c &Client) free() {
	unsafe {
		c.flags.free()
		c.fragments.free()
		c.message_callbacks.free()
		c.error_callbacks.free()
		c.open_callbacks.free()
		c.close_callbacks.free()
	}
}
