// websocket module implements websocket client and a websocket server
// attribution: @thecoderr the author of original websocket client
[manualfree]
module websocket

import net
import net.http
import net.openssl
import net.urllib
import time
import log
import rand

const (
	empty_bytearr = []u8{} // used as empty response to avoid allocation
)

// Client represents websocket client
pub struct Client {
	is_server bool
mut:
	ssl_conn          &openssl.SSLConn // secure connection used when wss is used
	flags             []Flag     // flags used in handshake
	fragments         []Fragment // current fragments
	message_callbacks []MessageEventHandler // all callbacks on_message
	error_callbacks   []ErrorEventHandler   // all callbacks on_error
	open_callbacks    []OpenEventHandler    // all callbacks on_open
	close_callbacks   []CloseEventHandler   // all callbacks on_close
pub:
	is_ssl bool   // true if secure socket is used
	uri    Uri    // uri of current connection
	id     string // unique id of client
pub mut:
	header            http.Header  // headers that will be passed when connecting
	conn              &net.TcpConn // underlying TCP socket connection
	nonce_size        int = 16 // size of nounce used for masking
	panic_on_callback bool     // set to true of callbacks can panic
	state             State    // current state of connection
	logger            &log.Log // logger used to log messages
	resource_name     string   // name of current resource
	last_pong_ut      i64      // last time in unix time we got a pong message
}

// Flag represents different types of headers in websocket handshake
enum Flag {
	has_accept // Webs
	has_connection
	has_upgrade
}

// State represents the state of the websocket connection.
pub enum State {
	connecting = 0
	open
	closing
	closed
}

// Message represents a whole message combined from 1 to n frames
pub struct Message {
pub:
	opcode  OPCode // websocket frame type of this message
	payload []u8   // payload of the message
}

// OPCode represents the supported websocket frame types
pub enum OPCode {
	continuation = 0x00
	text_frame = 0x01
	binary_frame = 0x02
	close = 0x08
	ping = 0x09
	pong = 0x0A
}

// new_client instance a new websocket client
pub fn new_client(address string) ?&Client {
	uri := parse_uri(address) ?
	return &Client{
		conn: 0
		is_server: false
		ssl_conn: openssl.new_ssl_conn()
		is_ssl: address.starts_with('wss')
		logger: &log.Log{
			level: .info
		}
		uri: uri
		state: .closed
		id: rand.uuid_v4()
		header: http.new_header()
	}
}

// connect connects to remote websocket server
pub fn (mut ws Client) connect() ? {
	ws.assert_not_connected() ?
	ws.set_state(.connecting)
	ws.logger.info('connecting to host $ws.uri')
	ws.conn = ws.dial_socket() ?
	// Todo: make setting configurable
	ws.conn.set_read_timeout(time.second * 30)
	ws.conn.set_write_timeout(time.second * 30)
	ws.handshake() ?
	ws.set_state(.open)
	ws.logger.info('successfully connected to host $ws.uri')
	ws.send_open_event()
}

// listen listens and processes incoming messages
pub fn (mut ws Client) listen() ? {
	mut log := 'Starting client listener, server($ws.is_server)...'
	ws.logger.info(log)
	unsafe { log.free() }
	defer {
		ws.logger.info('Quit client listener, server($ws.is_server)...')
		if ws.state == .open {
			ws.close(1000, 'closed by client') or {}
		}
	}
	for ws.state == .open {
		msg := ws.read_next_message() or {
			if ws.state in [.closed, .closing] {
				return
			}
			ws.debug_log('failed to read next message: $err')
			ws.send_error_event('failed to read next message: $err')
			return err
		}
		if ws.state in [.closed, .closing] {
			return
		}
		ws.debug_log('got message: $msg.opcode')
		match msg.opcode {
			.text_frame {
				log = 'read: text'
				ws.debug_log(log)
				unsafe { log.free() }
				ws.send_message_event(msg)
				unsafe { msg.free() }
			}
			.binary_frame {
				ws.debug_log('read: binary')
				ws.send_message_event(msg)
				unsafe { msg.free() }
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
					unsafe { msg.free() }
				}
			}
			.pong {
				ws.debug_log('read: pong')
				ws.last_pong_ut = time.now().unix
				ws.send_message_event(msg)
				if msg.payload.len > 0 {
					unsafe { msg.free() }
				}
			}
			.close {
				log = 'read: close'
				ws.debug_log(log)
				unsafe { log.free() }
				defer {
					ws.manage_clean_close()
				}
				if msg.payload.len > 0 {
					if msg.payload.len == 1 {
						ws.close(1002, 'close payload cannot be 1 byte') ?
						return error('close payload cannot be 1 byte')
					}
					code := u16(msg.payload[0]) << 8 | u16(msg.payload[1])
					if code in invalid_close_codes {
						ws.close(1002, 'invalid close code: $code') ?
						return error('invalid close code: $code')
					}
					reason := if msg.payload.len > 2 { msg.payload[2..] } else { []u8{} }
					if reason.len > 0 {
						ws.validate_utf_8(.close, reason) ?
					}
					if ws.state !in [.closing, .closed] {
						// sending close back according to spec
						ws.debug_log('close with reason, code: $code, reason: $reason')
						r := reason.bytestr()
						ws.close(code, r) ?
					}
					unsafe { msg.free() }
				} else {
					if ws.state !in [.closing, .closed] {
						ws.debug_log('close with reason, no code')
						// sending close back according to spec
						ws.close(1000, 'normal') ?
					}
					unsafe { msg.free() }
				}
				return
			}
			.continuation {
				ws.logger.error('unexpected opcode continuation, nothing to continue')
				ws.send_error_event('unexpected opcode continuation, nothing to continue')
				ws.close(1002, 'nothing to continue') ?
				return error('unexpected opcode continuation, nothing to continue')
			}
		}
	}
}

// manage_clean_close closes connection in a clean websocket way
fn (mut ws Client) manage_clean_close() {
	ws.send_close_event(1000, 'closed by client')
}

// ping sends ping message to server
pub fn (mut ws Client) ping() ? {
	ws.send_control_frame(.ping, 'PING', []) ?
}

// pong sends pong message to server,
pub fn (mut ws Client) pong() ? {
	ws.send_control_frame(.pong, 'PONG', []) ?
}

// write_ptr writes len bytes provided a byteptr with a websocket messagetype
pub fn (mut ws Client) write_ptr(bytes &u8, payload_len int, code OPCode) ?int {
	// ws.debug_log('write_ptr code: $code')
	if ws.state != .open || ws.conn.sock.handle < 1 {
		// todo: send error here later
		return error('trying to write on a closed socket!')
	}
	mut header_len := 2 + if payload_len > 125 { 2 } else { 0 } +
		if payload_len > 0xffff { 6 } else { 0 }
	if !ws.is_server {
		header_len += 4
	}
	mut header := []u8{len: header_len, init: `0`} // [`0`].repeat(header_len)
	header[0] = u8(int(code)) | 0x80
	masking_key := create_masking_key()
	if ws.is_server {
		if payload_len <= 125 {
			header[1] = u8(payload_len)
		} else if payload_len > 125 && payload_len <= 0xffff {
			len16 := C.htons(payload_len)
			header[1] = 126
			unsafe { C.memcpy(&header[2], &len16, 2) }
		} else if payload_len > 0xffff && payload_len <= 0x7fffffff {
			len_bytes := htonl64(u64(payload_len))
			header[1] = 127
			unsafe { C.memcpy(&header[2], len_bytes.data, 8) }
		}
	} else {
		if payload_len <= 125 {
			header[1] = u8(payload_len | 0x80)
			header[2] = masking_key[0]
			header[3] = masking_key[1]
			header[4] = masking_key[2]
			header[5] = masking_key[3]
		} else if payload_len > 125 && payload_len <= 0xffff {
			len16 := C.htons(payload_len)
			header[1] = (126 | 0x80)
			unsafe { C.memcpy(&header[2], &len16, 2) }
			header[4] = masking_key[0]
			header[5] = masking_key[1]
			header[6] = masking_key[2]
			header[7] = masking_key[3]
		} else if payload_len > 0xffff && payload_len <= 0x7fffffff {
			len64 := htonl64(u64(payload_len))
			header[1] = (127 | 0x80)
			unsafe { C.memcpy(&header[2], len64.data, 8) }
			header[10] = masking_key[0]
			header[11] = masking_key[1]
			header[12] = masking_key[2]
			header[13] = masking_key[3]
		} else {
			ws.close(1009, 'frame too large') ?
			return error('frame too large')
		}
	}
	len := header.len + payload_len
	mut frame_buf := []u8{len: len}
	unsafe {
		C.memcpy(&frame_buf[0], &u8(header.data), header.len)
		if payload_len > 0 {
			C.memcpy(&frame_buf[header.len], bytes, payload_len)
		}
	}
	if !ws.is_server {
		for i in 0 .. payload_len {
			frame_buf[header_len + i] ^= masking_key[i % 4] & 0xff
		}
	}
	written_len := ws.socket_write(frame_buf) ?
	unsafe {
		frame_buf.free()
		masking_key.free()
		header.free()
	}
	return written_len
}

// write writes a byte array with a websocket messagetype to socket
pub fn (mut ws Client) write(bytes []u8, code OPCode) ?int {
	return ws.write_ptr(&u8(bytes.data), bytes.len, code)
}

// write_str, writes a string with a websocket texttype to socket
pub fn (mut ws Client) write_string(str string) ?int {
	return ws.write_ptr(str.str, str.len, .text_frame)
}

// close closes the websocket connection
pub fn (mut ws Client) close(code int, message string) ? {
	ws.debug_log('sending close, $code, $message')
	if ws.state in [.closed, .closing] || ws.conn.sock.handle <= 1 {
		ws.debug_log('close: Websocket allready closed ($ws.state), $message, $code handle($ws.conn.sock.handle)')
		err_msg := 'Socket allready closed: $code'
		return error(err_msg)
	}
	defer {
		ws.shutdown_socket() or {}
		ws.reset_state()
	}
	ws.set_state(.closing)
	// mut code32 := 0
	if code > 0 {
		code_ := C.htons(code)
		message_len := message.len + 2
		mut close_frame := []u8{len: message_len}
		close_frame[0] = u8(code_ & 0xFF)
		close_frame[1] = u8(code_ >> 8)
		// code32 = (close_frame[0] << 8) + close_frame[1]
		for i in 0 .. message.len {
			close_frame[i + 2] = message[i]
		}
		ws.send_control_frame(.close, 'CLOSE', close_frame) ?
		unsafe { close_frame.free() }
	} else {
		ws.send_control_frame(.close, 'CLOSE', []) ?
	}
	ws.fragments = []
}

// send_control_frame sends a control frame to the server
fn (mut ws Client) send_control_frame(code OPCode, frame_typ string, payload []u8) ? {
	ws.debug_log('send control frame $code, frame_type: $frame_typ')
	if ws.state !in [.open, .closing] && ws.conn.sock.handle > 1 {
		return error('socket is not connected')
	}
	header_len := if ws.is_server { 2 } else { 6 }
	frame_len := header_len + payload.len
	mut control_frame := []u8{len: frame_len}
	mut masking_key := if !ws.is_server { create_masking_key() } else { websocket.empty_bytearr }
	defer {
		unsafe {
			control_frame.free()
			if masking_key.len > 0 {
				masking_key.free()
			}
		}
	}
	control_frame[0] = u8(int(code) | 0x80)
	if !ws.is_server {
		control_frame[1] = u8(payload.len | 0x80)
		control_frame[2] = masking_key[0]
		control_frame[3] = masking_key[1]
		control_frame[4] = masking_key[2]
		control_frame[5] = masking_key[3]
	} else {
		control_frame[1] = u8(payload.len)
	}
	if code == .close {
		if payload.len >= 2 {
			if !ws.is_server {
				mut parsed_payload := []u8{len: payload.len + 1}
				unsafe { C.memcpy(parsed_payload.data, &payload[0], payload.len) }
				parsed_payload[payload.len] = `\0`
				for i in 0 .. payload.len {
					control_frame[6 + i] = (parsed_payload[i] ^ masking_key[i % 4]) & 0xff
				}
				unsafe { parsed_payload.free() }
			} else {
				unsafe { C.memcpy(&control_frame[2], &payload[0], payload.len) }
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
				unsafe { C.memcpy(&control_frame[2], &payload[0], payload.len) }
			}
		}
	}
	ws.socket_write(control_frame) or {
		return error('send_control_frame: error sending $frame_typ control frame.')
	}
}

// parse_uri parses the url to a Uri
fn parse_uri(url string) ?&Uri {
	u := urllib.parse(url) ?
	request_uri := u.request_uri()
	v := request_uri.split('?')
	mut port := u.port()
	uri := u.str()
	if port == '' {
		port = if uri.starts_with('ws://') {
			'80'
		} else if uri.starts_with('wss://') {
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

// set_state sets current state of the websocket connection
fn (mut ws Client) set_state(state State) {
	lock  {
		ws.state = state
	}
}

// assert_not_connected returns error if the connection is not connected
fn (ws Client) assert_not_connected() ? {
	match ws.state {
		.connecting { return error('connect: websocket is connecting') }
		.open { return error('connect: websocket already open') }
		.closing { return error('connect: reconnect on closing websocket not supported, please use new client') }
		else {}
	}
}

// reset_state resets the websocket and initialize default settings
fn (mut ws Client) reset_state() {
	lock  {
		ws.state = .closed
		ws.ssl_conn = openssl.new_ssl_conn()
		ws.flags = []
		ws.fragments = []
	}
}

// debug_log handles debug logging output for client and server
fn (mut ws Client) debug_log(text string) {
	if ws.is_server {
		ws.logger.debug('server-> $text')
	} else {
		ws.logger.debug('client-> $text')
	}
}

// free handles manual free memory of Message struct
pub fn (m &Message) free() {
	unsafe { m.payload.free() }
}

// free handles manual free memory of Client struct
pub fn (c &Client) free() {
	unsafe {
		c.flags.free()
		c.fragments.free()
		c.message_callbacks.free()
		c.error_callbacks.free()
		c.open_callbacks.free()
		c.close_callbacks.free()
		c.header.free()
	}
}
