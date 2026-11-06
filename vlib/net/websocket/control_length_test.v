module websocket

import io
import net
import time

// test_send_control_frame_rejects_oversize_payload checks that a control
// frame payload longer than the RFC 6455 section 5.5 limit of 125 bytes is
// rejected with an error. Without the check the length byte truncates to 7
// bits, so the peer reads a short payload and treats the remainder as the
// next frame, desynchronising its parser.
// See https://github.com/vlang/v/issues/30175
fn test_send_control_frame_rejects_oversize_payload() {
	mut ws := Client{
		is_server: true
	}
	ws.set_state(.open)
	big := []u8{len: 200}
	if _ := ws.send_control_frame(.ping, 'PING', big) {
		assert false, 'control frame payload over 125 bytes must be rejected'
	} else {
		assert err.msg() == 'control frame payload exceeds 125 bytes'
	}
}

fn control_length_send(mut ws Client, path int, opcode OPCode, payload []u8) ! {
	match path {
		0 { ws.send_control_frame(opcode, 'TEST', payload)! }
		1 { ws.write(payload, opcode)! }
		else { ws.write_ptr(payload.data, payload.len, opcode)! }
	}
}

fn test_control_boundaries_preserve_wire_payload_and_masking() ! {
	for is_server in [false, true] {
		for path in 0 .. 3 {
			for opcode in [OPCode.ping, .pong, .close] {
				mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
				mut socket := net.dial_tcp(listener.addr()!.str())!
				mut peer := listener.accept()!
				listener.close()!
				peer.set_read_timeout(2 * time.second)
				socket.set_write_timeout(2 * time.second)
				mut ws := Client{ is_server: is_server, conn: socket }
				ws.set_state(.open)
				payload := []u8{len: 125, init: u8(index % 127)}
				for size in [126, 200] {
					if _ := control_length_send(mut ws, path, opcode, []u8{len: size}) {
						assert false, 'accepted control payload of ${size} bytes'
					} else {
						assert err.msg() == 'control frame payload exceeds 125 bytes'
					}
				}
				control_length_send(mut ws, path, opcode, payload)!
				ws.conn.close()!
				wire := io.read_all(reader: peer)!
				peer.close()!
				header := if is_server { 2 } else { 6 }
				// No rejected frame prefix precedes the valid frame.
				assert wire.len == header + payload.len
				assert wire[0] == u8(opcode) | 0x80
				assert wire[1] & 0x7f == 125
				assert wire[1] & 0x80 == if is_server { 0 } else { 0x80 }
				for i, byte in payload {
					decoded := if is_server {
						wire[header + i]
					} else {
						wire[header + i] ^ wire[2 + i % 4]
					}
					assert decoded == byte
				}
			}
		}
	}
}

@[unsafe]
fn test_write_control_rejects_before_dereferencing_socket() {
	mut ws := Client{}
	for opcode in [OPCode.ping, .pong, .close] {
		if _ := ws.write([]u8{len: 126}, opcode) {
			assert false
		} else {
			assert err.msg() == 'control frame payload exceeds 125 bytes'
		}
		if _ := ws.write_ptr(unsafe { nil }, 126, opcode) {
			assert false
		} else {
			assert err.msg() == 'control frame payload exceeds 125 bytes'
		}
	}
}

struct CloseLengthEvents {
mut:
	count  int
	code   int
	reason string
	state  State
}

fn close_length_callback(mut ws Client, code int, reason string, mut events CloseLengthEvents) ! {
	events.count++
	events.code = code
	events.reason = reason
	events.state = ws.get_state()
}

fn test_public_close_reason_boundary_preserves_wire_and_lifecycle() ! {
	for is_server in [false, true] {
		for reason_length in [123, 124] {
			mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
			mut socket := net.dial_tcp(listener.addr()!.str())!
			mut peer := listener.accept()!
			listener.close()!
			peer.set_read_timeout(2 * time.second)
			socket.set_write_timeout(2 * time.second)
			mut ws := Client{ is_server: is_server, conn: socket }
			ws.set_state(.open)
			mut events := CloseLengthEvents{}
			ws.on_close_ref(close_length_callback, &events)
			reason := 'reason:'.repeat(18)[..reason_length]
			if reason_length == 123 {
				ws.close(1000, reason)!
			} else {
				if _ := ws.close(1000, reason) {
					assert false, 'a status code and 124-byte reason exceed the control limit'
				} else {
					assert err.msg() == 'control frame payload exceeds 125 bytes'
				}
			}
			wire := io.read_all(reader: peer)!
			peer.close()!
			assert ws.get_state() == .closed
			assert ws.flags.len == 0
			assert ws.fragments.len == 0
			assert events.count == 1
			assert events.code == 1000
			assert events.reason == reason
			assert events.state == .closed
			if reason_length == 124 {
				assert wire.len == 0, 'rejecting a close reason must send no frame bytes'
			} else {
				header := if is_server { 2 } else { 6 }
				assert wire.len == header + 125
				assert wire[0] == 0x88
				assert wire[1] & 0x7f == 125
				assert wire[1] & 0x80 == if is_server { 0 } else { 0x80 }
				mut payload := []u8{len: 125}
				for i in 0 .. payload.len {
					payload[i] = if is_server {
						wire[header + i]
					} else {
						wire[header + i] ^ wire[2 + i % 4]
					}
				}
				assert payload[0] == 3
				assert payload[1] == 232
				assert payload[2..].bytestr() == reason
			}
			if _ := ws.close(1000, reason) {
				assert false, 'a repeated close must report the closed socket'
			} else {
				assert err.msg() == 'Socket already closed: 1000'
			}
			assert events.count == 1
		}
	}
}
