module websocket

import rand
import crypto.sha1
import encoding.base64
import encoding.binary
import log

const default_logger = setup_default_logger()

fn setup_default_logger() &log.Log {
	mut l := &log.Log{}
	l.set_level(.info)
	return l
}

// htonl64 converts payload length to header bits
fn htonl64(payload_len u64) []u8 {
	mut ret := []u8{len: 8}
	binary.big_endian_put_u64(mut ret, payload_len)
	return ret
}

// create_masking_key returns a new masking key to use when masking websocket messages
fn create_masking_key() []u8 {
	return rand.bytes(4) or { [u8(0), 0, 0, 0] }
}

// create_key_challenge_response creates a key challenge response from security key
fn create_key_challenge_response(seckey string) !string {
	if seckey.len == 0 {
		return error('unexpected seckey length zero')
	}
	guid := '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
	sha1buf := seckey + guid
	shabytes := sha1buf.bytes()
	hash := sha1.sum(shabytes)
	b64 := base64.encode(hash)
	unsafe {
		hash.free()
		shabytes.free()
	}
	return b64
}

// get_nonce creates a randomized array used in handshake process
fn get_nonce(nonce_size int) string {
	return rand.string_from_set('0123456789ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz',
		nonce_size)
}
