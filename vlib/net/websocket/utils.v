module websocket

import rand
import crypto.sha1
import encoding.base64

fn htonl64(payload_len u64) byteptr {
	unsafe {
		mut ret := malloc(8)
		ret[0] = byte(((payload_len & (u64(0xff) << 56)) >> 56) & 0xff)
		ret[1] = byte(((payload_len & (u64(0xff) << 48)) >> 48) & 0xff)
		ret[2] = byte(((payload_len & (u64(0xff) << 40)) >> 40) & 0xff)
		ret[3] = byte(((payload_len & (u64(0xff) << 32)) >> 32) & 0xff)
		ret[4] = byte(((payload_len & (u64(0xff) << 24)) >> 24) & 0xff)
		ret[5] = byte(((payload_len & (u64(0xff) << 16)) >> 16) & 0xff)
		ret[6] = byte(((payload_len & (u64(0xff) << 8)) >> 8) & 0xff)
		ret[7] = byte(((payload_len & (u64(0xff) << 0)) >> 0) & 0xff)
		return ret
	}
}

fn create_masking_key() []byte {
	mask_bit := byte(rand.intn(255))
	buf := [`0`].repeat(4)
	unsafe {
		C.memcpy(buf.data, &mask_bit, 4)
	}
	return buf
}

fn create_key_challenge_response(seckey string) string {
	guid := '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
	sha1buf := seckey + guid
	hash := sha1.sum(sha1buf.bytes())
	hashstr := hash.bytestr()
	b64 := base64.encode(hashstr)
	return b64
}

fn get_nonce(nonce_size int) string {
	mut nonce := []byte{len: nonce_size, cap: nonce_size}
	alphanum := '0123456789ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz'
	for i in 0 .. nonce_size {
		nonce[i] = alphanum[rand.intn(alphanum.len)]
	}
	return tos(nonce.data, nonce.len).clone()
}
