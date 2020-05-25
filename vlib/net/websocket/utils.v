module websocket

import time
import rand
import math
import crypto.sha1
import encoding.base64

fn htonl64(payload_len u64) byteptr {
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

fn create_masking_key() []byte {
	t := time.ticks()
	tseq := t % 23237671
	mut rnd := rand.new_pcg32(u64(t), u64(tseq))
	mask_bit := byte(rnd.bounded_next(u32(math.max_i32)))
	buf := [`0`].repeat(4)
	C.memcpy(buf.data, &mask_bit, 4)
	return buf
}

fn create_key_challenge_response(seckey string) string {
	guid := '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'
	sha1buf := seckey + guid
	hash := sha1.sum(sha1buf.bytes())
	hashstr := string(byteptr(hash.data))
	b64 := base64.encode(hashstr)
	unsafe {
		sha1buf.free()
		hash.free()
	}
	return b64
}

fn get_nonce() string {
	mut nonce := []byte{}
	alphanum := '0123456789ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz'
	for _ in 0 .. 18 {
		nonce << alphanum[rand.next(61)]
	}
	return string(byteptr(nonce.data))
}
