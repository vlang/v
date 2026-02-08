import crypto.cipher
import crypto.aes
import crypto.des

struct StreamCipher {
	cipher cipher.Stream
}

fn test_ofb_stream_cipher() ! {
	key := '123456789012345678901234'.bytes()
	iv := 'abcdegfh'.bytes()

	block := des.new_cipher(key[..8])
	c := cipher.new_ofb(block, iv)

	s := StreamCipher{
		cipher: c
	}
}

fn test_ofb_byte_by_byte() {
	key := []u8{len: 16, init: index}
	iv := []u8{len: 16, init: index}
	txt := []u8{len: 32, init: index}
	mut out := []u8{len: 32}

	mut ofb := cipher.new_ofb(aes.new_cipher(key), iv)
	for i in 0 .. 32 {
		ofb.xor_key_stream(mut out[i..i + 1], txt[i..i + 1])
	}
	assert out == [u8(10), 149, 9, 182, 69, 107, 246, 66, 249, 202, 158, 83, 202, 94, 228, 85,
		190, 246, 12, 182, 85, 194, 184, 92, 243, 121, 164, 215, 69, 34, 168, 124]
}
