import crypto.cipher
import crypto.des

struct StreamCipher {
	cipher cipher.Stream
}

fn test_cfb_stream_cipher() ! {
	key := '123456789012345678901234'.bytes()
	iv := 'abcdegfh'.bytes()

	block := des.new_cipher(key[..8])
	c := cipher.new_cfb_encrypter(block, iv)

	s := StreamCipher{
		cipher: c
	}
}
