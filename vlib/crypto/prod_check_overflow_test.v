// vtest vflags: -prod -check-overflow
// Hashes, ciphers and key derivations wrap on purpose; under `-check-overflow`
// each must still run, and give its usual answer.
import crypto.argon2
import crypto.blake2b
import crypto.blake2s
import crypto.blake3
import crypto.blowfish
import crypto.ed25519
import crypto.hmac
import crypto.md5
import crypto.pbkdf2
import crypto.rc4
import crypto.ripemd160
import crypto.scrypt
import crypto.sha1
import crypto.sha256
import crypto.sha512
import crypto.subtle
import hash.fnv1a
import math.big

fn test_the_hashes_run_and_answer_as_usual() {
	for n in [0, 1, 55, 56, 63, 64, 65, 127, 128, 129, 1000] {
		data := 'x'.repeat(n).bytes()
		assert md5.sum(data).len == 16
		assert sha1.sum(data).len == 20
		assert sha256.sum(data).len == 32
		assert sha512.sum512(data).len == 64
		assert blake2b.sum512(data).len == 64
		assert blake2s.sum256(data).len == 32
		assert blake3.sum256(data).len == 32
		assert ripemd160.hexhash(data.bytestr()).len == 40
	}
	assert sha256.hexhash('abc') == 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
	assert sha1.hexhash('abc') == 'a9993e364706816aba3e25717850c26c9cd0d89d'
	assert md5.hexhash('abc') == '900150983cd24fb0d6963f7d28e17f72'
	assert fnv1a.sum64_string('abc') == 0xe71fa2190541574b
	assert 'abcdefghijklmnopqrstuvwxyz'.hash() != 0
}

fn test_the_key_derivations_and_macs_run() {
	assert argon2.id_key('password'.bytes(), 'somesalt'.bytes(), 2, 64, 1, 32)!.len == 32
	assert scrypt.scrypt('password'.bytes(), 'salt'.bytes(), 16, 1, 1, 32)!.len == 32
	assert pbkdf2.key('password'.bytes(), 'salt'.bytes(), 2, 32, sha256.new())!.len == 32
	assert hmac.new('key'.bytes(), 'data'.bytes(), sha256.sum, sha256.block_size).len == 32
	assert subtle.constant_time_byte_eq(3, 3) == 1 && subtle.constant_time_byte_eq(3, 4) == 0
}

fn test_the_ciphers_and_signatures_run() {
	mut stream := rc4.new_cipher('Key'.bytes())!
	mut out := []u8{len: 9}
	stream.xor_key_stream(mut out, 'Plaintext'.bytes())
	assert out.hex() == 'bbf316e8d940af0ad3'
	_ := blowfish.new_cipher('key'.bytes())!
	public, private := ed25519.generate_key()!
	signature := ed25519.sign(private, 'message'.bytes())!
	assert ed25519.verify(public, 'message'.bytes(), signature)!
	assert big.integer_from_int(7).pow(20).mod_pow(3, big.integer_from_int(1000003)).str().len > 0
}
