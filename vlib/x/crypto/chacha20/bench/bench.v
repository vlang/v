// This is a benchmark for`x.crypto.chacha20` encryption and decryption
//
// Current output on my tests
//
// Chacha20 Encryption
// -----------
// Iterations:      10000          Total Duration:   76.045ms      ns/op:       7604       B/op:      4    allocs/op:      2
//
// ChaCha20 Decryption
// -----------
// Iterations:      10000          Total Duration:   71.275ms      ns/op:       7127       B/op:     11    allocs/op:     14
//
// After the patch
// Chacha20 Encryption
// -----------
// Iterations:      10000          Total Duration:   46.833ms      ns/op:       4683       B/op:     11    allocs/op:     11
//
// ChaCha20 Decryption
// -----------
// Iterations:      10000          Total Duration:   48.242ms      ns/op:       4824       B/op:      3    allocs/op:      4
//
// Chacha20 old xor_key_stream_backup
// -----------
// Iterations:      10000          Total Duration:   53.430ms      ns/op:       5343       B/op:     11    allocs/op:     12
// ChaCha20 new xor_key_stream
// -----------
// Iterations:      10000          Total Duration:   43.668ms      ns/op:       4366       B/op:      0    allocs/op:      1
//
import x.benchmark
import encoding.hex
import x.crypto.chacha20

// randomly generated key and nonce, 32-bytes of key, 12-bytes of nonce
const key = hex.decode('9d9603f4fc460e273b80795ea50eab5873c04f589226c7d591b5336feb32fcba')!
const nonce = hex.decode('9a3c83e4236ea9a2c4e482da')!

const plaintext = 'ChaCha20 encrypt decrypt benchmarking message'.bytes()

// expected ciphertext
const ciphertext = hex.decode('dbddb264e4c478d96805b2d557649232b4b3f37c51035464d12e3675e5e36ce6f6822b49dd6494ccd5213a89c9')!

fn bench_chacha20_encrypt() ! {
	_ := chacha20.encrypt(key, nonce, plaintext)!
}

fn bench_chacha20_decrypt() ! {
	_ := chacha20.decrypt(key, nonce, ciphertext)!
}

fn bench_chacha20_xor_key_stream() ! {
	mut dst := []u8{len: plaintext.len}
	mut cs := chacha20.new_cipher(key, nonce)!
	cs.xor_key_stream(mut dst, plaintext)
}

fn main() {
	cf := benchmark.BenchmarkDefaults{
		n: 10000
	}
	println('Chacha20 Encryption')
	println('-----------')
	mut b0 := benchmark.setup(bench_chacha20_encrypt, cf)!
	b0.run()

	println('ChaCha20 Decryption')
	println('-----------')
	mut b1 := benchmark.setup(bench_chacha20_decrypt, cf)!
	b1.run()

	println('ChaCha20 new xor_key_stream')
	println('-----------')
	mut b3 := benchmark.setup(bench_chacha20_xor_key_stream, cf)!
	b3.run()
}
