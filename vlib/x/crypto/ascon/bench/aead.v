// This benchmark is for Ascon-AEAD128 in `x.crypto.ascon` compared to
// already stocked `x.crypto.cacha20poly1305 for AEAD functionalities.
//
// Here is output in my tests, first item was `x.crypto.ascon` and the later
// for `x.crypto.chacha20poly1305` on encryption or decryption part.
//
// Encryption..
// -----------
// Iterations:      10000          Total Duration:   26.008ms      ns/op:       2600       B/op:     16    allocs/op:     17
// Iterations:      10000          Total Duration:  158.865ms      ns/op:      15886       B/op:     16    allocs/op:     16
//
// Decryption..
// -----------
// Iterations:      10000          Total Duration:   29.091ms      ns/op:       2909       B/op:      6    allocs/op:      8
// Iterations:      10000          Total Duration:  158.373ms      ns/op:      15837       B/op:      8    allocs/op:     12
//
import encoding.hex
import x.benchmark
import x.crypto.ascon
import x.crypto.chacha20poly1305

// randomly generated key and nonce, 16-bytes of ascon key and 32-bytes of chacha20poly1305 key.
const key_ascon = hex.decode('7857bfb462c654d1d1b02971be021235')!
const key_cpoly = hex.decode('9d9603f4fc460e273b80795ea50eab5873c04f589226c7d591b5336feb32fcba')!

// 16-bytes ascon-nonce
const ascon_nonce = hex.decode('8b521028fb54591472d8d8ee14430835')!

// 12-bytes chacha20poly1305 nonce
const cpoly_nonce = hex.decode('9a3c83e4236ea9a2c4e482da')!

const ad = 'Ascon-AEAD128 additional data'.bytes()
const msg = 'Ascon-AEAD128 benchmarking message'.bytes()

// expected ciphertext for aead128 := 4b21a18cbca65b11aaf73dc74241c89bfcec96a4c8973ae696a938e0a591e846c4eb7b2906664f2318c0fd6ec1c56424aa9b
const ciphertext_aead128 = hex.decode('4b21a18cbca65b11aaf73dc74241c89bfcec96a4c8973ae696a938e0a591e846c4eb7b2906664f2318c0fd6ec1c56424aa9b')!

fn bench_ascon_aead128_encrypt() ! {
	_ := ascon.encrypt(key_ascon, ascon_nonce, ad, msg)!
}

fn bench_ascon_aead128_decrypt() ! {
	_ := ascon.decrypt(key_ascon, ascon_nonce, ad, ciphertext_aead128)!
}

// expected ciphertext for chacha20poly1305
const ciphertext_chachapoly1305 = hex.decode('67dea3c65f0f326bcf587f024140a85d9535790d9b16129210a2289eda43bb9b62746450026fc1baf466bcb8a181843cd424')!

fn bench_chacha20poly1305_encrypt() ! {
	_ := chacha20poly1305.encrypt(msg, key_cpoly, cpoly_nonce, ad)!
}

fn bench_chacha20poly1305_decrypt() ! {
	_ := chacha20poly1305.decrypt(ciphertext_chachapoly1305, key_cpoly, cpoly_nonce, ad)!
}

fn main() {
	cf := benchmark.BenchmarkDefaults{
		n: 10000
	}
	println('Encryption..')
	println('-----------')
	mut b0 := benchmark.setup(bench_ascon_aead128_encrypt, cf)!
	b0.run()
	mut b1 := benchmark.setup(bench_chacha20poly1305_encrypt, cf)!
	b1.run()

	println('')
	println('Decryption..')
	println('-----------')
	mut b2 := benchmark.setup(bench_ascon_aead128_decrypt, cf)!
	b2.run()
	mut b3 := benchmark.setup(bench_chacha20poly1305_decrypt, cf)!
	b3.run()
}
