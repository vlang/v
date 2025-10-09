// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This file contains benchmarking code for standard AEAD_CHACHA20_POLY1305 encryption
// and decryption compared to AEAD_CHACHA20_POLY1305 with PSIV construct.
//
// This output on my test.
// Standard ChaCha20Poly1305 AEAD and PSIV construct output performance comparison
// ===============================================================================
// Iterations per test: 1000
// ------------------------------------------------------------------------------------------------------
//   Data Size |        Std |       PSIV | Enc (std/psiv) ||        Std |         PSIV | Dec (std/psiv)|
// ------------------------------------------------------------------------------------------------------
//         6 B |    16.00ms |    19.00ms |          0.85x ||    16.00ms |      19.00ms |         0.81x |
//         8 B |    15.00ms |    19.00ms |          0.81x ||    16.00ms |      18.00ms |         0.87x |
//        12 B |    15.00ms |    20.00ms |          0.76x ||    16.00ms |      20.00ms |         0.77x |
//        16 B |    15.00ms |    21.00ms |          0.72x ||    15.00ms |      19.00ms |         0.80x |
//        64 B |    21.00ms |    25.00ms |          0.82x ||    21.00ms |      26.00ms |         0.80x |
//        75 B |    28.00ms |    35.00ms |          0.81x ||    34.00ms |      44.00ms |         0.77x |
//       256 B |    55.00ms |    59.00ms |          0.94x ||    50.00ms |      63.00ms |         0.80x |
//      1028 B |   174.00ms |   242.00ms |          0.72x ||   178.00ms |     227.00ms |         0.78x |
//      2049 B |   361.00ms |   522.00ms |          0.69x ||   399.00ms |     558.00ms |         0.71x |
// ------------------------------------------------------------------------------------------------------
//       Total |   703.00ms |   965.00ms |          0.73x ||   748.00ms |        1.00s |          0.75x|
// ------------------------------------------------------------------------------------------------------
//
// Per-operation averages:
//   Standard ChaCha20Poly1305 encrypt:        78209 ns per hash
//   ChaCha20Poly1305 PSIV encrypt:           107325 ns per hash
//
//   Standard ChaCha20Poly1305 decrypt:        83151 ns per hash
//   ChaCha20Poly1305 PSIV decrypt:           111155 ns per hash
module main

import rand
import time
import x.crypto.chacha20poly1305

const benchmark_iterations = 1000

const test_data_sizes = [6, 8, 12, 16, 64, 75, 256, 1028, 2049]

// standard AEAD_CHACHA20_POLY1305 encryption
fn benchmark_aead_std_encrypt(msg []u8, key []u8, nonce []u8, ad []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := chacha20poly1305.encrypt(msg, key, nonce, ad) or { panic(err) }
	}
	return time.since(start)
}

// standard AEAD_CHACHA20_POLY1305 decryption
fn benchmark_aead_std_decrypt(data []u8, key []u8, nonce []u8, ad []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := chacha20poly1305.decrypt(data, key, nonce, ad) or { panic(err) }
	}
	return time.since(start)
}

// psiv AEAD_CHACHA20_POLY1305 encryption
fn benchmark_aead_psiv_encrypt(msg []u8, key []u8, nonce []u8, ad []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := chacha20poly1305.psiv_encrypt(msg, key, nonce, ad) or { panic(err) }
	}
	return time.since(start)
}

// psiv AEAD_CHACHA20_POLY1305 decryption
fn benchmark_aead_psiv_decrypt(data []u8, key []u8, nonce []u8, ad []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := chacha20poly1305.psiv_decrypt(data, key, nonce, ad) or { panic(err) }
	}
	return time.since(start)
}

fn format_duration(d time.Duration) string {
	if d.microseconds() < 1000 {
		return '${d.microseconds():6}μs'
	} else if d.milliseconds() < 1000 {
		return '${f64(d.milliseconds()):6.2f}ms'
	} else {
		return '${f64(d.seconds()):6.2f}s'
	}
}

const data_title = 'Data Size'
const aead_std_enc = 'Std'
const aead_psiv_enc = 'PSIV'
const aead_std_dec = 'Std'
const aead_psiv_dec = 'PSIV'
const ratio_std_psiv_enc_title = 'Enc (std/psiv)'
const ratio_std_psiv_dec_title = 'Dec (std/psiv)'

fn main() {
	println('Standard ChaCha20Poly1305 AEAD and PSIV construct output performance comparison')
	println('===============================================================================')
	println('Iterations per test: ${benchmark_iterations}')

	println('${'-'.repeat(102)}')
	println('${data_title:12} | ${aead_std_enc:10} | ${aead_psiv_enc:10} | ${ratio_std_psiv_enc_title:12} || ${aead_std_dec:10} | ${aead_psiv_dec:12} | ${ratio_std_psiv_dec_title:12}|')
	println('${'-'.repeat(102)}')

	mut total_std_encrypt := time.Duration(0)
	mut total_std_decrypt := time.Duration(0)
	mut total_psiv_encrypt := time.Duration(0)
	mut total_psiv_decrypt := time.Duration(0)

	key := rand.bytes(32)!
	nonce := rand.bytes(12)!
	for size in test_data_sizes {
		ad := rand.bytes(size)!
		test_msg := rand.bytes(size)!

		// Warm up
		out0 := chacha20poly1305.encrypt(test_msg, key, nonce, ad)!
		_ := chacha20poly1305.decrypt(out0, key, nonce, ad)!

		out1 := chacha20poly1305.psiv_encrypt(test_msg, key, nonce, ad)!
		_ := chacha20poly1305.psiv_decrypt(out1, key, nonce, ad)!

		// Benchmark Standard AEAD_CHACHA20_POLY1305 encryption
		std_encrypt_time := benchmark_aead_std_encrypt(test_msg, key, nonce, ad, benchmark_iterations)

		// Benchmark Standard AEAD_CHACHA20_POLY1305 decryption
		std_decrypt_time := benchmark_aead_std_decrypt(out0, key, nonce, ad, benchmark_iterations)

		// Benchmark AEAD_CHACHA20_POLY1305 PSIV encryption
		psiv_encrypt_time := benchmark_aead_psiv_encrypt(test_msg, key, nonce, ad, benchmark_iterations)

		// Benchmark AEAD_CHACHA20_POLY1305 PSIV decryption
		psiv_decrypt_time := benchmark_aead_psiv_decrypt(out1, key, nonce, ad, benchmark_iterations)

		// Calculate ratio Standard/PSIV encryption
		ratio_std_psiv_encrypt := f64(std_encrypt_time.nanoseconds()) / f64(psiv_encrypt_time.nanoseconds())

		// Calculate ratio Standard/PSIV decryption
		ratio_std_psiv_decrypt := f64(std_decrypt_time.nanoseconds()) / f64(psiv_decrypt_time.nanoseconds())

		stdencrypt_str := format_duration(std_encrypt_time)
		stddecrypt_str := format_duration(std_decrypt_time)
		psivencrypt_str := format_duration(psiv_encrypt_time)
		psivdecrypt_str := format_duration(psiv_decrypt_time)

		ratio_std_psiv_encrypt_str := '${ratio_std_psiv_encrypt:6.2f}x'
		ratio_std_psiv_decrypt_str := '${ratio_std_psiv_decrypt:6.2f}x'

		println('${size:10} B | ${stdencrypt_str:10} | ${psivencrypt_str:10} | ${ratio_std_psiv_encrypt_str:14} || ${stddecrypt_str:10} |  ${psivdecrypt_str:11} |  ${ratio_std_psiv_decrypt_str:12} |')

		total_std_encrypt += std_encrypt_time
		total_std_decrypt += std_decrypt_time
		total_psiv_encrypt += psiv_encrypt_time
		total_psiv_decrypt += psiv_decrypt_time
	}

	println('${'-'.repeat(102)}')

	// Overall performance comparison
	overall_std_psiv_encrypt_ratio := f64(total_std_encrypt.nanoseconds()) / f64(total_psiv_encrypt.nanoseconds())
	overall_std_psiv_decrypt_ratio := f64(total_std_decrypt.nanoseconds()) / f64(total_psiv_decrypt.nanoseconds())

	total_title := 'Total'
	println('${total_title:12} | ${format_duration(total_std_encrypt):10} | ${format_duration(total_psiv_encrypt):10} | ${overall_std_psiv_encrypt_ratio:13.2f}x || ${format_duration(total_std_decrypt):10} | ${format_duration(total_psiv_decrypt):12} |  ${overall_std_psiv_decrypt_ratio:12.2f}x|')
	println('${'-'.repeat(102)}')

	println('')
	println('Per-operation averages:')
	avg_std_encrypt := total_std_encrypt.nanoseconds() / (benchmark_iterations * test_data_sizes.len)
	avg_std_decrypt := total_std_decrypt.nanoseconds() / (benchmark_iterations * test_data_sizes.len)
	avg_psiv_encrypt := total_psiv_encrypt.nanoseconds() / (benchmark_iterations * test_data_sizes.len)
	avg_psiv_decrypt := total_psiv_decrypt.nanoseconds() / (benchmark_iterations * test_data_sizes.len)

	println('  Standard ChaCha20Poly1305 encrypt:\t ${avg_std_encrypt:8} ns per hash')
	println('  ChaCha20Poly1305 PSIV encrypt:\t ${avg_psiv_encrypt:8} ns per hash')
	println('')
	println('  Standard ChaCha20Poly1305 decrypt:\t ${avg_std_decrypt:8} ns per hash')
	println('  ChaCha20Poly1305 PSIV decrypt:\t ${avg_psiv_decrypt:8} ns per hash')
	println('')
}
