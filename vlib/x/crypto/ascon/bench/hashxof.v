// Ascon-Hash256 (and Ascon-XOF128) benchmark compared to builtin
// crypto.sha256 (for sum256) and sha3.shake256 (for xof outputing 256-bits)
//
// This benchmark code was adapted from argon2 benchmark by @fleximus, the creator argon2 module.
// Credit tributed to @fleximus
// See https://gist.github.com/fleximus/db5b867a9a37da46340db61bdac6e696
//
// Output
// ======
// Sum and Xof 256-bits output performance comparison
// ============================================================
// Iterations per test: 10000
// --------------------------------------------------------------------------------------------------
//   Data Size |   Ascon256 |     Sha256 |    Ratio 256 || AsconXof128 |   Shake256 |  Ratio (Xof) |
// --------------------------------------------------------------------------------------------------
//         4 B |    24.00ms |    33.00ms |        0.73x ||     24.00ms |   208.00ms |        0.12x |
//         6 B |    23.00ms |    53.00ms |        0.45x ||     25.00ms |   287.00ms |        0.08x |
//         8 B |    35.00ms |    37.00ms |        0.95x ||     26.00ms |   202.00ms |        0.18x |
//        16 B |    30.00ms |    37.00ms |        0.83x ||     30.00ms |   205.00ms |        0.15x |
//        64 B |    55.00ms |    61.00ms |        0.89x ||     53.00ms |   241.00ms |        0.23x |
//        75 B |    61.00ms |    57.00ms |        1.07x ||     58.00ms |   182.00ms |        0.34x |
//       256 B |   154.00ms |   123.00ms |        1.25x ||    144.00ms |   398.00ms |        0.39x |
//       512 B |   273.00ms |   216.00ms |        1.26x ||    265.00ms |   779.00ms |        0.35x |
//      1025 B |   610.00ms |   401.00ms |        1.52x ||    509.00ms |      1.37s |        0.45x |
// --------------------------------------------------------------------------------------------------
//       Total |      1.27s |      1.02s |        1.24x ||      1.14s |      3.87s |         0.294x|
// --------------------------------------------------------------------------------------------------
//
// Per-operation averages:
//   Ascon256:         14108 ns per hash
//   Sha256:           11360 ns per hash
//   AsconXof128:      12648 ns per hash
//   Shake256:         43036 ns per hash
//
module main

import time
import crypto.sha3
import crypto.sha256
import x.crypto.ascon

const benchmark_iterations = 10000

// We include more small size because, Ascon-Hash256 working with more smaller block size.
const test_data_sizes = [
	4, // below Ascon-Hash256 block size
	6, // Still below Ascon-Hash256 block size
	8, // align with Ascon-Hash256 block size
	16, // Small data
	64, // Medium data
	75, // above 64-bytes block
	256, // Large data
	512,
	1025,
]

fn generate_test_data(size int) []u8 {
	mut data := []u8{len: size}
	for i in 0 .. size {
		data[i] = u8(i % 256)
	}
	return data
}

fn benchmark_ascon_sha256(data []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := ascon.sum256(data)
	}
	return time.since(start)
}

fn benchmark_sha256_sum256(data []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := sha256.sum256(data)
	}
	return time.since(start)
}

// for eXtendable output functions (XOF)
fn benchmark_ascon_xof128_32(data []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := ascon.xof128(data, 32) or { panic(err) }
	}
	return time.since(start)
}

fn benchmark_sha3_shake256(data []u8, iterations int) time.Duration {
	start := time.now()
	for _ in 0 .. iterations {
		_ := sha3.shake256(data, 32)
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
const ascon_sum256_title = 'Ascon256'
const sha256_title = 'Sha256'
const ascon_xof128_title = 'AsconXof128'
const sha3_shake256_title = 'Shake256'
const ratio_ascon256_w_sha256 = 'Ratio 256'
const ratio_asconxof128_w_shake256 = 'Ratio (Xof)'

fn main() {
	println('')
	println('Sum and Xof 256-bits output performance comparison')
	println('============================================================')
	println('Iterations per test: ${benchmark_iterations}')

	println('${'-'.repeat(98)}')
	println('${data_title:12} | ${ascon_sum256_title:10} | ${sha256_title:10} | ${ratio_ascon256_w_sha256:12} || ${ascon_xof128_title:10} | ${sha3_shake256_title:10} | ${ratio_asconxof128_w_shake256:12} |')
	println('${'-'.repeat(98)}')

	mut total_ascon256 := time.Duration(0)
	mut total_sha256 := time.Duration(0)
	mut total_shake256 := time.Duration(0)
	mut total_asconxof128 := time.Duration(0)

	for size in test_data_sizes {
		test_data := generate_test_data(size)

		// Warm up
		_ := ascon.sum256(test_data)
		_ := sha256.sum256(test_data)

		_ := ascon.xof128(test_data, 32)!
		_ := sha3.shake256(test_data, 32)

		// Benchmark Ascon-HASH256
		ascon256_time := benchmark_ascon_sha256(test_data, benchmark_iterations)

		// Benchmark Sha256 implementation
		sha256_time := benchmark_sha256_sum256(test_data, benchmark_iterations)

		// Benchmark Sha3 shake256 implementation
		shake256_time := benchmark_sha3_shake256(test_data, benchmark_iterations)

		// Benchmark AsconXof128 256-bits output
		asconxof128_time := benchmark_ascon_xof128_32(test_data, benchmark_iterations)

		// Calculate ratio ascon256 / sha256
		ratio_ascon256_sha256 := f64(ascon256_time.nanoseconds()) / f64(sha256_time.nanoseconds())

		// Calculate ratio asconxof128 / shake256
		ratio_asconxof128_shake256 := f64(asconxof128_time.nanoseconds()) / f64(shake256_time.nanoseconds())

		ascon256_str := format_duration(ascon256_time)
		sha256_str := format_duration(sha256_time)
		asconxof128_str := format_duration(asconxof128_time)
		shake256_str := format_duration(shake256_time)

		ratio_ascon256_sha256_str := '${ratio_ascon256_sha256:6.2f}x'
		ratio_asconxof128_shake256_str := '${ratio_asconxof128_shake256:6.2f}x'

		println('${size:10} B | ${ascon256_str:10} | ${sha256_str:10} | ${ratio_ascon256_sha256_str:12} || ${asconxof128_str:11} | ${shake256_str:10} | ${ratio_asconxof128_shake256_str:12} |')

		total_ascon256 += ascon256_time
		total_sha256 += sha256_time

		total_asconxof128 += asconxof128_time
		total_shake256 += shake256_time
	}

	println('${'-'.repeat(98)}')

	// Overall performance comparison
	overall_ascon256_w_sha256_ratio := f64(total_ascon256.nanoseconds()) / f64(total_sha256.nanoseconds())
	overall_asconxof128_w_shake256_ratio := f64(total_asconxof128.nanoseconds()) / f64(total_shake256.nanoseconds())
	total_title := 'Total'
	println('${total_title:12} | ${format_duration(total_ascon256):10} | ${format_duration(total_sha256):10} | ${overall_ascon256_w_sha256_ratio:11.2f}x || ${format_duration(total_asconxof128):11} | ${format_duration(total_shake256):10} | ${overall_asconxof128_w_shake256_ratio:12.2f}x|')
	println('${'-'.repeat(98)}')

	println('')
	println('Per-operation averages:')
	avg_ascon256 := total_ascon256.nanoseconds() / (benchmark_iterations * test_data_sizes.len)
	avg_sha256 := total_sha256.nanoseconds() / (benchmark_iterations * test_data_sizes.len)
	avg_shake256 := total_shake256.nanoseconds() / (benchmark_iterations * test_data_sizes.len)
	avg_asconxof128 := total_asconxof128.nanoseconds() / (benchmark_iterations * test_data_sizes.len)

	println('  Ascon256:\t ${avg_ascon256:8} ns per hash')
	println('  Sha256:\t ${avg_sha256:8} ns per hash')
	println('  AsconXof128:\t ${avg_asconxof128:8} ns per hash')
	println('  Shake256:\t ${avg_shake256:8} ns per hash')
	println('')
}
