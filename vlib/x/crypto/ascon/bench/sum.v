import time
import x.crypto.ascon

// Before:
// Benchmarking ascon.sum256 ...
// Average ascon.sum256 time: 8 µs
// Benchmarking ascon.sum256 ...
// Average ascon.sum256 time: 6 µs

// For xof128 (32 bytes)
// Benchmarking ascon.xof128 ...
// Average ascon.xof128 time: 7 µs
// Benchmarking ascon.xof128 ...
// Average ascon.xof128 time: 6 µs

// For cxof128 32 bytes
// Benchmarking ascon.cxof128 ...
// Average ascon.cxof128 time: 9 µs
// Benchmarking ascon.sum256 ...
// Average ascon.cxof128 time: 7 µs
//
fn main() {
	iterations := 1000
	msg := [u8(0xff)].repeat(100)

	println('Benchmarking ascon.sum256 ...')
	mut total_sum_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_ := ascon.sum256(msg)
		elapsed := sw.elapsed().microseconds()
		total_sum_time += elapsed
	}
	avg_sum_time := total_sum_time / iterations
	println('Average ascon.sum256 time: ${avg_sum_time} µs')
}
