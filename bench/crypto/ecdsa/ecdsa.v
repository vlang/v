import time
import crypto.ecdsa

fn main() {
	iterations := 1000

	println('Benchmarking key generation...')
	mut total_gen_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_, _ := ecdsa.generate_key() or { panic(err) }
		elapsed := sw.elapsed().microseconds()
		total_gen_time += elapsed
	}
	avg_gen_time := total_gen_time / iterations
	println('Average key generation time: ${avg_gen_time} µs')

	pub_key, priv_key := ecdsa.generate_key() or { panic(err) }
	message := 'Benchmark message'.bytes()

	println('Benchmarking signing...')
	mut total_sign_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_ := priv_key.sign(message) or { panic(err) }
		elapsed := sw.elapsed().microseconds()
		total_sign_time += elapsed
	}
	avg_sign_time := total_sign_time / iterations
	println('Average sign time: ${avg_sign_time} µs')

	sig := priv_key.sign(message) or { panic(err) }

	println('Benchmarking verification...')
	mut total_verify_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_ := pub_key.verify(message, sig) or { panic(err) }
		elapsed := sw.elapsed().microseconds()
		total_verify_time += elapsed
	}
	avg_verify_time := total_verify_time / iterations
	println('Average verify time: ${avg_verify_time} µs')
}
