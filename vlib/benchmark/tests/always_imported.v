module main

import benchmark

struct BenchedTests {
	bench benchmark.Benchmark
}

fn (b BenchedTests) str() string {
	return 'BenchedTests{ bench: ' + ptr_str(&b.bench) + ' }'
}
////////////////////////////////////////////////////////////////
fn start_testing() BenchedTests {
	b := BenchedTests{ bench: benchmark.new_benchmark() }
	println( 'start_testing...' )
	println( b )
	return b
}

fn (b &BenchedTests) testing_step_start(stepfunc string) {
	println('BenchedTests.testing_step_start: $stepfunc')
	println(b)
}
fn (b &BenchedTests) testing_step_end(stepfunc string) {
	println('BenchedTests.testing_step_end: $stepfunc')
	println(b)
}
fn (b &BenchedTests) end_testing() {
	println('BenchedTests.end_testing')
	println(b)
}
