import time
import benchmark

fn test_measure() {
	mut b := benchmark.start()
	time.sleep(200 * time.millisecond)
	x := b.measure('sleeping') // returns microseconds
	flush_stdout()
	assert x > 150_000
	// assert x < 800_000 // this can be much longer on a slow CI runner
}

fn test_record_measure() {
	mut b := benchmark.start()
	println('step 1')
	flush_stdout()
	time.sleep(100 * time.millisecond)
	x := b.record_measure('sleeping 1')
	assert x > 50_000
	// assert x < 200_000
	flush_stdout()
	//
	println('step 2')
	flush_stdout()
	time.sleep(150 * time.millisecond)
	y := b.record_measure('sleeping 2')
	assert y > 100_000
	// assert y < 200_000
	flush_stdout()
	//
	res := b.all_recorded_measures()
	println('All recorded measurements:')
	println(res)
	flush_stdout()
	assert res.contains('ms in sleeping 1')
	assert res.contains('ms in sleeping 1')
	assert res.contains('SPENT')
}
