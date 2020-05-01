import time

fn test_stopwatch_works_as_intended(){
	sw := time.new_stopwatch()
	// sample code that you want to measure:
	println('Hello world')
	time.sleep_ms(1)
	//
	println('Greeting the world took: ${sw.elapsed().nanoseconds()}ns')
	assert sw.elapsed().nanoseconds() > 0
}

fn test_stopwatch_time_between_pause_and_start_should_be_skipped_in_elapsed(){
	sw := time.new_stopwatch()
	time.sleep_ms(10) // A
	//eprintln('${sw.elapsed().milliseconds()}ms')
	assert sw.elapsed().milliseconds() >= 10
	sw.pause()
	time.sleep_ms(10)
	//eprintln('${sw.elapsed().milliseconds()}ms')
	assert sw.elapsed().milliseconds() >= 10
	assert sw.elapsed().milliseconds() <  20
	sw.start()
	time.sleep_ms(10) // B
	// Here, sw.elapsed() should be ~10ms (from A) + 10ms (from B) = 20ms
	assert sw.elapsed().milliseconds() >= 20
	assert sw.elapsed().milliseconds()	< 30
}
