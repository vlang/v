import time

interface Cursor {
mut:
	blink time.StopWatch
}

struct LineCursor {
__global:
	blink time.StopWatch = time.new_stopwatch(auto_start: true)
}

fn test_cast_to_interface_with_struct_field_default() {
	c := Cursor(LineCursor{})
	time.sleep(2 * time.millisecond)
	duration := c.blink.elapsed().milliseconds()
	println(duration)
	assert duration > 0
}
