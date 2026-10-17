interface SmartcastImageData {
	frame_count() int
}

@[heap]
struct SmartcastAnimatedData {
mut:
	frames  int
	current int
}

fn (d &SmartcastAnimatedData) frame_count() int {
	return d.frames
}

fn (mut d SmartcastAnimatedData) restart() {
	d.current = 0
}

fn (mut d SmartcastAnimatedData) advance() {
	d.current++
}

@[heap]
struct SmartcastStillData {}

fn (d &SmartcastStillData) frame_count() int {
	return 1
}

// `&Impl(data)` inside `if data is Impl` reads the object of the interface value.
fn restart_data(data SmartcastImageData) {
	if data is SmartcastAnimatedData {
		mut animated := unsafe { &SmartcastAnimatedData(data) }
		animated.restart()
	}
}

fn advance_data(data SmartcastImageData) bool {
	if data is SmartcastAnimatedData {
		mut animated := unsafe { &SmartcastAnimatedData(data) }
		animated.advance()
		return true
	}
	return false
}

fn test_pointer_cast_of_a_smartcast_interface_value() {
	animated := &SmartcastAnimatedData{
		frames: 3
	}
	data := SmartcastImageData(animated)
	assert advance_data(data)
	assert advance_data(data)
	assert animated.current == 2
	restart_data(data)
	assert animated.current == 0
	assert advance_data(data)
	assert animated.current == 1
	assert !advance_data(SmartcastImageData(&SmartcastStillData{}))
}
