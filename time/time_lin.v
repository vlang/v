module time

// in ms
fn ticks() double {
	return double(0)
}

fn sleep(seconds int) {
	C.sleep(seconds)
}

fn sleep_ms(seconds int) {
	C.usleep(seconds * 1000)
}

