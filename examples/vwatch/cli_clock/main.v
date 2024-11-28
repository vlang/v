import time
// This example demonstrates how to use `v watch` for simple CLI apps.

fn main() {
	println('Run with: `v watch run examples/vwatch/cli_clock`,')
	println('then modify timer.v in your editor.')
	println('The application will be restarted,')
	println('as soon as you save your changes.')
	println('')
	for {
		println('The time is now: ${time.now()}')
		time.sleep(1000 * time.millisecond)
	}
}
