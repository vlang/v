// Build this example with
// v -live message.v
module main

import time

[live]
fn print_message() {
	println('Hello! Modify this message while the program is running.')
}

fn main() {
	for {
		print_message()
		time.sleep_ms(500)
	}
}




