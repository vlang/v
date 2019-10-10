// Build this example with
// v -live message.v
module main

import os
import time

[live]
fn print_message() {
	println('Hello! Modify this message while the program is running.')
}

fn main() {
	os.clear()
	for {
		print_message()
		time.sleep_ms(500)
	}
}




