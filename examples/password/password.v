module main

import os

fn main() {
	original_password := os.input_password('Enter your password : ')!
	repeated_password := os.input_password('Confirm password : ')!

	if original_password == repeated_password {
		println('Password confirmed! You entered: ${original_password} .')
	} else {
		println('Passwords do not match .')
	}
}
