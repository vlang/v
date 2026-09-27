module main

import os
import v.driver

fn main() {
	mut args := ['-b', 'wasm']
	args << os.args[1..]
	driver.run(args)
}
