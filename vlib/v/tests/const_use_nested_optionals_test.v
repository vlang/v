module main

import os

const iterations = (os.getenv_opt('ITERATIONS') or { '5' }).int()

fn test_const_use_nested_optionals() {
	println('Number of iterations: ${iterations}')
	assert iterations == 5
}
