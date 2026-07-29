module main

import os
import v3.driver

$if gcboehm ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_full ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_incr ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_opt ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if gcboehm_leak ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

$if vgc ? {
	$compile_error('v3 must be built without a garbage collector; use `-gc none` or `-prealloc`')
}

fn main() {
	driver.run(os.args[1..])
}
