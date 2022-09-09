[has_globals]
module main

import time

__global seconds_per_minute = 'sixty'

fn test_const_and_globals_with_the_same_name_in_different_modules_do_not_conflict() {
	assert seconds_per_minute == 'sixty'
	assert time.seconds_per_minute == 60
}
