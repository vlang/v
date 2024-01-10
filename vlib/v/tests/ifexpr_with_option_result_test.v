fn comptime_ret_result() !string {
	return $if debug {
		'debug'
	} $else {
		'not debug'
	}
}

fn test_main() {
	assert comptime_ret_result()? == 'not debug'
}
