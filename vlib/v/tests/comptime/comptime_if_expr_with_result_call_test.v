import os

fn test_comptime_if_expr_with_result_call() {
	config_dir := $if windows {
		os.home_dir()
	} $else {
		os.config_dir() or { os.home_dir() }
	}
	println(config_dir)
	assert true
}
