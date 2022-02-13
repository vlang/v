module stats_import

pub fn get_stats_ok() int {
	res := 0
	#res.val = +g_test_oks

	return res
}

pub fn get_stats_fail() int {
	res := 0
	#res.val = +g_test_fails

	return res
}
