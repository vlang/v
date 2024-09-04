fn test_comptime_pkgconfig() {
	$if $pkgconfig('mysqlclient') {
		assert true
		return
	} $else {
		assert true
		return
	}
	assert false
}
