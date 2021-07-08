fn test_comptime_pkgconfig() {
	$if [unhandled expr type unknown v.ast.Expr].$pkgconfig(mysqlclient) {
		assert true
		return
	} $else {
		assert true
		return
	}
	assert false
}
