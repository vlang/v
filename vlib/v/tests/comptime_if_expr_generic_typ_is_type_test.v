module main

fn write<T>(out T) string {
	$if T.typ is bool {
		println('FOO')
		return 'FOO'
	} $else $if T.typ !is bool {
		println('BAR')
		return 'BAR'
	}
	return 'EMPTY'
}

fn test_comptime_if_expr_generic_typ_is_type() {
	mut val := false
	ret := write<bool>(val)
	assert ret == 'FOO'
}
