const msg = $if windows {
	'windows, eh?'
} $else {
	'ok, then'
}

fn test_comptime_if_in_const_decl() {
	println(msg)
	assert msg.len > 0
}
