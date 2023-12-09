type JBoolean = bool

fn test_alias_not_op() {
	a := JBoolean(false)
	b := !a
	assert b == true
}
