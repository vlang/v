fn test_name_lpar() {
	mut v := 2
	// check ParExpr is parsed, not CallExpr
	mut p := &v
	(*p)++
	_ = v
	(*p)++
	assert v == 4
}
