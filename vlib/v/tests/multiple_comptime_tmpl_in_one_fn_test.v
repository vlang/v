fn test_multi_comptime_tmpl_in_one_fn() {
	a := 'A'
	at := $tmpl('tmpl/a.txt')
	println(at)
	assert at.contains('A')

	b := 'B'
	bt := $tmpl('tmpl/b.txt')
	println(bt)
	assert bt.contains('B')
}
