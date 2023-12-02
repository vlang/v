fn opt(arg ?int) ?int {
	return arg
}

fn test_main() {
	_, a, b, c, d := match 0 {
		0 {
			a := ?bool(true)
			b := ?bool(none)
			3, a, b, opt(none), opt(5)
		}
		else {
			3, ?bool(false), ?bool(false), ?int(false), ?int(false)
		}
	}
	assert a? == true
	assert b == none
	assert c == none
	assert d? == 5
}
