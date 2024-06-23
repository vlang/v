const x = $d('x', true)

fn test_main() {
	a := $if x {
		1
	} $else {
		0
	}
	assert a == 1
}
