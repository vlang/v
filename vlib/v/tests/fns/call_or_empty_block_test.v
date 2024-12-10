fn foo() ! {
}

fn bar() ?int {
	return 1
}

fn test_main() {
	y := if a := bar() {
		dump(a)
		foo() or {}
		true
	} else {
		false
	}
	assert y
}
