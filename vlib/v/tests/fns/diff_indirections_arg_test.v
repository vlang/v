fn takes_ref(x &&&&int) {
	unsafe {
		****x = 5
	}
}

fn test_main() {
	y := 4
	takes_ref(y)
	println(y)
	assert y == 5
}
