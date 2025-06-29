@[unsafe]
fn g() {
	mut static levels := 0
	levels++
	defer { levels-- }
}

fn f(depth int) {
	if depth == 0 {
		return
	}
	unsafe {
		mut static levels := 0
		levels++
		defer { levels-- }
		if depth == 3 {
			assert levels == 1
		}
		if depth == 2 {
			assert levels == 2
		}
		if depth == 1 {
			assert levels == 3
		}
		println('levels: ${levels} | depth: ${depth}')
	}
	f(depth - 1)
}

fn test_main() {
	f(3)
	f(3)

	unsafe { g() }
}
