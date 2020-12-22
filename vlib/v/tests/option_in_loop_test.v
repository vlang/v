fn opt_0_10_20(x int) ?int {
	if x < 0 || (x >= 10 && x <= 20) {
		return error('invalid')
	}
	return x
}

fn test_options_in_for_loop_break() {
	mut sum := 0
	mut nbreaks := 0
	for i := 5; i < 15; i++ {
		x := opt_0_10_20(i) or {
			nbreaks++
			break
		}
		sum += x
		// println('i: ${i:3} | sum: ${sum:3}')
	}
	assert nbreaks == 1
	assert sum == 35
}

fn test_options_in_for_loop_continue() {
	mut sum := 0
	mut ncontinue := 0
	for i := -5; i < 30; i++ {
		x := opt_0_10_20(i) or {
			ncontinue++
			continue
		}
		sum += x
		// println('i: ${i:3} | sum: ${sum:3}')
	}
	assert ncontinue == 16
	assert sum == 270
}
