// main runs the v3 tests entry point.
fn main() {
	x := 10
	if x > 5 {
		println('big')
	} else {
		println('small')
	}

	mut sum := 0
	for i := 0; i < 5; i++ {
		sum = sum + i
	}
	println(sum)
}
