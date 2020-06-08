fn multi_return() (int, int, string) {
	return 1, 2, '3'
}

fn main() {
	_, a, b := multi_return()
	c, _, d := multi_return()
	e, f, _ := multi_return()
	_, _, g := multi_return()
	_, h, _ := multi_return()
	i, _, _ := multi_return()
	_, _, _ := multi_return()

	JS.console.log(a, b, c, d, e, f, g, h, i)

	$if js {
		println('JS')
	}
	$if !js {
		println('!JS')
	}
}
