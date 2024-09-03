enum Color {
	red  = 1 + 1   @[json: 'Red']
	blue = 10 / 2  @[json: 'Blue']
}

fn test_main() {
	$for e in Color.values {
		if e.name == 'red' {
			assert e.value == Color.red
		} else if e.name == 'blue' {
			assert e.value == Color.blue
		}
	}
}
