enum Color {
	red
	green
	blue
}

struct St1 {}
struct St2 {}

type St = St1 | St2

fn test_sum_type(i St) {
	match i {
		St1 { println('St1') }
		St1 { println('St1') }
		St2 { println('St2') }
	}
	match i {
		St1 { println('St1') }
		St1 { println('St1') }
		else { println('else') }
	}
}

fn test_enum(c Color) {
	match c {
		.red { println('red') }
		.green { println('green') }
		.green { println('green') }
		.blue { println('blue') }
	}
	match c {
		.red, .green { println('red green') }
		.green { println('green') }
		else { println('else') }
	}
}

fn test_int(i int) {
	match i {
		1 { println('1') }
		2 { println('2') }
		2 { println('3') }
		else { println('else') }
	}
}

fn main() {
	test_sum_type(St1{})
	test_enum(.red)
	test_int(2)
}
