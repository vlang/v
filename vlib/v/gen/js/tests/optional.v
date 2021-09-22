module main

fn main() {
	try_propagation() or { println('captured: $err') }
}

fn try_propagation() ? {
	try_numbers() ?
}

fn try_numbers() ? {
	for x in 1 .. 10 {
		y := error_if_even(x) or { x + 1 }
		println('$x rounded to $y')
		error_if_prime(y) ?
	}
}

fn error_if_even(num int) ?int {
	if num % 2 == 0 {
		return error('number is even')
	}
	return num
}

fn error_if_prime(num int) ?int {
	for i in 2 .. num {
		if num % i == 0 {
			return error('$num is prime')
		}
	}
	return num
}
