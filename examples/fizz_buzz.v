for n in 1 .. 101 {
	println(match true {
		n % 15 == 0 { 'FizzBuzz' }
		n % 5 == 0 { 'Buzz' }
		n % 3 == 0 { 'Fizz' }
		else { n.str() }
	})
}
