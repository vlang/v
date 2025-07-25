interface Person {
	identify() int
}

struct Boy {}

fn (self Boy) identify() int {
	return 0
}

struct Girl {}

fn (self Girl) identify() int {
	return 1
}

fn test_main() {
	mut ppl := []Person{}
	ppl << &Girl{}
	ppl << Boy{}
	ppl[0].identify()
	ppl.sort(a.identify() < b.identify())
	assert ppl == [Person(Boy{}), Girl{}]
	println(ppl)
}
