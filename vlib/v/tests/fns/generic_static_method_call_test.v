struct Parser {
mut:
	data []u8
}

fn (mut p Parser) read_element[T]() !T {
	t := T.parse(mut p)!
	return t
}

struct TestA {
	data []u8
}

fn TestA.parse(mut p Parser) !TestA {
	return TestA{
		data: p.data
	}
}

struct TestB {
	st string
}

fn TestB.parse(mut p Parser) !TestB {
	return TestB{
		st: p.data.hex()
	}
}

fn test_main() {
	data := []u8{len: 5, init: u8(5)}
	mut p := Parser{
		data: data
	}

	ta := p.read_element[TestA]()!
	dump(ta)
	assert ta == TestA{
		data: [u8(5), 5, 5, 5, 5]
	}

	tb := p.read_element[TestB]()!
	dump(tb)
	assert tb == TestB{
		st: '0505050505'
	}
}
