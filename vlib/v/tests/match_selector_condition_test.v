struct Params {
	bytes  []int
	chars  []int
	fields []int
}

fn choose(p Params) int {
	return match true {
		p.bytes.len > 0 { 1 }
		p.chars.len > 0 { 2 }
		p.fields.len > 0 { 3 }
		else { 0 }
	}
}

fn test_distinct_conditions() {
	assert choose(Params{ bytes: [1] }) == 1
	assert choose(Params{ chars: [1] }) == 2
	assert choose(Params{ fields: [1] }) == 3
}
