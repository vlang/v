type Attribute = NextHop | Origin

type Origin = u8
type NextHop = []u8

fn test_array_of_sumtype_append_alias() {
	mut attrs := []Attribute{cap: 100}
	bytes := []u8{len: 4}

	attrs << NextHop(bytes)
	attrs << Origin(1)

	for attr in attrs {
		if attr is Origin {
			println(attr)
			continue
		}
		if attr is NextHop {
			println(attr)
			continue
		}
		panic('oh no !')
	}
	assert attrs.len == 2
	assert attrs[0] == Attribute(NextHop(bytes))
	assert attrs[1] == Attribute(Origin(1))
}
