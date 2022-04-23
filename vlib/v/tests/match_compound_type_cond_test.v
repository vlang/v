struct Foo {}

fn test_match_array_or_map_cond() {
	// array
	y1 := []u8{}
	x1 := []u8{}
	ret1 := match x1 {
		y1 { true }
		else { false }
	}
	println('ret1 is $ret1')
	assert ret1

	// fixed array
	y2 := [1, 2, 3]!
	x2 := [1, 2, 3]!
	ret2 := match x2 {
		y2 { true }
		else { false }
	}
	println('ret2 is $ret2')
	assert ret2

	// map
	y3 := {
		1: 11
		2: 22
	}
	x3 := {
		1: 11
		2: 22
	}
	ret3 := match x3 {
		y3 { true }
		else { false }
	}
	println('ret3 is $ret3')
	assert ret3

	// struct
	y4 := Foo{}
	x4 := Foo{}
	ret4 := match x4 {
		y4 { true }
		else { false }
	}
	println('ret4 is $ret4')
	assert ret4
}
