interface Rect {
	width  u32
	height u32
}

struct Square {
	side   u32
	width  u32
	height u32
}

fn test_main() {
	squares := [Square{
		side:   5
		width:  5
		height: 5
	}]
	rects := squares.map(Rect(it))

	assert rects.str() == '[Rect(Square{
    side: 5
    width: 5
    height: 5
})]'
}

fn test_fixed_array() {
	squares := [Square{
		side:   5
		width:  5
		height: 5
	}]!
	rects := squares.map(Rect(it))
}
