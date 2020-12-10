// The series of i?_test.v files, do test different edge cases for
// interface table generation. The differences may seem very minor
// (placement of the interface declaration, whether or not there are
// helper methods, etc), but PLEASE do NOT be tempted to merge them in
// a single _test.v file. Debugging interface code generation issues
// is *much easier* when the _test.v files are very short and focused.
struct Point {
	x i8
	y i8
	z i8
}

fn (p Point) draw() string {
	return 'Point($p.x,$p.y)'
}

fn to_string(d Drawer) string {
	x := d.draw()
	println(x)
	return x
}

interface Drawer {
	draw() string
}

fn test_to_string_can_be_called() {
	p := Point{
		x: 2
		y: 3
	}
	res := to_string(p)
	assert res == 'Point(2,3)'
}
