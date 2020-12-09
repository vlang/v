// The series of i?_test.v files, do test different edge cases for
// interface table generation. The differences may seem very minor
// (placement of the interface declaration, whether or not there are
// helper methods, etc), but PLEASE do NOT be tempted to merge them in
// a single _test.v file. Debugging interface code generation issues
// is *much easier* when the _test.v files are very short and focused.
struct Point {
	x int
	y int
	z int
}

fn (p Point) draw() string {
	return 'Point($p.x,$p.y)'
}

fn to_string(d Drawer) string {
	return d.draw()
}

interface Drawer {
	draw() string
}

fn test_calling_to_string() {
	p := Point{
		x: 2
		y: 3
	}
	res := to_string(p)
	println(res)
	assert res == 'Point(2,3)'
}
