// The series of i?_test.v files, do test different edge cases for
// interface table generation. The differences may seem very minor
// (placement of the interface declaration, whether or not there are
// helper methods, etc), but PLEASE do NOT be tempted to merge them in
// a single _test.v file. Debugging interface code generation issues
// is *much easier* when the _test.v files are very short and focused.
interface Drawable {
	draw() string
}

struct Point {
	x int
	y int
}

fn (p Point) draw() string {
	return 'Point($p.x,$p.y)'
}

// Note: this helper function forced the compiler to generate an
// interface dispatch table. Now, it should not be needed anymore,
// but it is better to test it too, to prevent future interface regressions.
fn (x Point) tointerface() Drawable {
	return x
}

fn to_string(d Drawable) string {
	return d.draw()
}

fn test_p_draw_can_be_called() {
	p := Point{
		x: 2
		y: 3
	}
	res := p.draw()
	assert res == 'Point(2,3)'
}
