@[has_globals]
module main

struct Rectangle {
	x int
	y int
	w int
	h int
}

__global bricks = []Rectangle{}

fn init_bricks(mut bricks []Rectangle) {
	for i in 0 .. 5 {
		bricks << Rectangle{i, 2 * i, 3 * i, 4 * i}
	}
	dump(bricks.len)
}

fn test_main() {
	assert bricks.len == 0
	init_bricks(mut bricks)
	assert bricks.len == 5
}
