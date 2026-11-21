module gg

fn test_lsystem() {
	mut l := new_lsystem('F', 25)
	l.rule('F', 'F+F')
	assert l.expand(0) == 'F'
	assert l.expand(1) == 'F+F'
	assert l.expand(2) == 'F+F+F+F'
	assert l.angle == 25
}

fn test_turtle() {
	segments := turtle_segments('F+F', Vec2{0, 0}, 0, 10)
	assert segments.len == 2
	assert segments[0].from.x == 0 && segments[0].to.x == 10
	assert segments[1].from.x == 10
	pushed := turtle_segments('F[+F]F', Vec2{0, 0}, 0, 1)
	assert pushed.len == 3
	wide := turtle_segments_with_turn('F+F', Vec2{0, 0}, 0, 10, 90)
	assert wide.len == 2
	assert wide[1].to.y == 10
}

fn test_voronoi() {
	sites := [Vec2{0, 0}, Vec2{10, 10}]
	assert nearest_site(1, 1, sites) == 0
	assert nearest_site(9, 9, sites) == 1
	assert nearest_site(1, 1, []) == -1
	cells := voronoi_cells(4, 4, [Vec2{0, 0}, Vec2{3, 3}])
	assert cells.len == 16
	assert cells[0] == 0
	assert cells[15] == 1
}

fn test_maze() {
	m := generate_maze(11, 11, 7)
	assert m.len == 121
	assert !m[1 * 11 + 1]
	assert m[0]
	mut open := 0
	for w in m {
		if !w {
			open++
		}
	}
	assert open > 20
}

fn test_cellular() {
	cells := [false, false, false, false, true, false, false, false, false]
	born := cellular_step(cells, 3, 3, 1, 1)
	assert born[0] && born[1] && born[2]
	assert !born[4]
}
