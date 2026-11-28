module game

fn test_pathfind_straight() {
	p := new_grid_pathfinder(5, 1)
	path := p.find_path(Cell{0, 0}, Cell{4, 0})
	assert path.len == 5
	assert path[0].x == 0 && path[4].x == 4
}

fn test_pathfind_around_wall() {
	mut p := new_grid_pathfinder(5, 5)
	for row in 0 .. 4 {
		p.set_walkable(2, row, false)
	}
	path := p.find_path(Cell{0, 0}, Cell{4, 0})
	assert path.len > 5
	for c in path {
		assert p.is_walkable(c.x, c.y)
	}
	assert p.is_walkable(2, 4)
}

fn test_pathfind_blocked() {
	mut p := new_grid_pathfinder(3, 3)
	p.set_walkable(1, 1, false)
	assert p.find_path(Cell{1, 1}, Cell{2, 2}).len == 0
	for x in 0 .. 3 {
		p.set_walkable(x, 1, false)
	}
	assert p.find_path(Cell{0, 0}, Cell{2, 2}).len == 0
	assert !p.in_bounds(-1, 0)
}
