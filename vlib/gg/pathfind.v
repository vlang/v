// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// Cell identifies a grid coordinate used by GridPathfinder.
pub struct Cell {
pub mut:
	x int
	y int
}

// GridPathfinder performs four-directional A* on a walkable grid. It uses a
// Manhattan heuristic, so diagonal movement is never assumed.
pub struct GridPathfinder {
pub mut:
	width    int
	height   int
	walkable []bool
}

// grid_from_tilemap builds a pathfinder where nonzero tiles are walls.
pub fn grid_from_tilemap(m TileMap) GridPathfinder {
	mut p := new_grid_pathfinder(m.cols, m.rows)
	for row in 0 .. m.rows {
		for col in 0 .. m.cols {
			p.set_walkable(col, row, m.at(col, row) <= 0)
		}
	}
	return p
}

// new_grid_pathfinder returns a fully walkable grid of the given size.
pub fn new_grid_pathfinder(width int, height int) GridPathfinder {
	return GridPathfinder{
		width:    width
		height:   height
		walkable: []bool{len: width * height, init: true}
	}
}

fn path_index(p &GridPathfinder, x int, y int) int {
	return y * p.width + x
}

// in_bounds reports whether a cell lies inside the grid.
pub fn (p &GridPathfinder) in_bounds(x int, y int) bool {
	return x >= 0 && y >= 0 && x < p.width && y < p.height
}

// is_walkable reports whether a cell is inside the grid and passable.
pub fn (p &GridPathfinder) is_walkable(x int, y int) bool {
	return p.in_bounds(x, y) && p.walkable[path_index(p, x, y)]
}

// set_walkable marks a cell passable or blocked.
pub fn (mut p GridPathfinder) set_walkable(x int, y int, value bool) {
	if p.in_bounds(x, y) {
		p.walkable[path_index(p, x, y)] = value
	}
}

fn path_heuristic(a_x int, a_y int, b_x int, b_y int) int {
	dx := if a_x > b_x { a_x - b_x } else { b_x - a_x }
	dy := if a_y > b_y { a_y - b_y } else { b_y - a_y }
	return dx + dy
}

// find_path returns the cells from start to goal inclusive, or an empty slice
// when either endpoint is blocked or no route exists.
pub fn (p &GridPathfinder) find_path(start Cell, goal Cell) []Cell {
	if !p.is_walkable(start.x, start.y) || !p.is_walkable(goal.x, goal.y) {
		return []Cell{}
	}
	n := p.width * p.height
	start_idx := path_index(p, start.x, start.y)
	goal_idx := path_index(p, goal.x, goal.y)
	mut g := []f32{len: n, init: 999999.0}
	mut f := []f32{len: n, init: 999999.0}
	mut came := []int{len: n, init: -1}
	mut closed := []bool{len: n}
	mut open := [start_idx]
	g[start_idx] = 0
	f[start_idx] = f32(path_heuristic(start.x, start.y, goal.x, goal.y))

	dxs := [1, -1, 0, 0]
	dys := [0, 0, 1, -1]
	for open.len > 0 {
		mut best_k := 0
		mut best_f := f[open[0]]
		for k in 1 .. open.len {
			if f[open[k]] < best_f {
				best_f = f[open[k]]
				best_k = k
			}
		}
		current := open[best_k]
		open.delete(best_k)
		if current == goal_idx {
			mut path := []Cell{}
			mut cur := current
			for cur != -1 {
				path << Cell{cur % p.width, cur / p.width}
				if cur == start_idx {
					break
				}
				cur = came[cur]
			}
			mut reversed := []Cell{len: path.len}
			for i, c in path {
				reversed[path.len - 1 - i] = c
			}
			return reversed
		}
		closed[current] = true
		cx := current % p.width
		cy := current / p.width
		for i in 0 .. dxs.len {
			nx := cx + dxs[i]
			ny := cy + dys[i]
			if !p.is_walkable(nx, ny) {
				continue
			}
			next := path_index(p, nx, ny)
			if closed[next] {
				continue
			}
			tentative := g[current] + 1
			if tentative < g[next] {
				came[next] = current
				g[next] = tentative
				f[next] = tentative + f32(path_heuristic(nx, ny, goal.x, goal.y))
				if next !in open {
					open << next
				}
			}
		}
	}
	return []Cell{}
}
