// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module game

import math
import gg

// LSystem is a simple string rewriting system. Symbols without a rule are
// copied unchanged, so constants like '+' and '[' need no explicit rule.
pub struct LSystem {
pub mut:
	axiom string
	rules map[string]string
	angle f32 = 25
}

// new_lsystem returns a system starting from axiom.
pub fn new_lsystem(axiom string, angle f32) LSystem {
	return LSystem{ axiom: axiom, angle: angle, rules: map[string]string{} }
}

// rule defines the replacement string for one symbol.
pub fn (mut l LSystem) rule(symbol string, replacement string) {
	l.rules[symbol] = replacement
}

// expand applies the rewrite rules iterations times.
pub fn (l &LSystem) expand(iterations int) string {
	mut current := l.axiom
	for _ in 0 .. iterations {
		mut next := ''
		for i in 0 .. current.len {
			symbol := current[i..i + 1]
			next += l.rules[symbol] or { symbol }
		}
		current = next
	}
	return current
}

// TurtleSegment is one drawn line produced by interpreting an L-system path.
pub struct TurtleSegment {
pub mut:
	from gg.Vec2
	to   gg.Vec2
}

// turtle_segments interprets F/f as forward moves, + and - as turns of 25
// degrees, and brackets as a position/heading stack. Other symbols are ignored.
pub fn turtle_segments(path string, start gg.Vec2, heading f32, step f32) []TurtleSegment {
	return turtle_segments_with_turn(path, start, heading, step, 25)
}

// turtle_segments_with_turn is turtle_segments with an explicit turn angle
// in degrees, normally taken from LSystem.angle.
pub fn turtle_segments_with_turn(path string, start gg.Vec2, heading f32, step f32, turn f32) []TurtleSegment {
	mut segments := []TurtleSegment{}
	mut pos := start
	mut angle := heading
	mut stack := []gg.Vec2{}
	mut angles := []f32{}
	for i in 0 .. path.len {
		symbol := path[i]
		match symbol {
			`F`, `G` {
				rad := f64(angle) * math.pi / 180
				next := gg.Vec2{pos.x + f32(math.cos(rad)) * step, pos.y + f32(math.sin(rad)) * step}
				segments << TurtleSegment{pos, next}
				pos = next
			}
			`f` {
				rad := f64(angle) * math.pi / 180
				pos = gg.Vec2{pos.x + f32(math.cos(rad)) * step, pos.y + f32(math.sin(rad)) * step}
			}
			`+` {
				angle += turn
			}
			`-` {
				angle -= turn
			}
			`[` {
				stack << pos
				angles << angle
			}
			`]` {
				if stack.len > 0 {
					pos = stack[stack.len - 1]
					stack = stack[..stack.len - 1]
					angle = angles[angles.len - 1]
					angles = angles[..angles.len - 1]
				}
			}
			else {}
		}
	}
	return segments
}

// nearest_site returns the index of the site closest to (x, y).
pub fn nearest_site(x f32, y f32, sites []gg.Vec2) int {
	if sites.len == 0 {
		return -1
	}
	mut best := 0
	mut best_d := f32(0)
	for i, s in sites {
		dx := s.x - x
		dy := s.y - y
		d := dx * dx + dy * dy
		if i == 0 || d < best_d {
			best_d = d
			best = i
		}
	}
	return best
}

// voronoi_cells returns one site index per pixel in a width by height grid.
pub fn voronoi_cells(width int, height int, sites []gg.Vec2) []int {
	mut out := []int{len: width * height}
	for y in 0 .. height {
		for x in 0 .. width {
			out[y * width + x] = nearest_site(f32(x), f32(y), sites)
		}
	}
	return out
}

fn proc_next(seed u32) u32 {
	mut value := seed
	value ^= value << 13
	value ^= value >> 17
	value ^= value << 5
	return value
}

// generate_maze builds a randomized depth-first maze on a grid. Odd-sized
// dimensions work best: odd cells are rooms and even cells are walls.
pub fn generate_maze(cols int, rows int, seed u32) []bool {
	mut walls := []bool{len: cols * rows, init: true}
	if cols < 3 || rows < 3 {
		return walls
	}
	mut rng := seed
	mut stack := [Cell{1, 1}]
	walls[1 * cols + 1] = false
	dirs := [Cell{0, -2}, Cell{2, 0}, Cell{0, 2}, Cell{-2, 0}]
	for stack.len > 0 {
		cur := stack[stack.len - 1]
		mut candidates := []Cell{}
		for d in dirs {
			nx := cur.x + d.x
			ny := cur.y + d.y
			if nx > 0 && ny > 0 && nx < cols - 1 && ny < rows - 1 && walls[ny * cols + nx] {
				candidates << Cell{nx, ny}
			}
		}
		if candidates.len == 0 {
			stack = stack[..stack.len - 1]
			continue
		}
		rng = proc_next(rng)
		next := candidates[int(rng % u32(candidates.len))]
		walls[(cur.y + next.y) / 2 * cols + (cur.x + next.x) / 2] = false
		walls[next.y * cols + next.x] = false
		stack << next
	}
	return walls
}

// cellular_step applies a two-state cellular automaton rule: birth counts
// are the neighbor counts that turn a dead cell alive, survive counts apply
// to live cells. Eight neighbors are counted with clamped edges.
pub fn cellular_step(cells []bool, cols int, rows int, birth int, survive int) []bool {
	mut out := []bool{len: cells.len}
	for y in 0 .. rows {
		for x in 0 .. cols {
			mut neighbors := 0
			for dy in -1 .. 2 {
				for dx in -1 .. 2 {
					if dx == 0 && dy == 0 {
						continue
					}
					nx := x + dx
					ny := y + dy
					if nx < 0 || ny < 0 || nx >= cols || ny >= rows {
						continue
					}
					if cells[ny * cols + nx] {
						neighbors++
					}
				}
			}
			alive := cells[y * cols + x]
			out[y * cols + x] = if alive { neighbors == survive } else { neighbors == birth }
		}
	}
	return out
}
