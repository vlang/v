module main

import gg
import gx
import rand
import os.asset

const header_size = 30

enum Cell {
	empty
	n1
	n2
	n3
	n4
	n5
	n6
	n7
	n8
	mine
}

@[heap]
struct Game {
mut:
	ctx           &gg.Context = unsafe { nil }
	grid          [][]Cell
	flags         [][]bool
	revealed      [][]bool
	size          int = 10 // in cells
	csize         int = 30 // in pixels
	game_over     bool
	first_click   bool = true
	mines         int  = 10
	mines_flagged int
}

@[inline]
fn (mut g Game) in_grid(cy int, cx int) bool {
	return cx >= 0 && cx < g.size && cy >= 0 && cy < g.size
}

fn (mut g Game) generate_mines(first_y int, first_x int) {
	mut mines_placed := 0
	for mines_placed < g.mines {
		x, y := rand.intn(g.size) or { 0 }, rand.intn(g.size) or { 0 }
		// avoid placing mines at the position of the first click
		if (x == first_x && y == first_y) || g.grid[y][x] == .mine {
			continue
		}
		g.grid[y][x] = .mine
		mines_placed++
	}
	for y in 0 .. g.size {
		for x in 0 .. g.size {
			if g.grid[y][x] == .mine {
				continue
			}
			mut count := 0
			for dy in -1 .. 2 {
				for dx in -1 .. 2 {
					cy, cx := y + dy, x + dx
					if g.in_grid(cy, cx) {
						if g.grid[cy][cx] == .mine {
							count++
						}
					}
				}
			}
			g.grid[y][x] = unsafe { Cell(count) }
		}
	}
}

fn (mut g Game) reveal(y int, x int) {
	if !g.in_grid(y, x) {
		return
	}
	if g.revealed[y][x] {
		return
	}
	g.revealed[y][x] = true
	if g.grid[y][x] == .mine {
		g.game_over = true
		return
	}
	if g.grid[y][x] == .empty {
		for dy in -1 .. 2 {
			for dx in -1 .. 2 {
				g.reveal(y + dy, x + dx)
			}
		}
	}
}

fn (mut g Game) restart() {
	g.grid = [][]Cell{len: g.size, init: []Cell{len: g.size}}
	g.flags = [][]bool{len: g.size, init: []bool{len: g.size}}
	g.revealed = [][]bool{len: g.size, init: []bool{len: g.size}}
	g.game_over = false
	g.first_click = true
	g.mines_flagged = 0
}

fn on_event(e &gg.Event, mut g Game) {
	if e.typ == .key_down {
		match e.key_code {
			.escape { g.ctx.quit() }
			.r { g.restart() }
			else {}
		}
		return
	}
	if g.game_over {
		return
	}
	if e.typ != .mouse_down {
		return
	}
	x := int(e.mouse_x / g.csize)
	y := int((e.mouse_y - header_size) / g.csize)
	if e.mouse_button == .left {
		if g.first_click {
			g.generate_mines(y, x)
			g.first_click = false
		}
		g.reveal(y, x)
	}
	if e.mouse_button == .right {
		if !g.revealed[y][x] {
			old := g.flags[y][x]
			g.flags[y][x] = !old
			g.mines_flagged += if old { -1 } else { 1 }
		}
	}
	if e.mouse_button == .middle {
		if g.revealed[y][x] {
			count := g.act_on_neighbors(y, x, fn (mut g Game, cy int, cx int) int {
				if g.in_grid(cy, cx) {
					return int(g.flags[cy][cx])
				}
				return 0
			})
			if int(g.grid[y][x]) == count {
				g.act_on_neighbors(y, x, fn (mut g Game, cy int, cx int) int {
					if g.in_grid(cy, cx) && !g.flags[cy][cx] {
						g.reveal(cy, cx)
					}
					return 0
				})
			}
		}
	}
}

fn (mut g Game) act_on_neighbors(y int, x int, f fn (mut g Game, cy int, cx int) int) int {
	mut count := 0
	for dy in -1 .. 2 {
		for dx in -1 .. 2 {
			if dy == 0 && dx == 0 {
				continue
			}
			cy, cx := y + dy, x + dx
			count += f(mut g, cy, cx)
		}
	}
	return count
}

fn (mut g Game) draw_cell(y int, x int) {
	o := header_size
	rect_x, rect_y := x * g.csize, y * g.csize
	if g.revealed[y][x] {
		if g.grid[y][x] == .mine {
			g.ctx.draw_rect_filled(rect_x, o + rect_y, g.csize, g.csize, gx.red)
			g.ctx.draw_text(rect_x + 10, o + rect_y + 5, '*', color: gx.black)
		} else if int(g.grid[y][x]) > 0 {
			g.ctx.draw_rect_filled(rect_x, o + rect_y, g.csize, g.csize, gx.light_gray)
			n := int(g.grid[y][x]).str()
			g.ctx.draw_text(rect_x + 10, o + rect_y + 5, n, color: gx.black)
		} else {
			c := gx.rgb(240, 240, 240)
			g.ctx.draw_rect_filled(rect_x, o + rect_y, g.csize, g.csize, c)
		}
	} else if g.flags[y][x] {
		g.ctx.draw_rect_filled(rect_x, o + rect_y, g.csize, g.csize, gx.gray)
		g.ctx.draw_text(rect_x + 10, o + rect_y + 5, 'F', color: gx.white)
	} else {
		g.ctx.draw_rect_filled(rect_x, o + rect_y, g.csize, g.csize, gx.gray)
	}
	g.ctx.draw_rect_empty(rect_x, o + rect_y, g.csize, g.csize, gx.black)
}

fn on_frame(mut g Game) {
	g.ctx.begin()
	for y in 0 .. g.size {
		for x in 0 .. g.size {
			g.draw_cell(y, x)
		}
	}
	message := 'Flagged: ${g.mines_flagged:02}/${g.mines:02}               (r)estart (ESC)ape'
	g.ctx.draw_text(5, 7, message, color: gx.green)
	g.ctx.end()
}

fn main() {
	mut g := &Game{}
	g.restart()
	g.ctx = gg.new_context(
		bg_color:     gx.black
		width:        g.size * g.csize
		height:       header_size + g.size * g.csize
		window_title: 'V Minesweeper'
		user_data:    g
		frame_fn:     on_frame
		event_fn:     on_event
		font_path:    asset.get_path('../assets', 'fonts/RobotoMono-Regular.ttf')
	)
	g.ctx.run()
}
