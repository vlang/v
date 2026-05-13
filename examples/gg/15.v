// Copyright (c) 2026 Delyan Angelov. All rights reserved.
// The use of this source code is governed by the MIT license.
module main

import gg
import os.asset
import rand

const n = 4
const tile = 110
const gap = 10
const pad = 24
const head = 52
const r = f32(18)
const speed = f32(0.14)
const board_px = n * tile + (n - 1) * gap
const width = pad * 2 + board_px
const height = head + board_px + 62
const wcolor = gg.rgb(104, 93, 79)

@[heap]
struct Game {
mut:
	ctx   &gg.Context = unsafe { nil }
	board []int
	moves int
	won   bool
	anim  bool
	t     f32
	from  int = -1
	to    int = -1
	val   int
}

fn main() {
	mut g := &Game{}
	g.shuffle()
	g.ctx = gg.new_context(
		bg_color:     gg.rgb(247, 244, 235)
		width:        width
		height:       height
		sample_count: 4
		window_title: '15 Puzzle'
		user_data:    g
		frame_fn:     frame
		event_fn:     event
		font_path:    asset.get_path('../assets', 'fonts/Graduate-Regular.ttf')
	)
	g.ctx.run()
}

fn frame(mut g Game) {
	g.step()
	g.ctx.begin()
	g.header()
	g.board()
	g.ctx.end()
}

fn event(e &gg.Event, mut g Game) {
	if e.typ == .key_down {
		match e.key_code {
			.escape {
				g.ctx.quit()
			}
			.r {
				g.shuffle()
			}
			.up, .w {
				if !g.anim {
					g.slide(1, 0)
				}
			}
			.down, .s {
				if !g.anim {
					g.slide(-1, 0)
				}
			}
			.left, .a {
				if !g.anim {
					g.slide(0, 1)
				}
			}
			.right, .d {
				if !g.anim {
					g.slide(0, -1)
				}
			}
			else {}
		}

		return
	}
	if !g.anim && e.typ == .mouse_down && e.mouse_button == .left {
		if i := g.hit(int(e.mouse_x), int(e.mouse_y)) {
			g.move(i)
		}
	}
}

fn (mut g Game) header() {
	g.ctx.draw_text(width / 2, 16, if g.won {
		'Solved in ${g.moves} moves. Press R to reshuffle.'
	} else {
		'Moves: ${g.moves}'
	},
		size:  if g.won { 16 } else { 20 }
		bold:  true
		align: .center
		color: if g.won { wcolor } else { gg.rgb(48, 44, 37) }
	)
}

fn (mut g Game) board() {
	g.ctx.draw_rect_filled(pad - 8, head - 8, board_px + 16, board_px + 16, gg.rgb(218, 206, 188))
	for i, v in g.board {
		if g.anim && i == g.from {
			continue
		}
		x, y := xy(i)
		g.tile(x, y, v, v != 0 && v == i + 1)
	}
	if g.anim {
		x0, y0 := xy(g.from)
		x1, y1 := xy(g.to)
		g.tile(int(f32(x0) + f32(x1 - x0) * g.t), int(f32(y0) + f32(y1 - y0) * g.t), g.val, false)
	}
	g.ctx.draw_text(pad, head + board_px + 22,
		'Arrow keys / WASD or click a tile next to the empty space.',
		size:  16
		color: wcolor
	)
	g.ctx.draw_text(width / 2, height - 18, '[R] Shuffle  [Esc] Quit',
		size:           16
		bold:           true
		align:          .center
		vertical_align: .middle
		color:          wcolor
	)
}

fn (mut g Game) tile(x int, y int, v int, ok bool) {
	fill, border := if v == 0 {
		gg.rgb(238, 232, 220), gg.rgb(210, 202, 191)
	} else if ok {
		gg.rgb(214, 173, 108), gg.rgb(168, 121, 56)
	} else {
		gg.rgb(84, 110, 122), gg.rgb(48, 66, 74)
	}
	g.ctx.draw_rounded_rect_filled(x, y, tile, tile, r, fill)
	g.ctx.draw_rounded_rect_empty(x, y, tile, tile, r, border)
	if v != 0 {
		g.ctx.draw_text(x + tile / 2, y + tile / 2, v.str(),
			size:           42
			bold:           true
			align:          .center
			vertical_align: .middle
			color:          gg.white
		)
	}
}

fn (mut g Game) shuffle() {
	g.board = []int{len: n * n}
	for i in 0 .. g.board.len - 1 {
		g.board[i] = i + 1
	}
	for _ in 0 .. 300 {
		e := g.board.index(0)
		ns := neighbors(e)
		j := ns[rand.intn(ns.len) or { 0 }]
		g.board[e], g.board[j] = g.board[j], g.board[e]
	}
	if done(g.board) {
		g.board[g.board.len - 2], g.board[g.board.len - 3] = g.board[g.board.len - 3], g.board[g.board.len - 2]
	}
	g.moves, g.won, g.anim, g.t, g.from, g.to, g.val = 0, false, false, 0, -1, -1, 0
}

fn (mut g Game) slide(dr int, dc int) {
	e := g.board.index(0)
	row, col := e / n + dr, e % n + dc
	if row >= 0 && row < n && col >= 0 && col < n {
		g.move(row * n + col)
	}
}

fn (mut g Game) move(i int) bool {
	if g.won || g.anim || i < 0 || i >= g.board.len || g.board[i] == 0 {
		return false
	}
	e := g.board.index(0)
	dr, dc := i / n - e / n, i % n - e % n
	if !((dr == 0 && (dc == 1 || dc == -1)) || (dc == 0 && (dr == 1 || dr == -1))) {
		return false
	}
	g.anim, g.t, g.from, g.to, g.val = true, 0, i, e, g.board[i]
	return true
}

fn (mut g Game) step() {
	if !g.anim {
		return
	}
	g.t += speed
	if g.t < 1 {
		return
	}
	g.board[g.to], g.board[g.from] = g.val, 0
	g.moves++
	g.won = done(g.board)
	g.anim, g.t, g.from, g.to, g.val = false, 0, -1, -1, 0
}

fn (g &Game) hit(mx int, my int) ?int {
	if mx < pad || my < head || mx >= pad + board_px || my >= head + board_px {
		return none
	}
	x, y := mx - pad, my - head
	col, row := x / (tile + gap), y / (tile + gap)
	if x % (tile + gap) >= tile || y % (tile + gap) >= tile {
		return none
	}
	return row * n + col
}

fn neighbors(i int) []int {
	row, col := i / n, i % n
	mut ns := []int{}
	if row > 0 {
		ns << i - n
	}
	if row + 1 < n {
		ns << i + n
	}
	if col > 0 {
		ns << i - 1
	}
	if col + 1 < n {
		ns << i + 1
	}
	return ns
}

fn xy(i int) (int, int) {
	return pad + (i % n) * (tile + gap), head + (i / n) * (tile + gap)
}

fn done(b []int) bool {
	for i in 0 .. b.len - 1 {
		if b[i] != i + 1 {
			return false
		}
	}
	return b[b.len - 1] == 0
}
