module main

import gg
import gx
import rand
import time

const cover = gx.rgba(85, 200, 85, 255)
const csize = 120 // cell size in pixels
const letters = 'AABBOOCCVVXXYYZZMMKKHHTT'.split('')
const header_size = 30

struct Cell {
mut:
	is_open bool
	letter  string
}

struct Game {
mut:
	ctx       &gg.Context = unsafe { nil }
	cells     []Cell
	card1_idx ?int
	card2_idx ?int
	size      int // in cells
	remaining int
	sw        time.StopWatch = time.new_stopwatch()
	revert_sw time.StopWatch = time.new_stopwatch(auto_start: false)
}

fn (mut g Game) restart() {
	ncells := g.size * g.size
	g.remaining = ncells
	g.cells = []Cell{len: ncells, init: Cell{
		letter: letters[index % letters.len]
	}}
	rand.shuffle(mut g.cells) or {}
	g.sw = time.new_stopwatch()
	g.card1_idx = none
	g.card2_idx = none
}

fn (mut g Game) draw_cell(i int, cell Cell) {
	x, y := i % g.size, i / g.size
	rect_x, rect_y := x * csize, header_size + y * csize
	if g.cells[i].is_open || g.sw.elapsed().milliseconds() <= 1000 {
		lsize := 96
		g.ctx.draw_rect_empty(rect_x + 6, rect_y + 6, csize - 10, csize - 10, gx.light_gray)
		g.ctx.draw_text(rect_x + csize / 2 - lsize / 3, rect_y + csize / 2 - lsize / 2,
			g.cells[i].letter, color: gx.yellow, size: lsize)
	} else {
		g.ctx.draw_rect_filled(rect_x + 6, rect_y + 6, csize - 10, csize - 10, cover)
	}
}

fn on_frame(mut g Game) {
	ws := gg.window_size()
	g.ctx.begin()
	message := '(r)estart (esc)ape | remaining: ${g.remaining:02} | time: ${f64(g.sw.elapsed().milliseconds()) / 1000.0:06.1f}s'
	g.ctx.draw_text(ws.width / 2, 7, message, color: gx.light_gray, size: 22, align: .center)
	for i, cell in g.cells {
		g.draw_cell(i, cell)
	}
	if g.revert_sw.elapsed().milliseconds() > 750 {
		g.revert_sw = time.new_stopwatch(auto_start: false)
		if g.card1_idx != none {
			if g.card2_idx != none {
				g.cells[g.card1_idx].is_open = false
				g.cells[g.card2_idx].is_open = false
				g.card1_idx = none
				g.card2_idx = none
				g.remaining = g.cells.count(!it.is_open)
			}
		}
	}
	g.ctx.end()
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
	if e.typ != .mouse_down {
		return
	}
	x, y := int(e.mouse_x / csize), int((e.mouse_y - header_size) / csize)
	if e.mouse_button == .left && g.card2_idx == none {
		if g.remaining == 0 {
			g.restart()
			return
		}
		i := y * g.size + x
		if !g.cells[i].is_open {
			g.cells[i].is_open = true
			if g.card1_idx == none {
				g.card1_idx = i
			} else {
				g.card2_idx = i
				if g.cells[g.card1_idx].letter == g.cells[i].letter {
					g.card1_idx = none
					g.card2_idx = none
				} else {
					// start a timer, that will be checked in the on_frame callback
					g.revert_sw.start()
				}
			}
		}
	}
	g.remaining = g.cells.count(!it.is_open)
	if g.remaining == 0 {
		g.sw.stop()
	}
}

mut g := &Game{
	// the CLI argument should be number of pairs, so `size` is even, and the puzzle can be solved:
	size: arguments()[1] or { '3' }.int() * 2
}
g.restart()
g.ctx = gg.new_context(
	bg_color:     gx.black
	width:        g.size * csize
	height:       header_size + g.size * csize
	window_title: 'V Memory ${g.size} x ${g.size}'
	user_data:    g
	frame_fn:     on_frame
	event_fn:     on_event
	sample_count: 2
)
g.ctx.run()
