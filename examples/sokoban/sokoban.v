import os.asset
import gg
import gx

const csize = 32
const color_box = gx.rgb(139, 69, 19) // Brown
const color_wall = gx.rgb(100, 100, 100) // Gray
const color_floor = gx.rgb(200, 200, 200) // Light gray
const color_player = gx.rgb(0, 0, 255) // Blue
const color_storage = gx.rgb(0, 255, 0) // Green
const color_box_on_storage = gx.rgb(255, 255, 0) // Yellow

struct Pos {
	x int
	y int
}

enum Direction {
	up
	down
	left
	right
}

@[heap]
struct Game {
mut:
	warehouse [][]rune // the warehouse: `#`=wall, ` `=floor, `@`=storage, `b`=box, `p`=player
	ww        int
	wh        int
	player    Pos
	boxes     []Pos
	win       bool
	level     int
	titles    []string
	levels    []string
	moves     int
	ctx       &gg.Context = unsafe { nil }
}

fn (mut g Game) parse_level(lnumber int) ! {
	level := g.levels[lnumber] or { return error('invalid level number ${lnumber}') }
	if level == '' {
		return error('empty level ${lnumber}')
	}
	lines := level.split('\n').map(it.trim_space_right()).filter(it != '' && !it.starts_with('//'))
	mut warehouse := [][]rune{}
	mut warehouse_w := 0
	mut player := Pos{-1, -1}
	mut boxes := []Pos{}
	for y, line in lines#[1..] {
		mut row := []rune{}
		for x, c in line {
			match c {
				`#`, ` `, `@` {
					row << c
				}
				`b` { // a normal box
					row << ` `
					boxes << Pos{x, y}
				}
				`p` { // a normal player
					row << ` `
					player = Pos{x, y}
				}
				`B` { // box on storage
					row << `@`
					boxes << Pos{x, y}
				}
				`P` { // player on storage
					row << `@`
					player = Pos{x, y}
				}
				else {
					return error('unknown rune `${rune(c)}` at position ${y}x${x}, in level: ${c}')
				}
			}
		}
		warehouse << row
		warehouse_w = int_max(row.len, warehouse_w)
	}
	g.warehouse = warehouse
	g.ww = warehouse_w * csize
	g.wh = warehouse.len * csize
	g.titles = lines[0].split('@').filter(it != '')
	g.player = player
	g.boxes = boxes
	g.moves = 0
	g.win = false
}

// is_valid_pos returns whether the given `pos` is inside the game field, and not inside an inner wall
fn (g &Game) is_valid_pos(pos Pos) bool {
	return pos.y >= 0 && pos.y < g.warehouse.len && pos.x >= 0 && pos.x < g.warehouse[pos.y].len
		&& g.warehouse[pos.y][pos.x] != `#`
}

fn (mut g Game) move_player(dir Direction) {
	dx, dy := match dir {
		.up { 0, -1 }
		.down { 0, 1 }
		.left { -1, 0 }
		.right { 1, 0 }
	}
	target := Pos{g.player.x + dx, g.player.y + dy}
	if !g.is_valid_pos(target) {
		return
	}
	// Check for a box at the target position:
	mut target_box_index := -1
	for i, box in g.boxes {
		if box.x == target.x && box.y == target.y {
			target_box_index = i
			break
		}
	}
	if target_box_index >= 0 {
		// try pushing the box
		target_after := Pos{g.player.x + 2 * dx, g.player.y + 2 * dy}
		if !g.is_valid_pos(target_after) {
			return
		}
		// if there is another box at that place, prevent the push:
		for box in g.boxes {
			if box.x == target_after.x && box.y == target_after.y {
				return
			}
		}
		g.boxes[target_box_index] = target_after
	}
	g.player = target
	g.moves++
}

fn (g &Game) get_cell_color(pos Pos) gx.Color {
	c := g.warehouse[pos.y][pos.x]
	if pos.x == g.player.x && pos.y == g.player.y {
		return color_player
	}
	for box in g.boxes {
		if box.x == pos.x && box.y == pos.y {
			if c == `@` {
				return color_box_on_storage
			}
			return color_box
		}
	}
	match c {
		` ` { return color_floor }
		`#` { return color_wall }
		`@` { return color_storage }
		else { return gx.black }
	}
}

fn (mut g Game) next_level() {
	nlevel := (g.level + 1) % g.levels.len
	g.parse_level(nlevel) or { eprintln('level error: ${err}') }
	g.level = nlevel
}

fn (mut g Game) key_down(key gg.KeyCode, _ gg.Modifier, _ voidptr) {
	// controls:
	match key {
		.escape {
			g.ctx.quit()
		}
		.space {
			if g.win {
				g.next_level()
				return
			}
		}
		.r {
			g.parse_level(g.level) or {}
			return
		}
		.n {
			g.next_level()
		}
		else {}
	}
	if g.win {
		return
	}
	// player movement:
	dir := match key {
		.w, .up { Direction.up }
		.s, .down { Direction.down }
		.a, .left { Direction.left }
		.d, .right { Direction.right }
		else { return }
	}
	g.move_player(dir)
	g.win = g.boxes.all(g.warehouse[it.y][it.x] == `@`)
}

fn (g &Game) ctext(ws gg.Size, oy int, message string, size int, color gx.Color) {
	g.ctx.draw_text(ws.width / 2, ws.height + oy, message,
		color:          color
		size:           size
		align:          .center
		vertical_align: .middle
	)
}

fn (g &Game) draw_frame(_ voidptr) {
	g.ctx.begin()
	ws := gg.window_size()
	ox := (ws.width - g.ww) / 2
	oy := (ws.height - 40 - g.wh) / 2
	for y in 0 .. g.warehouse.len {
		for x in 0 .. g.warehouse[y].len {
			pos := Pos{x, y}
			color := g.get_cell_color(pos)
			g.ctx.draw_rect_filled(ox + x * csize, oy + y * csize, csize, csize, color)
		}
	}
	g.ctx.draw_rect_filled(0, ws.height - 70, ws.width, 70, gx.black)
	if g.win {
		g.ctext(ws, -50, 'You win!!!', 60, gx.yellow)
		g.ctext(ws, -15, 'Press `space` to continue.', 20, gx.gray)
	} else {
		for idx, title in g.titles {
			g.ctext(ws, -60 + (idx * 20), title, 22, gx.white)
		}
	}
	g.ctx.draw_rect_filled(0, 0, ws.width, 40, gx.black)
	g.ctx.draw_text(5, 0, 'Level: ${g.level + 1:02}', color: gx.green, size: 40)
	g.ctx.draw_text(ws.width - 225, 0, 'Moves: ${g.moves:04}', color: gx.green, size: 40)
	g.ctx.end()
}

fn main() {
	mut g := &Game{}
	all_level_names := asset.read_bytes('/', '_all_levels.txt')!.bytestr().split_into_lines()
	g.levels = all_level_names.map(asset.read_bytes('/', it)!.bytestr())
	g.parse_level(0)!
	g.ctx = gg.new_context(
		width:        800
		height:       480
		window_title: 'V Sokoban'
		user_data:    g
		frame_fn:     g.draw_frame
		keydown_fn:   g.key_down
	)
	g.ctx.run()
}
