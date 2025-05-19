import os
import os.asset
import gg
import gx

const csize = 32

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
	pushes    int
	ctx       &gg.Context = unsafe { nil }
	//
	id_box            int
	id_box_on_storage int
	id_wall           int
	id_floor          int
	id_other          int
	id_player         int
	id_storage        int
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
				`#`, ` ` {
					row << c
				}
				`b`, `$` { // a normal box
					row << ` `
					boxes << Pos{x, y}
				}
				`p`, `@` { // a normal player
					row << ` `
					player = Pos{x, y}
				}
				`.` { // storage
					row << `@`
				}
				`*` { // box on storage
					row << `@`
					boxes << Pos{x, y}
				}
				`+` { // player on storage
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
	g.pushes = 0
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
		g.pushes++
	}
	g.player = target
	g.moves++
}

fn (g &Game) get_cell_iid(pos Pos) int {
	c := g.warehouse[pos.y][pos.x]
	if pos.x == g.player.x && pos.y == g.player.y {
		return g.id_player
	}
	for box in g.boxes {
		if box.x == pos.x && box.y == pos.y {
			if c == `@` {
				return g.id_box_on_storage
			}
			return g.id_box
		}
	}
	match c {
		` ` { return g.id_floor }
		`#` { return g.id_wall }
		`@` { return g.id_storage }
		else { return g.id_other }
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
			iid := g.get_cell_iid(pos)
			if iid == g.id_player {
				// the player is transparent
				g.ctx.draw_image_by_id(ox + x * csize, oy + y * csize, 32, 32, g.id_floor)
			}
			g.ctx.draw_image_by_id(ox + x * csize, oy + y * csize, 32, 32, iid)
		}
	}
	g.ctx.draw_rect_filled(0, ws.height - 70, ws.width, 70, gx.black)
	if g.win {
		g.ctext(ws, -50, 'You win!!!', 60, gx.yellow)
		g.ctext(ws, -15, 'Press `space` to continue.', 20, gx.gray)
	} else {
		for idx, title in g.titles {
			g.ctext(ws, -65 + (idx * 20), title, 22, gx.white)
		}
		g.ctext(ws, -65 + (g.titles.len * 20), 'Boxes: ${g.boxes.len:04}', 16, gx.gray)
	}
	g.ctx.draw_rect_filled(0, 0, ws.width, 40, gx.black)
	g.ctx.draw_text(30, 0, 'Level: ${g.level + 1:02}', color: gx.green, size: 40)
	g.ctx.draw_text(ws.width - 225, 0, 'Moves: ${g.moves:04}', color: gx.green, size: 40)
	g.ctx.draw_text(ws.width / 2 - 110, 0, 'Pushes: ${g.pushes:04}', color: gx.green, size: 40)
	g.ctx.end()
}

fn (mut g Game) iid(name string) !int {
	return g.ctx.create_image(asset.get_path('/', name))!.id
}

fn main() {
	mut g := &Game{}
	if os.args.len > 1 {
		for fpath in os.args[1..] {
			content := os.read_file(fpath)!
			if content.starts_with(';') {
				// many levels in the same file:
				parts := content.trim_space().split('\n\n')
				for part in parts {
					g.levels << '${fpath}${part}'
				}
			} else {
				// a single level:
				g.levels << content
			}
		}
	} else {
		all_level_names := asset.read_text('/', '_all_levels.txt')!.split_into_lines()
		g.levels = all_level_names.map(asset.read_text('/', it)!)
	}
	g.parse_level(0)!
	g.ctx = gg.new_context(
		width:        800
		height:       640
		window_title: 'V Sokoban'
		user_data:    g
		frame_fn:     g.draw_frame
		keydown_fn:   g.key_down
	)
	g.id_box = g.iid('box.png')!
	g.id_box_on_storage = g.iid('box_on_storage.png')!
	g.id_wall = g.iid('wall.png')!
	g.id_floor = g.iid('floor.png')!
	g.id_other = g.iid('other.png')!
	g.id_player = g.iid('player.png')!
	g.id_storage = g.iid('storage.png')!
	g.ctx.run()
}
