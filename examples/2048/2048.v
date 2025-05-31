import gg
import gx
import math
import math.easing
import os.asset
import rand
import time

const zooming_percent_per_frame = 5
const movement_percent_per_frame = 10

const window_title = 'V 2048'
const default_window_width = 544
const default_window_height = 560

const possible_moves = [Direction.up, .right, .down, .left]
const predictions_per_move = 300
const prediction_depth = 8

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	touch       TouchInfo
	ui          Ui
	theme       &Theme = themes[0]
	theme_idx   int
	board       Board
	undo        []Undo
	atickers    [4][4]f64
	mtickers    [4][4]f64
	state       GameState  = .play
	tile_format TileFormat = .normal
	moves       int
	updates     u64

	is_ai_mode bool
	ai_fpm     u64 = 8
}

struct Ui {
mut:
	dpi_scale     f32
	tile_size     int
	border_size   int
	padding_size  int
	header_size   int
	font_size     int
	window_width  int
	window_height int
	x_padding     int
	y_padding     int
}

struct Theme {
	bg_color        gx.Color
	padding_color   gx.Color
	text_color      gx.Color
	game_over_color gx.Color
	victory_color   gx.Color
	tile_colors     []gx.Color
}

const themes = [
	&Theme{
		bg_color:        gx.rgb(250, 248, 239)
		padding_color:   gx.rgb(143, 130, 119)
		victory_color:   gx.rgb(100, 160, 100)
		game_over_color: gx.rgb(190, 50, 50)
		text_color:      gx.black
		tile_colors:     [
			gx.rgb(205, 193, 180), // Empty / 0 tile
			gx.rgb(238, 228, 218), // 2
			gx.rgb(237, 224, 200), // 4
			gx.rgb(242, 177, 121), // 8
			gx.rgb(245, 149, 99), // 16
			gx.rgb(246, 124, 95), // 32
			gx.rgb(246, 94, 59), // 64
			gx.rgb(237, 207, 114), // 128
			gx.rgb(237, 204, 97), // 256
			gx.rgb(237, 200, 80), // 512
			gx.rgb(237, 197, 63), // 1024
			gx.rgb(237, 194, 46),
		]
	},
	&Theme{
		bg_color:        gx.rgb(55, 55, 55)
		padding_color:   gx.rgb(68, 60, 59)
		victory_color:   gx.rgb(100, 160, 100)
		game_over_color: gx.rgb(190, 50, 50)
		text_color:      gx.white
		tile_colors:     [
			gx.rgb(123, 115, 108),
			gx.rgb(142, 136, 130),
			gx.rgb(142, 134, 120),
			gx.rgb(145, 106, 72),
			gx.rgb(147, 89, 59),
			gx.rgb(147, 74, 57),
			gx.rgb(147, 56, 35),
			gx.rgb(142, 124, 68),
			gx.rgb(142, 122, 58),
			gx.rgb(142, 120, 48),
			gx.rgb(142, 118, 37),
			gx.rgb(142, 116, 27),
		]
	},
	&Theme{
		bg_color:        gx.rgb(38, 38, 66)
		padding_color:   gx.rgb(58, 50, 74)
		victory_color:   gx.rgb(100, 160, 100)
		game_over_color: gx.rgb(190, 50, 50)
		text_color:      gx.white
		tile_colors:     [
			gx.rgb(92, 86, 140),
			gx.rgb(106, 99, 169),
			gx.rgb(106, 97, 156),
			gx.rgb(108, 79, 93),
			gx.rgb(110, 66, 76),
			gx.rgb(110, 55, 74),
			gx.rgb(110, 42, 45),
			gx.rgb(106, 93, 88),
			gx.rgb(106, 91, 75),
			gx.rgb(106, 90, 62),
			gx.rgb(106, 88, 48),
			gx.rgb(106, 87, 35),
		]
	},
]

struct Pos {
	x int = -1
	y int = -1
}

struct Board {
mut:
	field  [4][4]int
	oidxs  [4][4]u32 // old indexes of the fields, when != 0;  each index is an encoding of its y,x coordinates = y << 16 | x
	points int
	shifts int
}

struct Undo {
	board Board
	state GameState
}

struct TileLine {
	ypos int
mut:
	field  [5]int
	oidxs  [5]u32
	points int
	shifts int
}

struct TouchInfo {
mut:
	start Touch
	end   Touch
}

struct Touch {
mut:
	pos  Pos
	time time.Time
}

enum TileFormat {
	normal
	log
	exponent
	shifts
	none
	end // To know when to wrap around
}

enum GameState {
	play
	over
	victory
	freeplay
}

enum LabelKind {
	keys
	points
	moves
	tile
	victory
	game_over
	score_end
}

enum Direction {
	up
	down
	left
	right
}

// Utility functions
@[inline]
fn avg(a int, b int) int {
	return (a + b) / 2
}

fn (b Board) transpose() Board {
	mut res := b
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			res.field[y][x] = b.field[x][y]
			res.oidxs[y][x] = b.oidxs[x][y]
		}
	}
	return res
}

fn (b Board) hmirror() Board {
	mut res := b
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			res.field[y][x] = b.field[y][3 - x]
			res.oidxs[y][x] = b.oidxs[y][3 - x]
		}
	}
	return res
}

fn (t TileLine) to_left() TileLine {
	right_border_idx := 4
	mut res := t
	mut zeros := 0
	mut nonzeros := 0
	// gather meta info about the line:
	for x in res.field {
		if x == 0 {
			zeros++
		} else {
			nonzeros++
		}
	}
	if nonzeros == 0 {
		// when all the tiles are empty, there is nothing left to do
		return res
	}
	if zeros > 0 {
		// we have some 0s, do shifts to compact them:
		mut remaining_zeros := zeros
		for x := 0; x < right_border_idx - 1; x++ {
			for res.field[x] == 0 && remaining_zeros > 0 {
				res.shifts++
				for k := x; k < right_border_idx; k++ {
					res.field[k] = res.field[k + 1]
					res.oidxs[k] = res.oidxs[k + 1]
				}
				remaining_zeros--
			}
		}
	}
	// At this point, the non 0 tiles are all on the left, with no empty spaces
	// between them. we can safely merge them, when they have the same value:
	for x := 0; x < right_border_idx - 1; x++ {
		if res.field[x] == 0 {
			break
		}
		if res.field[x] == res.field[x + 1] {
			for k := x; k < right_border_idx; k++ {
				res.field[k] = res.field[k + 1]
				res.oidxs[k] = res.oidxs[k + 1]
			}
			res.shifts++
			res.field[x]++
			res.points += 1 << res.field[x]
		}
	}
	return res
}

fn (b Board) to_left() Board {
	mut res := b
	for y in 0 .. 4 {
		mut hline := TileLine{
			ypos: y
		}
		for x in 0 .. 4 {
			hline.field[x] = b.field[y][x]
			hline.oidxs[x] = b.oidxs[y][x]
		}
		reshline := hline.to_left()
		res.shifts += reshline.shifts
		res.points += reshline.points
		for x in 0 .. 4 {
			res.field[y][x] = reshline.field[x]
			res.oidxs[y][x] = reshline.oidxs[x]
		}
	}
	return res
}

fn yx2i(y int, x int) u32 {
	return u32(y) << 16 | u32(x)
}

fn (mut b Board) move(d Direction) (Board, bool) {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			b.oidxs[y][x] = yx2i(y, x)
		}
	}
	new := match d {
		.left { b.to_left() }
		.right { b.hmirror().to_left().hmirror() }
		.up { b.transpose().to_left().transpose() }
		.down { b.transpose().hmirror().to_left().hmirror().transpose() }
	}
	// If the board hasn't changed, it's an illegal move, don't allow it.
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			if b.field[y][x] != new.field[y][x] {
				return new, true
			}
		}
	}
	return new, false
}

fn (mut b Board) is_game_over() bool {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := b.field[y][x]
			if fidx == 0 {
				// there are remaining zeros
				return false
			}
			if (x > 0 && fidx == b.field[y][x - 1])
				|| (x < 4 - 1 && fidx == b.field[y][x + 1])
				|| (y > 0 && fidx == b.field[y - 1][x])
				|| (y < 4 - 1 && fidx == b.field[y + 1][x]) {
				// there are remaining merges
				return false
			}
		}
	}
	return true
}

fn (mut app App) update_tickers() {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			app.atickers[y][x] = math.clip(app.atickers[y][x] - f64(zooming_percent_per_frame) / 100.0,
				0.0, 1.0)
			app.mtickers[y][x] = math.clip(app.mtickers[y][x] - f64(movement_percent_per_frame) / 100.0,
				0.0, 1.0)
		}
	}
}

fn (mut app App) new_game() {
	app.board = Board{}
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			app.board.field[y][x] = 0
			app.atickers[y][x] = 0
			app.mtickers[y][x] = 0
		}
	}
	app.state = .play
	app.undo = []Undo{cap: 4096}
	app.moves = 0
	app.new_random_tile()
	app.new_random_tile()
}

@[inline]
fn (mut app App) check_for_victory() {
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := app.board.field[y][x]
			if fidx == 11 {
				app.state = .victory
				return
			}
		}
	}
}

@[inline]
fn (mut app App) check_for_game_over() {
	if app.board.is_game_over() {
		app.state = .over
	}
}

fn (mut b Board) place_random_tile() (Pos, int) {
	mut etiles := [16]Pos{}
	mut empty_tiles_max := 0
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := b.field[y][x]
			if fidx == 0 {
				etiles[empty_tiles_max] = Pos{x, y}
				empty_tiles_max++
			}
		}
	}
	if empty_tiles_max > 0 {
		new_random_tile_index := rand.intn(empty_tiles_max) or { 0 }
		empty_pos := etiles[new_random_tile_index]
		// 10% chance of getting a `4` tile
		value := rand.f64n(1.0) or { 0.0 }
		random_value := if value < 0.9 { 1 } else { 2 }
		b.field[empty_pos.y][empty_pos.x] = random_value
		b.oidxs[empty_pos.y][empty_pos.x] = yx2i(empty_pos.y, empty_pos.x)
		return empty_pos, random_value
	}
	return Pos{}, 0
}

fn (mut app App) new_random_tile() {
	// do not animate empty fields:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			fidx := app.board.field[y][x]
			if fidx == 0 {
				app.atickers[y][x] = 0
				app.board.oidxs[y][x] = 0xFFFF_FFFF
			}
		}
	}
	empty_pos, random_value := app.board.place_random_tile()
	if random_value > 0 {
		app.atickers[empty_pos.y][empty_pos.x] = 1.0
	}
	if app.state != .freeplay {
		app.check_for_victory()
	}
	app.check_for_game_over()
}

fn (mut app App) apply_new_board(new Board) {
	old := app.board
	app.moves++
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			if old.oidxs[y][x] != new.oidxs[y][x] {
				app.mtickers[y][x] = 1.0
			}
		}
	}
	app.board = new
	app.undo << Undo{old, app.state}
	app.new_random_tile()
}

fn (mut app App) move(d Direction) {
	new, is_valid := app.board.move(d)
	if !is_valid {
		return
	}
	app.apply_new_board(new)
}

struct Prediction {
mut:
	move    Direction
	mpoints f64
	mcmoves f64
}

fn (p Prediction) str() string {
	return '{ move: ${p.move:5}, mpoints: ${p.mpoints:8.2f}, mcmoves: ${p.mcmoves:6.2f} }'
}

fn (mut app App) ai_move() {
	mut predictions := [4]Prediction{}
	mut is_valid := false
	think_watch := time.new_stopwatch()
	for move in possible_moves {
		move_idx := int(move)
		predictions[move_idx].move = move
		mut mpoints := 0
		mut mcmoves := 0
		for _ in 0 .. predictions_per_move {
			mut cboard := app.board
			cboard, is_valid = cboard.move(move)
			if !is_valid || cboard.is_game_over() {
				continue
			}
			mpoints += cboard.points
			mut cmoves := 0
			for !cboard.is_game_over() {
				nmove := rand.element(possible_moves) or { Direction.up }
				cboard, is_valid = cboard.move(nmove)
				if !is_valid {
					continue
				}
				cboard.place_random_tile()
				cmoves++
				if cmoves > prediction_depth {
					break
				}
			}
			mpoints += cboard.points
			mcmoves += cmoves
		}
		predictions[move_idx].mpoints = f64(mpoints) / predictions_per_move
		predictions[move_idx].mcmoves = f64(mcmoves) / predictions_per_move
	}
	mut bestprediction := Prediction{
		mpoints: -1
	}
	for move_idx in 0 .. possible_moves.len {
		if bestprediction.mpoints < predictions[move_idx].mpoints {
			bestprediction = predictions[move_idx]
		}
	}
	eprintln('Simulation time: ${think_watch.elapsed().microseconds():4}µs |  best ${bestprediction}')
	app.move(bestprediction.move)
}

fn (app &App) label_format(kind LabelKind) gx.TextCfg {
	match kind {
		.keys {
			return gx.TextCfg{
				color:          gx.Color{150, 150, 255, 200}
				align:          .center
				vertical_align: .bottom
				size:           app.ui.font_size / 4
			}
		}
		.points {
			return gx.TextCfg{
				color: if app.state in [.over, .victory] { gx.white } else { app.theme.text_color }
				align: .left
				size:  app.ui.font_size / 2
			}
		}
		.moves {
			return gx.TextCfg{
				color: if app.state in [.over, .victory] { gx.white } else { app.theme.text_color }
				align: .right
				size:  app.ui.font_size / 2
			}
		}
		.tile {
			return gx.TextCfg{
				color:          app.theme.text_color
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size
			}
		}
		.victory {
			return gx.TextCfg{
				color:          app.theme.victory_color
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size * 2
			}
		}
		.game_over {
			return gx.TextCfg{
				color:          app.theme.game_over_color
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size * 2
			}
		}
		.score_end {
			return gx.TextCfg{
				color:          gx.white
				align:          .center
				vertical_align: .middle
				size:           app.ui.font_size * 3 / 4
			}
		}
	}
}

@[inline]
fn (mut app App) set_theme(idx int) {
	theme := themes[idx]
	app.theme_idx = idx
	app.theme = theme
	app.gg.set_bg_color(theme.bg_color)
}

fn (mut app App) resize() {
	mut s := app.gg.scale
	if s == 0.0 {
		s = 1.0
	}
	window_size := app.gg.window_size()
	w := window_size.width
	h := window_size.height
	m := f32(math.min(w, h))
	app.ui.dpi_scale = s
	app.ui.window_width = w
	app.ui.window_height = h
	app.ui.padding_size = int(m / 38)
	app.ui.header_size = app.ui.padding_size
	app.ui.border_size = app.ui.padding_size * 2
	app.ui.tile_size = int((m - app.ui.padding_size * 5 - app.ui.border_size * 2) / 4)
	app.ui.font_size = int(m / 10)
	// If the window's height is greater than its width, center the board vertically.
	// If not, center it horizontally
	if w > h {
		app.ui.y_padding = 0
		app.ui.x_padding = (app.ui.window_width - app.ui.window_height) / 2
	} else {
		app.ui.y_padding = (app.ui.window_height - app.ui.window_width - app.ui.header_size) / 2
		app.ui.x_padding = 0
	}
}

fn (app &App) draw() {
	xpad, ypad := app.ui.x_padding, app.ui.y_padding
	ww := app.ui.window_width
	wh := app.ui.window_height
	m := math.min(ww, wh)
	labelx := xpad + app.ui.border_size
	labely := ypad + app.ui.border_size / 2
	app.draw_tiles()
	// TODO: Make transparency work in `gg`
	if app.state == .over {
		app.gg.draw_rect_filled(0, 0, ww, wh, gx.rgba(10, 0, 0, 180))
		app.gg.draw_text(ww / 2, (m * 4 / 10) + ypad, 'Game Over', app.label_format(.game_over))
		f := app.label_format(.tile)
		msg := $if android { 'Tap to restart' } $else { 'Press `r` to restart' }
		app.gg.draw_text(ww / 2, (m * 6 / 10) + ypad, msg, gx.TextCfg{
			...f
			color: gx.white
			size:  f.size * 3 / 4
		})
	}
	if app.state == .victory {
		app.gg.draw_rect_filled(0, 0, ww, wh, gx.rgba(0, 10, 0, 180))
		app.gg.draw_text(ww / 2, (m * 4 / 10) + ypad, 'Victory!', app.label_format(.victory))
		// f := app.label_format(.tile)
		msg1 := $if android { 'Tap to continue' } $else { 'Press `space` to continue' }
		msg2 := $if android { 'Tap to restart' } $else { 'Press `r` to restart' }
		app.gg.draw_text(ww / 2, (m * 6 / 10) + ypad, msg1, app.label_format(.score_end))
		app.gg.draw_text(ww / 2, (m * 8 / 10) + ypad, msg2, app.label_format(.score_end))
	}
	// Draw at the end, so that it's on top of the victory / game over overlays
	app.gg.draw_text(labelx, labely, 'Points: ${app.board.points}', app.label_format(.points))
	app.gg.draw_text(ww - labelx, labely, 'Moves: ${app.moves}', app.label_format(.moves))
	app.gg.draw_text(ww / 2, wh, 'Controls: WASD,V,<=,T,Enter,ESC', app.label_format(.keys))
}

fn (app &App) draw_tiles() {
	xstart := app.ui.x_padding + app.ui.border_size
	ystart := app.ui.y_padding + app.ui.border_size + app.ui.header_size
	toffset := app.ui.tile_size + app.ui.padding_size
	tiles_size := math.min(app.ui.window_width, app.ui.window_height) - app.ui.border_size * 2
	// Draw the padding around the tiles
	app.gg.draw_rounded_rect_filled(xstart, ystart, tiles_size, tiles_size, tiles_size / 24,
		app.theme.padding_color)

	// Draw empty tiles:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			tw := app.ui.tile_size
			th := tw // square tiles, w == h
			xoffset := xstart + app.ui.padding_size + x * toffset
			yoffset := ystart + app.ui.padding_size + y * toffset
			app.gg.draw_rounded_rect_filled(xoffset, yoffset, tw, th, tw / 8, app.theme.tile_colors[0])
		}
	}

	// Draw the already placed and potentially moving tiles:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			tidx := app.board.field[y][x]
			oidx := app.board.oidxs[y][x]
			if tidx == 0 || oidx == 0xFFFF_FFFF {
				continue
			}
			app.draw_one_tile(x, y, tidx)
		}
	}

	// Draw the newly placed random tiles on top of everything else:
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			tidx := app.board.field[y][x]
			oidx := app.board.oidxs[y][x]
			if oidx == 0xFFFF_FFFF && tidx != 0 {
				app.draw_one_tile(x, y, tidx)
			}
		}
	}
}

fn (app &App) draw_one_tile(x int, y int, tidx int) {
	xstart := app.ui.x_padding + app.ui.border_size
	ystart := app.ui.y_padding + app.ui.border_size + app.ui.header_size
	toffset := app.ui.tile_size + app.ui.padding_size
	oidx := app.board.oidxs[y][x]
	oy := oidx >> 16
	ox := oidx & 0xFFFF
	mut dx := 0
	mut dy := 0
	if oidx != 0xFFFF_FFFF {
		scaling := app.ui.tile_size * easing.in_out_quint(app.mtickers[y][x])
		if ox != x {
			dx = math.clip(int(scaling * (f64(ox) - f64(x))), -4 * app.ui.tile_size, 4 * app.ui.tile_size)
		}
		if oy != y {
			dy = math.clip(int(scaling * (f64(oy) - f64(y))), -4 * app.ui.tile_size, 4 * app.ui.tile_size)
		}
	}
	tile_color := if tidx < app.theme.tile_colors.len {
		app.theme.tile_colors[tidx]
	} else {
		// If there isn't a specific color for this tile, reuse the last color available
		app.theme.tile_colors.last()
	}
	anim_size := 1.0 - app.atickers[y][x]
	tw := int(f64(anim_size * app.ui.tile_size))
	th := tw // square tiles, w == h
	xoffset := dx + xstart + app.ui.padding_size + x * toffset + (app.ui.tile_size - tw) / 2
	yoffset := dy + ystart + app.ui.padding_size + y * toffset + (app.ui.tile_size - th) / 2
	app.gg.draw_rounded_rect_filled(xoffset, yoffset, tw, th, tw / 8, tile_color)
	if tidx != 0 { // 0 == blank spot
		xpos := xoffset + tw / 2
		ypos := yoffset + th / 2
		mut fmt := app.label_format(.tile)
		fmt = gx.TextCfg{
			...fmt
			size: int(anim_size * (fmt.size - 1))
		}
		match app.tile_format {
			.normal {
				app.gg.draw_text(xpos, ypos, '${1 << tidx}', fmt)
			}
			.log {
				app.gg.draw_text(xpos, ypos, '${tidx}', fmt)
			}
			.exponent {
				app.gg.draw_text(xpos, ypos, '2', fmt)
				fs2 := int(f32(fmt.size) * 0.67)
				app.gg.draw_text(xpos + app.ui.tile_size / 10, ypos - app.ui.tile_size / 8,
					'${tidx}', gx.TextCfg{
					...fmt
					size:  fs2
					align: gx.HorizontalAlign.left
				})
			}
			.shifts {
				fs2 := int(f32(fmt.size) * 0.6)
				app.gg.draw_text(xpos, ypos, '2<<${tidx - 1}', gx.TextCfg{
					...fmt
					size: fs2
				})
			}
			.none {} // Don't draw any text here, colors only
			.end {} // Should never get here
		}
		// oidx_fmt := gx.TextCfg{...fmt,size: 14}
		// app.gg.draw_text(xoffset + 50, yoffset + 15, 'y:${oidx >> 16}|x:${oidx & 0xFFFF}|m:${app.mtickers[y][x]:5.3f}',	oidx_fmt)
		// app.gg.draw_text(xoffset + 52, yoffset + 30, 'ox:${ox}|oy:${oy}', oidx_fmt)
		// app.gg.draw_text(xoffset + 52, yoffset + 85, 'dx:${dx}|dy:${dy}', oidx_fmt)
	}
}

fn (mut app App) handle_touches() {
	s, e := app.touch.start, app.touch.end
	adx, ady := math.abs(e.pos.x - s.pos.x), math.abs(e.pos.y - s.pos.y)
	if math.max(adx, ady) < 10 {
		app.handle_tap()
	} else {
		app.handle_swipe()
	}
}

fn (mut app App) handle_tap() {
	_, ypad := app.ui.x_padding, app.ui.y_padding
	w, h := app.ui.window_width, app.ui.window_height
	m := math.min(w, h)
	s, e := app.touch.start, app.touch.end
	avgx, avgy := avg(s.pos.x, e.pos.x), avg(s.pos.y, e.pos.y)
	// TODO: Replace "touch spots" with actual buttons
	// bottom left -> change theme
	if avgx < 50 && h - avgy < 50 {
		app.next_theme()
	}
	// bottom right -> change tile format
	if w - avgx < 50 && h - avgy < 50 {
		app.next_tile_format()
	}
	if app.state == .victory {
		if avgy > (m / 2) + ypad {
			if avgy < (m * 7 / 10) + ypad {
				app.state = .freeplay
			} else if avgy < (m * 9 / 10) + ypad {
				app.new_game()
			} else {
				// TODO: remove and implement an actual way to toggle themes on mobile
			}
		}
	} else if app.state == .over {
		if avgy > (m / 2) + ypad && avgy < (m * 7 / 10) + ypad {
			app.new_game()
		}
	}
}

fn (mut app App) handle_swipe() {
	// Currently, swipes are only used to move the tiles.
	// If the user's not playing, exit early to avoid all the unnecessary calculations
	if app.state !in [.play, .freeplay] {
		return
	}
	s, e := app.touch.start, app.touch.end
	w, h := app.ui.window_width, app.ui.window_height
	dx, dy := e.pos.x - s.pos.x, e.pos.y - s.pos.y
	adx, ady := math.abs(dx), math.abs(dy)
	dmin := if math.min(adx, ady) > 0 { math.min(adx, ady) } else { 1 }
	dmax := if math.max(adx, ady) > 0 { math.max(adx, ady) } else { 1 }
	tdiff := (e.time - s.time).milliseconds()
	// TODO: make this calculation more accurate (don't use arbitrary numbers)
	min_swipe_distance := int(math.sqrt(math.min(w, h) * tdiff / 100)) + 20
	if dmax < min_swipe_distance {
		return
	}
	// Swipe was too short
	if dmax / dmin < 2 {
		return
	}
	// Swiped diagonally
	if adx > ady {
		if dx < 0 {
			app.move(.left)
		} else {
			app.move(.right)
		}
	} else {
		if dy < 0 {
			app.move(.up)
		} else {
			app.move(.down)
		}
	}
}

@[inline]
fn (mut app App) next_theme() {
	app.set_theme(if app.theme_idx == themes.len - 1 { 0 } else { app.theme_idx + 1 })
}

@[inline]
fn (mut app App) next_tile_format() {
	app.tile_format = unsafe { TileFormat(int(app.tile_format) + 1) }
	if app.tile_format == .end {
		app.tile_format = .normal
	}
}

@[inline]
fn (mut app App) undo() {
	if app.undo.len > 0 {
		undo := app.undo.pop()
		app.board = undo.board
		app.state = undo.state
		app.moves--
	}
}

fn (mut app App) on_key_down(key gg.KeyCode) {
	// these keys are independent from the game state:
	match key {
		.v { app.is_ai_mode = !app.is_ai_mode }
		.page_up { app.ai_fpm = dump(math.min(app.ai_fpm + 1, 60)) }
		.page_down { app.ai_fpm = dump(math.max(app.ai_fpm - 1, 1)) }
		//
		.escape { app.gg.quit() }
		.n, .r { app.new_game() }
		.backspace { app.undo() }
		.enter { app.next_tile_format() }
		.j { app.state = .over }
		.t { app.next_theme() }
		else {}
	}
	if app.state in [.play, .freeplay] {
		if !app.is_ai_mode {
			match key {
				.w, .up { app.move(.up) }
				.a, .left { app.move(.left) }
				.s, .down { app.move(.down) }
				.d, .right { app.move(.right) }
				else {}
			}
		}
	}
	if app.state == .victory {
		if key == .space {
			app.state = .freeplay
		}
	}
}

fn on_event(e &gg.Event, mut app App) {
	match e.typ {
		.key_down {
			app.on_key_down(e.key_code)
		}
		.resized, .restored, .resumed {
			app.resize()
		}
		.touches_began {
			if e.num_touches > 0 {
				t := e.touches[0]
				app.touch.start = Touch{
					pos:  Pos{
						x: int(t.pos_x / app.ui.dpi_scale)
						y: int(t.pos_y / app.ui.dpi_scale)
					}
					time: time.now()
				}
			}
		}
		.touches_ended {
			if e.num_touches > 0 {
				t := e.touches[0]
				app.touch.end = Touch{
					pos:  Pos{
						x: int(t.pos_x / app.ui.dpi_scale)
						y: int(t.pos_y / app.ui.dpi_scale)
					}
					time: time.now()
				}
				app.handle_touches()
			}
		}
		.mouse_down {
			app.touch.start = Touch{
				pos:  Pos{
					x: int(e.mouse_x / app.ui.dpi_scale)
					y: int(e.mouse_y / app.ui.dpi_scale)
				}
				time: time.now()
			}
		}
		.mouse_up {
			app.touch.end = Touch{
				pos:  Pos{
					x: int(e.mouse_x / app.ui.dpi_scale)
					y: int(e.mouse_y / app.ui.dpi_scale)
				}
				time: time.now()
			}
			app.handle_touches()
		}
		else {}
	}
}

fn frame(mut app App) {
	mut do_update := false
	if app.gg.timer.elapsed().milliseconds() > 15 {
		app.gg.timer.restart()
		do_update = true
		app.updates++
	}
	app.gg.begin()
	if do_update {
		app.update_tickers()
	}
	app.draw()
	app.gg.end()
	if do_update && app.is_ai_mode && app.state in [.play, .freeplay]
		&& app.updates % app.ai_fpm == 0 {
		app.ai_move()
	}
	if app.updates % 120 == 0 {
		// do GC once per 2 seconds
		// eprintln('> gc_memory_use: ${gc_memory_use()}')
		if gc_is_enabled() {
			// Avoid assert error when built with `-cg` on some systems
			gc_disable()
		}
		gc_enable()
		gc_collect()
		gc_disable()
	}
}

fn init(mut app App) {
	app.resize()
}

fn main() {
	mut app := &App{}
	app.new_game()
	app.gg = gg.new_context(
		bg_color:     app.theme.bg_color
		width:        default_window_width
		height:       default_window_height
		sample_count: 2 // higher quality curves
		window_title: 'V 2048'
		frame_fn:     frame
		event_fn:     on_event
		init_fn:      init
		user_data:    app
		font_path:    asset.get_path('../assets', 'fonts/RobotoMono-Regular.ttf')
	)
	app.gg.run()
}
