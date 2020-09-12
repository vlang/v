import gg
import gx
import os
import rand
import sokol.sapp
import time

struct App {
mut:
	gg            &gg.Context = 0
	touch         TouchInfo
	ui            Ui
	theme         &Theme = themes[0]
	theme_idx     int
	board         Board
	undo          []Board
	atickers      [4][4]int
	state         GameState = .play
	tile_format   TileFormat = .normal
	moves         int
	perf          &Perf = 0
}

struct Ui {
mut:
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

const (
	themes = [
		&Theme {
			bg_color: gx.rgb(250, 248, 239)
			padding_color: gx.rgb(143, 130, 119)
			victory_color: gx.rgb(100, 160, 100)
			game_over_color: gx.rgb(190, 50, 50)
			text_color: gx.black
			tile_colors: [
				gx.rgb(205, 193, 180) // Empty / 0 tile
				gx.rgb(238, 228, 218) // 2
				gx.rgb(237, 224, 200) // 4
				gx.rgb(242, 177, 121) // 8
				gx.rgb(245, 149, 99)  // 16
				gx.rgb(246, 124, 95)  // 32
				gx.rgb(246, 94, 59)   // 64
				gx.rgb(237, 207, 114) // 128
				gx.rgb(237, 204, 97)  // 256
				gx.rgb(237, 200, 80)  // 512
				gx.rgb(237, 197, 63)  // 1024
				gx.rgb(237, 194, 46)  // 2048
			]
		},
		&Theme {
			bg_color: gx.rgb(55, 55, 55)
			padding_color: gx.rgb(68, 60, 59)
			victory_color: gx.rgb(100, 160, 100)
			game_over_color: gx.rgb(190, 50, 50)
			text_color: gx.white
			tile_colors: [
				gx.rgb(123, 115, 108)
				gx.rgb(142, 136, 130)
				gx.rgb(142, 134, 120)
				gx.rgb(145, 106, 72)
				gx.rgb(147, 89, 59)
				gx.rgb(147, 74, 57)
				gx.rgb(147, 56, 35)
				gx.rgb(142, 124, 68)
				gx.rgb(142, 122, 58)
				gx.rgb(142, 120, 48)
				gx.rgb(142, 118, 37)
				gx.rgb(142, 116, 27)
			]
		},
		&Theme {
			bg_color: gx.rgb(38, 38, 66)
			padding_color: gx.rgb(58, 50, 74)
			victory_color: gx.rgb(100, 160, 100)
			game_over_color: gx.rgb(190, 50, 50)
			text_color: gx.white
			tile_colors: [
				gx.rgb(92, 86, 140)
				gx.rgb(106, 99, 169)
				gx.rgb(106, 97, 156)
				gx.rgb(108, 79, 93)
				gx.rgb(110, 66, 76)
				gx.rgb(110, 55, 74)
				gx.rgb(110, 42, 45)
				gx.rgb(106, 93, 88)
				gx.rgb(106, 91, 75)
				gx.rgb(106, 90, 62)
				gx.rgb(106, 88, 48)
				gx.rgb(106, 87, 35)
			]
		},
	]
	window_title          = 'V 2048'
	default_window_width  = 544
	default_window_height = 560
	animation_length      = 10 // frames
)

// Used for performance monitoring when `-d showfps` is passed, unused / optimized out otherwise
struct Perf {
mut:
	frame     int
	frame_old int
	frame_sw  time.StopWatch = time.new_stopwatch({})
	second_sw time.StopWatch = time.new_stopwatch({})
}

struct Pos {
	x int = -1
	y int = -1
}

struct Board {
mut:
	field  [4][4]int
	points int
	shifts int
}

struct TileLine {
	ypos   int
mut:
	field  [5]int
	points int
	shifts int
}

struct TouchInfo {
mut:
	start_pos Pos
}

enum TileFormat {
	normal
	log
	exponent
	shifts
	none_
	end_ // To know when to wrap around
}

enum GameState {
	play
	over
	victory
}

enum LabelKind {
	points
	moves
	tile
	victory
	game_over
}

enum Direction {
	up
	down
	left
	right
}

// Utility functions, to avoid importing `math`
[inline]
fn min(a, b int) int {
	if a < b { return a } else { return b }
}

[inline]
fn max(a, b int) int {
	if a > b { return a } else { return b }
}

[inline]
fn abs(a int) int {
	if a < 0 { return -a } else { return a }
}

fn (b Board) transpose() Board {
	mut res := b
	for y in 0..4 {
		for x in 0..4 {
			res.field[y][x] = b.field[x][y]
		}
	}
	return res
}

fn (b Board) hmirror() Board {
	mut res := b
	for y in 0..4 {
		for x in 0..4 {
			res.field[y][x] = b.field[y][3 - x]
		}
	}
	return res
}

// GCC optimization bug; inlining fails when compiled with -prod
[no_inline]
fn (t TileLine) to_left() TileLine {
	right_border_idx := 5
	mut res := t
	mut zeros := 0
	mut nonzeros := 0
	// gather meta info about the line:
	for x in res.field {
		if x == 0 { zeros++ } else { nonzeros++ }
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
	for y in 0..4 {
		mut hline := TileLine{ypos: y}
		for x in 0..4 {
			hline.field[x] = b.field[y][x]
		}
		reshline := hline.to_left()
		res.shifts += reshline.shifts
		res.points += reshline.points
		for x in 0..4 {
			res.field[y][x] = reshline.field[x]
		}
	}
	return res
}

fn (mut app App) update_tickers() {
	for y in 0..4 {
		for x in 0..4 {
			mut old := app.atickers[y][x]
			if old > 0 {
				old--
				app.atickers[y][x] = old
			}
		}
	}
}

fn (mut app App) new_game() {
	app.board = Board{}
	for y in 0..4 {
		for x in 0..4 {
			app.board.field[y][x] = 0
			app.atickers[y][x] = 0
		}
	}
	app.state = .play
	app.undo = []
	app.moves = 0
	app.new_random_tile()
	app.new_random_tile()
}

fn (mut app App) check_for_victory() {
	for y in 0..4 {
		for x in 0..4 {
			fidx := app.board.field[y][x]
			if fidx == 11 {
				app.victory()
				return
			}
		}
	}
}

fn (mut app App) check_for_game_over() {
	mut zeros := 0
	mut remaining_merges := 0
	for y in 0..4 {
		for x in 0..4 {
			fidx := app.board.field[y][x]
			if fidx == 0 {
				zeros++
				continue
			}
			if (x > 0 && fidx == app.board.field[y][x - 1])
				|| (x < 4 - 1 && fidx == app.board.field[y][x + 1])
				|| (y > 0 && fidx == app.board.field[y - 1][x])
				|| (y < 4 - 1 && fidx == app.board.field[y + 1][x]) {
				remaining_merges++
			}
		}
	}
	if remaining_merges == 0 && zeros == 0 {
		app.game_over()
	}
}

fn (mut app App) new_random_tile() {
	mut etiles := [16]Pos{}
	mut empty_tiles_max := 0
	for y in 0..4 {
		for x in 0..4 {
			fidx := app.board.field[y][x]
			if fidx == 0 {
				etiles[empty_tiles_max] = Pos{x, y}
				app.atickers[y][x] = 0
				empty_tiles_max++
			}
		}
	}
	if empty_tiles_max > 0 {
		new_random_tile_index := rand.intn(empty_tiles_max)
		empty_pos := etiles[new_random_tile_index]
		// 1/8 chance of creating a `4` tile
		random_value := if rand.intn(8) == 0 { 2 } else { 1 }
		app.board.field[empty_pos.y][empty_pos.x] = random_value
		app.atickers[empty_pos.y][empty_pos.x] = animation_length
	}
	app.check_for_victory()
	app.check_for_game_over()
}

fn (mut app App) victory() {
	app.state = .victory
}

fn (mut app App) game_over() {
	app.state = .over
}

fn (mut app App) move(d Direction) {
	old := app.board
	new := match d {
		.left  { old.to_left() }
		.right { old.hmirror().to_left().hmirror() }
		.up    { old.transpose().to_left().transpose() }
		.down  { old.transpose().hmirror().to_left().hmirror().transpose() }
	}
	if old.shifts != new.shifts {
		app.moves++
		app.board = new
		app.undo << old
		app.new_random_tile()
	}
}

fn (app &App) label_format(kind LabelKind) gx.TextCfg {
	match kind {
		.points {
			return {
				color: app.theme.text_color
				align: .left
				size: app.ui.font_size / 2
			}
		} .moves {
			return {
				color: app.theme.text_color
				align: .right
				size: app.ui.font_size / 2
			}
		} .tile {
			return {
				color: app.theme.text_color
				align: .center
				vertical_align: .middle
				size: app.ui.font_size
			}
		} .victory {
			return {
				color: app.theme.victory_color
				align: .center
				vertical_align: .middle
				size: app.ui.font_size * 2
			}
		} .game_over {
			return {
				color: app.theme.game_over_color
				align: .center
				vertical_align: .middle
				size: app.ui.font_size * 2
			}
		}
	}
}

fn (mut app App) set_theme(idx int) {
	theme := themes[idx]
	app.theme_idx = idx
	app.theme = theme
	app.gg.set_bg_color(theme.bg_color)
}

fn (mut app App) resize() {
	mut s := sapp.dpi_scale() || 1
	w := int(sapp.width() / s)
	h := int(sapp.height() / s)
	m := f32(min(w, h))

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
	ww := app.ui.window_width
	wh := app.ui.window_height
	labelx := app.ui.x_padding + app.ui.border_size
	labely := app.ui.y_padding + app.ui.border_size / 2

	app.draw_tiles()
	app.gg.draw_text(labelx, labely, 'Points: $app.board.points', app.label_format(.points))
	app.gg.draw_text(ww - labelx, labely, 'Moves: $app.moves', app.label_format(.moves))
	
	// TODO: Make transparency work in `gg`
	if app.state == .over {
		app.gg.draw_rect(0, 0, ww, wh, gx.rgba(15, 0, 0, 44))
		app.gg.draw_text(ww / 2, wh / 2, 'Game Over', app.label_format(.game_over))
	}
	if app.state == .victory {
		app.gg.draw_rect(0, 0, ww, wh, gx.rgba(0, 15, 0, 44))
		app.gg.draw_text(ww / 2, wh / 2, 'Victory!', app.label_format(.victory))
	}
}

fn (app &App) draw_tiles() {
	xstart := app.ui.x_padding + app.ui.border_size
	ystart := app.ui.y_padding + app.ui.border_size + app.ui.header_size

	toffset := app.ui.tile_size + app.ui.padding_size
	tiles_size := min(app.ui.window_width, app.ui.window_height) - app.ui.border_size * 2

	// Draw the padding around the tiles
	app.gg.draw_rect(xstart, ystart, tiles_size, tiles_size, app.theme.padding_color)
	// Draw the actual tiles
	for y in 0..4 {
		for x in 0..4 {
			tidx := app.board.field[y][x]
			tile_color := if tidx < app.theme.tile_colors.len {
				app.theme.tile_colors[tidx]
			} else {
				// If there isn't a specific color for this tile, reuse the last color available
				app.theme.tile_colors.last()
			}
			anim_size := animation_length - app.atickers[y][x]
			tw := int(f64(app.ui.tile_size) / animation_length * anim_size)
			th := tw // square tiles, w == h
			xoffset := xstart + app.ui.padding_size + x * toffset + (app.ui.tile_size - tw) / 2
			yoffset := ystart + app.ui.padding_size + y * toffset + (app.ui.tile_size - th) / 2
			app.gg.draw_rect(xoffset, yoffset, tw, th, tile_color)
			
			if tidx != 0 { // 0 == blank spot
				xpos := xoffset + tw / 2
				ypos := yoffset + th / 2
				mut fmt := app.label_format(.tile)
				fmt = { fmt | size: int(f32(fmt.size - 1) / animation_length * anim_size) }
				
				match app.tile_format {
					.normal {
						app.gg.draw_text(xpos, ypos, '${1 << tidx}', fmt)
					} .log {
						app.gg.draw_text(xpos, ypos, '$tidx', fmt)
					} .exponent {
						app.gg.draw_text(xpos, ypos, '2', fmt)
						fs2 := int(f32(fmt.size) * 0.67)
						app.gg.draw_text(xpos + app.ui.tile_size / 10, ypos - app.ui.tile_size / 8,
							'$tidx', { fmt | size: fs2, align: gx.HorizontalAlign.left })
					} .shifts {
						fs2 := int(f32(fmt.size) * 0.6)
						app.gg.draw_text(xpos, ypos, '2<<${tidx - 1}', { fmt | size: fs2 })
					} .none_ {} // Don't draw any text here, colors only
					.end_ {} // Should never get here
				}
			}
		}
	}
}

fn (mut app App) handle_swipe(start, end Pos) {
	min_swipe_distance := min(app.ui.window_width, app.ui.window_height) / 10
	dx := end.x - start.x
	dy := end.y - start.y
	adx := abs(dx)
	ady := abs(dy)
	dmax := max(adx, ady)
	dmin := min(adx, ady)
	
	if dmax < min_swipe_distance { return } // Swipe was too short
	if dmax / dmin < 2 { return } // Swiped diagonally

	if adx > ady {
		if dx < 0 { app.move(.left) } else { app.move(.right) }
	} else {
		if dy < 0 { app.move(.up) } else { app.move(.down) }
	}
}

fn (mut app App) on_key_down(key sapp.KeyCode) {
	// these keys are independent from the game state:
	match key {
		.escape {
			exit(0)
		} .n {
			app.new_game()
		} .backspace {
			if app.undo.len > 0 {
				app.state = .play
				app.board = app.undo.pop()
				app.moves--
				return
			}
		} .enter {
			app.tile_format = int(app.tile_format) + 1
			if app.tile_format == .end_ {
				app.tile_format = .normal
			}
		} .j {
			app.game_over()
		} .t {
			app.set_theme(if app.theme_idx == themes.len - 1 { 0 } else { app.theme_idx + 1 })
		} else {}
	}

	if app.state == .play {
		match key {
			.w, .up    { app.move(.up) }
			.a, .left  { app.move(.left) }
			.s, .down  { app.move(.down) }
			.d, .right { app.move(.right) }
			else {}
		}
	}
}

fn on_event(e &sapp.Event, mut app App) {
	match e.typ {
		.key_down {
			app.on_key_down(e.key_code)
		} .resized, .restored, .resumed {
			app.resize()
		} .touches_began {
			if e.num_touches > 0 {
				t := e.touches[0]
				app.touch.start_pos = { x: int(t.pos_x), y: int(t.pos_y) }
			}
		} .touches_ended {
			if e.num_touches > 0 {
				t := e.touches[0]
				end_pos := Pos{ x: int(t.pos_x), y: int(t.pos_y) }
				app.handle_swipe(app.touch.start_pos, end_pos)
			}
		} else {}
	}
}


fn frame(mut app App) {
	$if showfps? { app.perf.frame_sw.restart() }
	app.gg.begin()
	app.update_tickers()
	app.draw()
	app.gg.end()
	$if showfps? { app.showfps() }
}

fn init(mut app App) {
	app.resize()
	$if showfps? {
		app.perf.frame_sw.restart()
		app.perf.second_sw.restart()
	}
}

fn (mut app App) showfps() {
	println(app.perf.frame_sw.elapsed().microseconds())
	app.perf.frame++
	f := app.perf.frame
	if (f & 127) == 0 {
		last_frame_us := app.perf.frame_sw.elapsed().microseconds()
		ticks := f64(app.perf.second_sw.elapsed().milliseconds())
		fps := f64(app.perf.frame - app.perf.frame_old) * ticks / 1000 / 4.5
		last_fps := 128_000.0 / ticks
		eprintln('frame ${f:-5} | avg. fps: ${fps:-5.1f} | avg. last 128 fps: ${last_fps:-5.1f} | last frame time: ${last_frame_us:-4}µs')
		app.perf.second_sw.restart()
		app.perf.frame_old = f
	}
}

// TODO: Move this somewhere else (vlib?) once Android support is merged
$if android {
	#include <android/log.h>
	#define LOG_TAG "v_logcat_test"
	#define printf(...) __android_log_print(ANDROID_LOG_INFO, LOG_TAG, __VA_ARGS__)
	#define fprintf(a, ...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)
}

fn main() {
	mut app := &App{}
	app.new_game()

	mut font_path := os.resource_abs_path(os.join_path('../assets/fonts/', 'RobotoMono-Regular.ttf'))
	$if android {
		font_path = 'assets/RobotoMono-Regular.ttf'
	}

	mut window_title := 'V 2048'
	// TODO: Make emcc a real platform ifdef
	$if emscripten? {
		// in emscripten, sokol uses `window_title` as the selector to the canvas it'll render to,
		// and since `document.querySelector('V 2048')` isn't valid JS, we use `canvas` instead
		window_title = 'canvas'
	}

	$if showfps? {
		app.perf = &Perf{}
	}

	app.gg = gg.new_context({
		bg_color: app.theme.bg_color
		width: default_window_width
		height: default_window_height
		create_window: true
		window_title: window_title
		frame_fn: frame
		event_fn: on_event
		init_fn: init
		user_data: app
		font_path: font_path
	})
	app.gg.run()
}