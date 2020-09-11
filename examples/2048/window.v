import gx
import sokol.sapp

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
