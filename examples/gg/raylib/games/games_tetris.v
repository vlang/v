module main

// raylib [games] classic - tetris, ported to gg.
// Random tetrominoes fall into a 12x20 well. LEFT/RIGHT slide the active piece,
// UP rotates it, DOWN makes it fall faster, P pauses and ENTER restarts when
// the stack reaches the top. Complete rows flash and are removed, scoring
// lines as they go.

// PORT NOTE: no external assets or audio are used by the original classic;
// the playfield, the incoming-piece preview and all text use gg primitives.
// The original's unused `level`/`fallingSpeed` variables are omitted.

import gg
import rand

const screen_width = 800
const screen_height = 450

const square_size = f32(20)
const grid_horizontal_size = 12
const grid_vertical_size = 20

const lateral_speed = 10
const turning_speed = 12
const fast_fall_await_counter = 30
const fading_time = 33

// GridSquare values from the original: EMPTY, MOVING, FULL, BLOCK, FADING.
const cell_empty = 0
const cell_moving = 1
const cell_full = 2
const cell_block = 3
const cell_fading = 4

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx                      &gg.Context = unsafe { nil }
	grid                     [grid_horizontal_size][grid_vertical_size]int
	piece                    [4][4]int
	incoming_piece           [4][4]int
	piece_position_x         int
	piece_position_y         int
	fading_color             gg.Color
	begin_play               bool
	piece_active             bool
	detection                bool
	line_to_delete           bool
	lines                    int
	gravity_movement_counter int
	lateral_movement_counter int
	turn_movement_counter    int
	fast_fall_counter        int
	fade_line_counter        int
	gravity_speed            int
	game_over                bool
	paused                   bool
	press_left               bool
	press_right              bool
	press_up                 bool
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn pad4(n int) string {
	mut s := n.str()
	for s.len < 4 {
		s = '0${s}'
	}
	return s
}

// grid_cell keeps rotation checks inside the fixed grid; the C version read
// past the array edge near the floor, which would panic in V.
fn grid_cell(app &App, x int, y int) int {
	if x < 0 || x >= grid_horizontal_size || y < 0 || y >= grid_vertical_size {
		return cell_block
	}
	return app.grid[x][y]
}

fn init_game(mut app &App) {
	app.lines = 0
	app.fading_color = gray
	app.piece_position_x = 0
	app.piece_position_y = 0
	app.paused = false
	app.begin_play = true
	app.piece_active = false
	app.detection = false
	app.line_to_delete = false
	app.gravity_movement_counter = 0
	app.lateral_movement_counter = 0
	app.turn_movement_counter = 0
	app.fast_fall_counter = 0
	app.fade_line_counter = 0
	app.gravity_speed = 30
	app.press_left = false
	app.press_right = false
	app.press_up = false

	for i in 0 .. grid_horizontal_size {
		for j in 0 .. grid_vertical_size {
			if j == grid_vertical_size - 1 || i == 0 || i == grid_horizontal_size - 1 {
				app.grid[i][j] = cell_block
			} else {
				app.grid[i][j] = cell_empty
			}
		}
	}
	for i in 0 .. 4 {
		for j in 0 .. 4 {
			app.incoming_piece[i][j] = cell_empty
		}
	}
}

fn get_random_piece(mut app &App) {
	r := rand_range(0, 6)
	for i in 0 .. 4 {
		for j in 0 .. 4 {
			app.incoming_piece[i][j] = cell_empty
		}
	}
	match r {
		0 {
			app.incoming_piece[1][1] = cell_moving
			app.incoming_piece[2][1] = cell_moving
			app.incoming_piece[1][2] = cell_moving
			app.incoming_piece[2][2] = cell_moving
		}
		1 {
			app.incoming_piece[1][0] = cell_moving
			app.incoming_piece[1][1] = cell_moving
			app.incoming_piece[1][2] = cell_moving
			app.incoming_piece[2][2] = cell_moving
		}
		2 {
			app.incoming_piece[1][2] = cell_moving
			app.incoming_piece[2][0] = cell_moving
			app.incoming_piece[2][1] = cell_moving
			app.incoming_piece[2][2] = cell_moving
		}
		3 {
			app.incoming_piece[0][1] = cell_moving
			app.incoming_piece[1][1] = cell_moving
			app.incoming_piece[2][1] = cell_moving
			app.incoming_piece[3][1] = cell_moving
		}
		4 {
			app.incoming_piece[1][0] = cell_moving
			app.incoming_piece[1][1] = cell_moving
			app.incoming_piece[1][2] = cell_moving
			app.incoming_piece[2][1] = cell_moving
		}
		5 {
			app.incoming_piece[1][1] = cell_moving
			app.incoming_piece[2][1] = cell_moving
			app.incoming_piece[2][2] = cell_moving
			app.incoming_piece[3][2] = cell_moving
		}
		6 {
			app.incoming_piece[1][2] = cell_moving
			app.incoming_piece[2][2] = cell_moving
			app.incoming_piece[2][1] = cell_moving
			app.incoming_piece[3][1] = cell_moving
		}
		else {}
	}
}

fn create_piece(mut app &App) bool {
	app.piece_position_x = int((grid_horizontal_size - 4) / 2)
	app.piece_position_y = 0
	if app.begin_play {
		get_random_piece(mut app)
		app.begin_play = false
	}
	for i in 0 .. 4 {
		for j in 0 .. 4 {
			app.piece[i][j] = app.incoming_piece[i][j]
		}
	}
	get_random_piece(mut app)
	for i := app.piece_position_x; i < app.piece_position_x + 4; i++ {
		for j in 0 .. 4 {
			if app.piece[i - app.piece_position_x][j] == cell_moving {
				app.grid[i][j] = cell_moving
			}
		}
	}
	return true
}

fn resolve_falling_movement(mut app &App) {
	if app.detection {
		for j := grid_vertical_size - 2; j >= 0; j-- {
			for i := 1; i < grid_horizontal_size - 1; i++ {
				if app.grid[i][j] == cell_moving {
					app.grid[i][j] = cell_full
					app.detection = false
					app.piece_active = false
				}
			}
		}
	} else {
		for j := grid_vertical_size - 2; j >= 0; j-- {
			for i := 1; i < grid_horizontal_size - 1; i++ {
				if app.grid[i][j] == cell_moving {
					app.grid[i][j + 1] = cell_moving
					app.grid[i][j] = cell_empty
				}
			}
		}
		app.piece_position_y++
	}
}

fn resolve_lateral_movement(mut app &App) bool {
	mut collision := false
	if app.ctx.pressed_keys[int(gg.KeyCode.left)] {
		for j := grid_vertical_size - 2; j >= 0; j-- {
			for i := 1; i < grid_horizontal_size - 1; i++ {
				if app.grid[i][j] == cell_moving {
					if i - 1 == 0 || app.grid[i - 1][j] == cell_full {
						collision = true
					}
				}
			}
		}
		if !collision {
			for j := grid_vertical_size - 2; j >= 0; j-- {
				for i := 1; i < grid_horizontal_size - 1; i++ {
					if app.grid[i][j] == cell_moving {
						app.grid[i - 1][j] = cell_moving
						app.grid[i][j] = cell_empty
					}
				}
			}
			app.piece_position_x--
		}
	} else if app.ctx.pressed_keys[int(gg.KeyCode.right)] {
		for j := grid_vertical_size - 2; j >= 0; j-- {
			for i := 1; i < grid_horizontal_size - 1; i++ {
				if app.grid[i][j] == cell_moving {
					if i + 1 == grid_horizontal_size - 1 || app.grid[i + 1][j] == cell_full {
						collision = true
					}
				}
			}
		}
		if !collision {
			for j := grid_vertical_size - 2; j >= 0; j-- {
				for i := grid_horizontal_size - 1; i >= 1; i-- {
					if app.grid[i][j] == cell_moving {
						app.grid[i + 1][j] = cell_moving
						app.grid[i][j] = cell_empty
					}
				}
			}
			app.piece_position_x++
		}
	}
	return collision
}

// The original rejects a rotation when any of a set of corner/edge cells is
// occupied. grid_cell() supplies BLOCK for out-of-range probes.
fn resolve_turn_movement(mut app &App) bool {
	if !app.ctx.pressed_keys[int(gg.KeyCode.up)] {
		return false
	}

	px := app.piece_position_x
	py := app.piece_position_y
	mut checker := false
	if grid_cell(app, px + 3, py) == cell_moving && grid_cell(app, px, py) != cell_empty
		&& grid_cell(app, px, py) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 3, py + 3) == cell_moving && grid_cell(app, px + 3, py) != cell_empty
		&& grid_cell(app, px + 3, py) != cell_moving {
		checker = true
	}
	if grid_cell(app, px, py + 3) == cell_moving && grid_cell(app, px + 3, py + 3) != cell_empty
		&& grid_cell(app, px + 3, py + 3) != cell_moving {
		checker = true
	}
	if grid_cell(app, px, py) == cell_moving && grid_cell(app, px, py + 3) != cell_empty
		&& grid_cell(app, px, py + 3) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 1, py) == cell_moving && grid_cell(app, px, py + 2) != cell_empty
		&& grid_cell(app, px, py + 2) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 3, py + 1) == cell_moving && grid_cell(app, px + 1, py) != cell_empty
		&& grid_cell(app, px + 1, py) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 2, py + 3) == cell_moving && grid_cell(app, px + 3, py + 1) != cell_empty
		&& grid_cell(app, px + 3, py + 1) != cell_moving {
		checker = true
	}
	if grid_cell(app, px, py + 2) == cell_moving && grid_cell(app, px + 2, py + 3) != cell_empty
		&& grid_cell(app, px + 2, py + 3) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 2, py) == cell_moving && grid_cell(app, px, py + 1) != cell_empty
		&& grid_cell(app, px, py + 1) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 3, py + 2) == cell_moving && grid_cell(app, px + 2, py) != cell_empty
		&& grid_cell(app, px + 2, py) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 1, py + 3) == cell_moving && grid_cell(app, px + 3, py + 2) != cell_empty
		&& grid_cell(app, px + 3, py + 2) != cell_moving {
		checker = true
	}
	if grid_cell(app, px, py + 1) == cell_moving && grid_cell(app, px + 1, py + 3) != cell_empty
		&& grid_cell(app, px + 1, py + 3) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 1, py + 1) == cell_moving && grid_cell(app, px + 1, py + 2) != cell_empty
		&& grid_cell(app, px + 1, py + 2) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 2, py + 1) == cell_moving && grid_cell(app, px + 1, py + 1) != cell_empty
		&& grid_cell(app, px + 1, py + 1) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 2, py + 2) == cell_moving && grid_cell(app, px + 2, py + 1) != cell_empty
		&& grid_cell(app, px + 2, py + 1) != cell_moving {
		checker = true
	}
	if grid_cell(app, px + 1, py + 2) == cell_moving && grid_cell(app, px + 2, py + 2) != cell_empty
		&& grid_cell(app, px + 2, py + 2) != cell_moving {
		checker = true
	}

	if !checker {
		mut aux := app.piece[0][0]
		app.piece[0][0] = app.piece[3][0]
		app.piece[3][0] = app.piece[3][3]
		app.piece[3][3] = app.piece[0][3]
		app.piece[0][3] = aux

		aux = app.piece[1][0]
		app.piece[1][0] = app.piece[3][1]
		app.piece[3][1] = app.piece[2][3]
		app.piece[2][3] = app.piece[0][2]
		app.piece[0][2] = aux

		aux = app.piece[2][0]
		app.piece[2][0] = app.piece[3][2]
		app.piece[3][2] = app.piece[1][3]
		app.piece[1][3] = app.piece[0][1]
		app.piece[0][1] = aux

		aux = app.piece[1][1]
		app.piece[1][1] = app.piece[2][1]
		app.piece[2][1] = app.piece[2][2]
		app.piece[2][2] = app.piece[1][2]
		app.piece[1][2] = aux
	}

	for j := grid_vertical_size - 2; j >= 0; j-- {
		for i := 1; i < grid_horizontal_size - 1; i++ {
			if app.grid[i][j] == cell_moving {
				app.grid[i][j] = cell_empty
			}
		}
	}
	for i := app.piece_position_x; i < app.piece_position_x + 4; i++ {
		for j := app.piece_position_y; j < app.piece_position_y + 4; j++ {
			if i >= 0 && i < grid_horizontal_size && j >= 0 && j < grid_vertical_size
				&& app.piece[i - app.piece_position_x][j - app.piece_position_y] == cell_moving {
				app.grid[i][j] = cell_moving
			}
		}
	}
	return true
}

fn check_detection(mut app &App) {
	for j := grid_vertical_size - 2; j >= 0; j-- {
		for i := 1; i < grid_horizontal_size - 1; i++ {
			if app.grid[i][j] == cell_moving
				&& (app.grid[i][j + 1] == cell_full || app.grid[i][j + 1] == cell_block) {
				app.detection = true
			}
		}
	}
}

fn check_completion(mut app &App) {
	mut is_empty_row_found := false
	for j := grid_vertical_size - 2; j >= 0 && !is_empty_row_found; j-- {
		mut calculator := 0
		for i := 1; i < grid_horizontal_size - 1; i++ {
			if app.grid[i][j] == cell_full {
				calculator++
			}
		}
		if calculator == grid_horizontal_size - 2 {
			app.line_to_delete = true
			for z := 1; z < grid_horizontal_size - 1; z++ {
				app.grid[z][j] = cell_fading
			}
		} else if calculator == 0 {
			is_empty_row_found = true
		}
	}
}

fn delete_complete_lines(mut app &App) int {
	mut deleted_lines := 0
	for j := grid_vertical_size - 2; j >= 0; j-- {
		for app.grid[1][j] == cell_fading {
			for i := 1; i < grid_horizontal_size - 1; i++ {
				app.grid[i][j] = cell_empty
			}
			for j2 := j - 1; j2 >= 0; j2-- {
				for i2 := 1; i2 < grid_horizontal_size - 1; i2++ {
					if app.grid[i2][j2] == cell_full {
						app.grid[i2][j2 + 1] = cell_full
						app.grid[i2][j2] = cell_empty
					} else if app.grid[i2][j2] == cell_fading {
						app.grid[i2][j2 + 1] = cell_fading
						app.grid[i2][j2] = cell_empty
					}
				}
			}
			deleted_lines++
		}
	}
	return deleted_lines
}

fn update(mut app &App) {
	if !app.game_over {
		if !app.paused {
			if !app.line_to_delete {
				if !app.piece_active {
					check_completion(mut app)
					app.piece_active = create_piece(mut app)
					app.fast_fall_counter = 0
				} else {
					app.fast_fall_counter++
					app.gravity_movement_counter++
					app.lateral_movement_counter++
					app.turn_movement_counter++

					// A fresh key press moves/rotates immediately.
					if app.press_left || app.press_right {
						app.lateral_movement_counter = lateral_speed
					}
					if app.press_up {
						app.turn_movement_counter = turning_speed
					}

					if app.ctx.pressed_keys[int(gg.KeyCode.down)]
						&& app.fast_fall_counter >= fast_fall_await_counter {
						app.gravity_movement_counter += app.gravity_speed
					}

					if app.gravity_movement_counter >= app.gravity_speed {
						check_detection(mut app)
						resolve_falling_movement(mut app)
						app.gravity_movement_counter = 0
					}
					if app.lateral_movement_counter >= lateral_speed {
						if !resolve_lateral_movement(mut app) {
							app.lateral_movement_counter = 0
						}
					}
					if app.turn_movement_counter >= turning_speed {
						if resolve_turn_movement(mut app) {
							app.turn_movement_counter = 0
						}
					}
				}

				for j := 0; j < 2; j++ {
					for i := 1; i < grid_horizontal_size - 1; i++ {
						if app.grid[i][j] == cell_full {
							app.game_over = true
						}
					}
				}
			} else {
				app.fade_line_counter++
				if app.fade_line_counter % 8 < 4 {
					app.fading_color = maroon
				} else {
					app.fading_color = gray
				}
				if app.fade_line_counter >= fading_time {
					deleted := delete_complete_lines(mut app)
					app.fade_line_counter = 0
					app.line_to_delete = false
					app.lines += deleted
				}
			}
		}
	}

	// One-shot presses are consumed once per frame.
	app.press_left = false
	app.press_right = false
	app.press_up = false
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	update(mut app)
}

fn centered_text(ctx &gg.Context, msg string, y int, size int, c gg.Color) {
	w := ctx.text_width_cfg(msg, size: size)
	ctx.draw_text(screen_width / 2 - w / 2, y, msg, size: size, color: c)
}

fn on_frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if !app.game_over {
		base_x := f32(screen_width / 2 - (grid_horizontal_size * int(square_size) / 2) - 50)
		base_y := f32(screen_height / 2 - ((grid_vertical_size - 1) * int(square_size) / 2) +
			int(square_size) * 2 - 50)

		for j in 0 .. grid_vertical_size {
			for i in 0 .. grid_horizontal_size {
				x := base_x + f32(i) * square_size
				y := base_y + f32(j) * square_size
				c := app.grid[i][j]
				if c == cell_empty {
					ctx.draw_rect_empty(x, y, square_size, square_size, light_gray)
				} else if c == cell_full {
					ctx.draw_rect_filled(x, y, square_size, square_size, gray)
				} else if c == cell_moving {
					ctx.draw_rect_filled(x, y, square_size, square_size, dark_gray)
				} else if c == cell_block {
					ctx.draw_rect_filled(x, y, square_size, square_size, light_gray)
				} else {
					ctx.draw_rect_filled(x, y, square_size, square_size, app.fading_color)
				}
			}
		}

		incoming_x := f32(500)
		incoming_y := f32(45)
		for j in 0 .. 4 {
			for i in 0 .. 4 {
				x := incoming_x + f32(i) * square_size
				y := incoming_y + f32(j) * square_size
				if app.incoming_piece[i][j] == cell_empty {
					ctx.draw_rect_empty(x, y, square_size, square_size, light_gray)
				} else {
					ctx.draw_rect_filled(x, y, square_size, square_size, gray)
				}
			}
		}

		ctx.draw_text(500, 25, 'INCOMING:', size: 10, color: gray)
		ctx.draw_text(500, 145, 'LINES:      ${pad4(app.lines)}', size: 10, color: gray)
		if app.paused {
			centered_text(ctx, 'GAME PAUSED', screen_height / 2 - 40, 40, gray)
		}
	} else {
		centered_text(ctx, 'PRESS [ENTER] TO PLAY AGAIN', screen_height / 2 - 50, 20, gray)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && !e.key_repeat {
		match e.key_code {
			.p {
				if !app.game_over {
					app.paused = !app.paused
				}
			}
			.enter {
				if app.game_over {
					init_game(mut app)
					app.game_over = false
				}
			}
			.left {
				app.press_left = true
			}
			.right {
				app.press_right = true
			}
			.up {
				app.press_up = true
			}
			else {}
		}
	}
}

fn main() {
	mut app := &App{}
	init_game(mut app)
	app.ctx = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [games] classic - tetris'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
