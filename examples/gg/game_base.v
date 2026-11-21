module main

// A game starter: fixed timestep, input edges, tilemap collision, A* path and
// a frame stats readout. Run with: v run examples/gg/game_base.v
import gg
import gg.game
import time

const tile = f32(32)

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	input   game.InputState
	player  gg.Rect
	map     game.TileMap
	stats   game.FrameStats
	step    game.FixedStep
	last    time.Time
	path    []game.Cell
	message string
}

fn build_map() game.TileMap {
	mut tiles := []int{len: 12 * 8}
	for x in 0 .. 12 {
		tiles[x] = 1
		tiles[7 * 12 + x] = 1
	}
	for y in 0 .. 8 {
		tiles[y * 12] = 1
		tiles[y * 12 + 11] = 1
	}
	for y in 2 .. 6 {
		tiles[y * 12 + 5] = 2
	}
	return game.new_tilemap(tiles, 12, 8, tile)
}

fn main() {
	mut app := &App{
		player: gg.Rect{tile * 2, tile * 2, 24, 24}
		map:    build_map()
		step:   game.new_fixed_step(1.0 / 60)
	}
	app.input.bind('left', [gg.KeyCode.left, gg.KeyCode.a])
	app.input.bind('right', [gg.KeyCode.right, gg.KeyCode.d])
	app.input.bind('up', [gg.KeyCode.up, gg.KeyCode.w])
	app.input.bind('down', [gg.KeyCode.down, gg.KeyCode.s])
	grid := game.grid_from_tilemap(app.map)
	app.path = grid.find_path(game.Cell{1, 1}, game.Cell{10, 6})
	app.last = time.now()

	app.ctx = gg.new_context(
		width:        384
		height:       256
		window_title: 'gg game base'
		bg_color:     gg.rgb(20, 22, 30)
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.ctx.run()
}

fn on_event(e &gg.Event, mut app App) {
	if e.typ == .key_down {
		app.input.on_key_down(e.key_code)
		if e.key_code == .space {
			app.message = 'jump!'
		}
	}
	if e.typ == .key_up {
		app.input.on_key_up(e.key_code)
	}
	if e.typ == .mouse_down {
		app.input.on_mouse_button(e.mouse_button, true, e.mouse_x, e.mouse_y)
	}
	if e.typ == .mouse_up {
		app.input.on_mouse_button(e.mouse_button, false, e.mouse_x, e.mouse_y)
	}
	if e.typ == .mouse_move {
		app.input.on_mouse_move(e.mouse_x, e.mouse_y)
	}
}

fn frame(mut app App) {
	now := time.now()
	mut dt := f32((now - app.last).seconds())
	app.last = now
	if dt > 0.1 {
		dt = 0.1
	}
	app.stats.update(dt)

	steps := app.step.advance(dt)
	for _ in 0 .. steps {
		mut dx := f32(0)
		mut dy := f32(0)
		if app.input.action_down('left') {
			dx -= 1
		}
		if app.input.action_down('right') {
			dx += 1
		}
		if app.input.action_down('up') {
			dy -= 1
		}
		if app.input.action_down('down') {
			dy += 1
		}
		if dx != 0 || dy != 0 {
			old_x := app.player.x
			old_y := app.player.y
			app.player.x += dx * 140 * app.step.step
			app.player.y += dy * 140 * app.step.step
			if app.map.collides_rect(app.player) {
				app.player.x = old_x
				app.player.y = old_y
			}
		}
	}
	app.input.end_frame()

	app.ctx.begin()
	app.map.draw(app.ctx, [gg.rgb(56, 84, 116), gg.rgb(122, 86, 62)])
	for cell in app.path {
		app.ctx.draw_circle_filled(f32(cell.x) * tile + tile / 2, f32(cell.y) * tile + tile / 2, 3,
			gg.rgb(70, 200, 150))
	}
	app.ctx.draw_rect_filled(app.player.x, app.player.y, app.player.width, app.player.height,
		gg.yellow)
	app.ctx.draw_text_def(6, 6, 'fps: ${int(app.stats.fps)}  wasd/arrows to move')
	if app.message != '' {
		app.ctx.draw_text_def(6, 22, app.message)
	}
	app.ctx.end()
}
