// Demonstrates how raycasting works. The left side shows
// the 2D layout of the walls and player. The green lines
// represent the field of view of the player.
//
// The right side is a simple 3D projection of the field
// of view.
//
// There is no collision detection so yes, you can walk
// through walls.
//
// Watch https://www.youtube.com/watch?v=gYRrGTC7GtA to
// learn more on how this code works. There are some silly
// digressons in the video but the tech content is spot on.
import gg
import gx
import math

const player_size = 8
const player_move_delta = 10
const map_x_size = 8
const map_y_size = 8
const map_square = 64

struct App {
mut:
	ctx          &gg.Context = unsafe { nil }
	player_x     f32
	player_y     f32
	player_dx    f32
	player_dy    f32
	player_angle f32
	map          []int
}

fn main() {
	mut app := App{
		player_x: 230
		player_y: 320
		// each number represents an 8x8 square
		// 1 is a wall cube, 0 is empty space
		map: [
			// vfmt off
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 0, 0, 0, 0, 0, 0, 1,
			1, 0, 1, 1, 0, 0, 0, 1,
			1, 0, 1, 0, 0, 0, 0, 1,
			1, 0, 0, 0, 0, 0, 0, 1,
			1, 0, 0, 0, 0, 1, 0, 1,
			1, 0, 0, 0, 0, 0, 0, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			// vfmt on
		]
	}

	calc_deltas(mut app)

	app.ctx = gg.new_context(
		user_data:    &app
		window_title: 'Raycaster Demo'
		width:        1024
		height:       512
		bg_color:     gx.gray
		frame_fn:     draw
		event_fn:     handle_events
	)

	app.ctx.run()
}

fn draw(mut app App) {
	app.ctx.begin()
	draw_map_2d(app)
	draw_player(app)
	draw_rays_and_walls(app)
	draw_instructions(app)
	app.ctx.end()
}

fn draw_map_2d(app App) {
	for y := 0; y < map_y_size; y++ {
		for x := 0; x < map_x_size; x++ {
			color := if app.map[y * map_x_size + x] == 1 { gx.white } else { gx.black }
			app.ctx.draw_rect_filled(x * map_square, y * map_square, map_square - 1, map_square - 1,
				color)
		}
	}
}

fn draw_player(app App) {
	app.ctx.draw_rect_filled(app.player_x, app.player_y, player_size, player_size, gx.yellow)
	cx := app.player_x + player_size / 2
	cy := app.player_y + player_size / 2
	app.ctx.draw_line(cx, cy, cx + app.player_dx * 5, cy + app.player_dy * 5, gx.yellow)
}

fn draw_rays_and_walls(app App) {
	pi2 := math.pi / 2
	pi3 := 3 * math.pi / 2
	degree_radian := f32(0.0174533)
	max_depth_of_field := 8
	field_of_view := 60 // 60 degrees

	mut distance := f32(0)
	mut depth_of_field := 0
	mut ray_x := f32(0)
	mut ray_y := f32(0)
	mut offset_x := f32(0)
	mut offset_y := f32(0)
	mut map_x := 0
	mut map_y := 0
	mut map_pos := 0
	mut color := gx.red
	mut ray_angle := clamp_ray_angle(app.player_angle - degree_radian * field_of_view / 2)

	// each step = 1/2 degree
	steps := field_of_view * 2

	for step := 0; step < steps; step++ {
		// check horizontal lines
		mut hd := f32(max_int)
		mut hx := app.player_x
		mut hy := app.player_y
		depth_of_field = 0
		arc_tan := -1.0 / math.tanf(ray_angle)
		if ray_angle > math.pi { // looking up
			ray_y = f32(int(app.player_y) / map_square * map_square) - .0001
			ray_x = (app.player_y - ray_y) * arc_tan + app.player_x
			offset_y = -map_square
			offset_x = -offset_y * arc_tan
		} else if ray_angle < math.pi { // looking down
			ray_y = f32(int(app.player_y) / map_square * map_square + map_square)
			ray_x = (app.player_y - ray_y) * arc_tan + app.player_x
			offset_y = map_square
			offset_x = -offset_y * arc_tan
		} else if ray_angle == 0 || ray_angle == 2 * math.pi { // looking straight left/right
			ray_x = app.player_x
			ray_y = app.player_y
			depth_of_field = max_depth_of_field
		}
		for depth_of_field < max_depth_of_field {
			map_x = int(ray_x) / map_square
			map_y = int(ray_y) / map_square
			map_pos = map_y * map_x_size + map_x
			if app.map[map_pos] or { 0 } == 1 {
				// hit a wall
				hx = ray_x
				hy = ray_y
				hd = hypotenuse(app.player_x, app.player_y, hx, hy)
				depth_of_field = max_depth_of_field
			} else { // go to next line
				ray_x += offset_x
				ray_y += offset_y
				depth_of_field += 1
			}
		}
		// check vertical lines
		mut vd := f32(max_int)
		mut vx := app.player_x
		mut vy := app.player_y
		depth_of_field = 0
		neg_tan := -math.tanf(ray_angle)
		if ray_angle > pi2 && ray_angle < pi3 { // looking left
			ray_x = f32(int(app.player_x) / map_square * map_square) - .0001
			ray_y = (app.player_x - ray_x) * neg_tan + app.player_y
			offset_x = -map_square
			offset_y = -offset_x * neg_tan
		} else if ray_angle < pi2 || ray_angle > pi3 { // looking right
			ray_x = f32(int(app.player_x) / map_square * map_square + map_square)
			ray_y = (app.player_x - ray_x) * neg_tan + app.player_y
			offset_x = map_square
			offset_y = -offset_x * neg_tan
		} else if ray_angle == 0 || ray_angle == 2 * math.pi { // looking straight up/down
			ray_x = app.player_x
			ray_y = app.player_y
			depth_of_field = max_depth_of_field
		}
		for depth_of_field < max_depth_of_field {
			map_x = int(ray_x) / map_square
			map_y = int(ray_y) / map_square
			map_pos = map_y * map_x_size + map_x
			if app.map[map_pos] or { 0 } == 1 {
				// hit a wall
				vx = ray_x
				vy = ray_y
				vd = hypotenuse(app.player_x, app.player_y, vx, vy)
				depth_of_field = max_depth_of_field
			} else { // go to next line
				ray_x += offset_x
				ray_y += offset_y
				depth_of_field += 1
			}
		}
		// use the shorter of the horizontal and vertical distances to draw rays
		// use different colors for the two sides of the walls for lighting effect
		if vd < hd {
			ray_x = vx
			ray_y = vy
			distance = vd
			color = gx.rgb(0, 100, 0)
		} else if hd < vd {
			ray_x = hx
			ray_y = hy
			distance = hd
			color = gx.rgb(0, 120, 0)
		}
		// draw ray
		cx := app.player_x + player_size / 2
		cy := app.player_y + player_size / 2
		app.ctx.draw_line(cx, cy, ray_x, ray_y, gx.green)
		// draw wall section
		mut ca := clamp_ray_angle(app.player_angle - ray_angle)
		distance *= math.cosf(ca) // remove fish eye
		offset_3d_view := 530
		line_thickeness := 4
		max_wall_height := 320
		wall_height := math.min((map_square * max_wall_height) / distance, max_wall_height)
		wall_offset := max_wall_height / 2 - wall_height / 2
		app.ctx.draw_line_with_config(step * line_thickeness + offset_3d_view, wall_offset,
			step * line_thickeness + offset_3d_view, wall_offset + wall_height, gg.PenConfig{
			color:     color
			thickness: line_thickeness
		})
		// step to next ray angle
		ray_angle = clamp_ray_angle(ray_angle + degree_radian / 2)
	}
}

fn handle_events(event &gg.Event, mut app App) {
	if event.typ == .key_down {
		match event.key_code {
			.up {
				app.player_x += app.player_dx
				app.player_y += app.player_dy
			}
			.down {
				app.player_x -= app.player_dx
				app.player_y -= app.player_dy
			}
			.left {
				app.player_angle -= 0.1
				if app.player_angle < 0 {
					app.player_angle += 2 * math.pi
				}
				calc_deltas(mut app)
			}
			.right {
				app.player_angle += 0.1
				if app.player_angle > 2 * math.pi {
					app.player_angle -= 2 * math.pi
				}
				calc_deltas(mut app)
			}
			else {}
		}
	}
}

fn calc_deltas(mut app App) {
	app.player_dx = math.cosf(app.player_angle) * 5
	app.player_dy = math.sinf(app.player_angle) * 5
}

fn hypotenuse(ax f32, ay f32, bx f32, by f32) f32 {
	a2 := math.square(bx - ax)
	b2 := math.square(by - ay)
	return math.sqrtf(a2 + b2)
}

fn clamp_ray_angle(ra f32) f32 {
	return match true {
		ra < 0 { ra + 2 * math.pi }
		ra > 2 * math.pi { ra - 2 * math.pi }
		else { ra }
	}
}

fn draw_instructions(app App) {
	app.ctx.draw_text(700, app.ctx.height - 17, 'use arrow keys to move player')
}
