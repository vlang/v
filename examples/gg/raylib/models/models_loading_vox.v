module main

// PORT NOTE: gg has no VOX (MagicaVoxel) loader and no custom lighting shader
// API. Four placeholder voxel-style models are shown; LEFT click cycles them.
import gg

const vox_raywhite = gg.Color{245, 245, 245, 255}
const vox_blue = gg.Color{0, 121, 241, 255}
const vox_skyblue = gg.Color{135, 206, 235, 255}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	cam     gg.Camera3D
	current int
	names   []string
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{10, 10, 10}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.names = ['chr_knight', 'chr_sword', 'monu9', 'fez']
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.mouse_buttons.has(.left) {
		app.current = (app.current + 1) % app.names.len
	}
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	mut x := i32(0)
	for x < 3 {
		mut y := i32(0)
		for y < 3 {
			app.ctx.draw_cube(gg.Vec3{f32(x) - 1, 0.5 + f32(y) * 0.4, 0},
				gg.Vec3{0.4, 0.4, 0.4}, vox_skyblue)
			y++
		}
		x++
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_rect_filled(10, 40, 340, 70, vox_skyblue)
	app.ctx.draw_rect_empty(10, 40, 340, 70, vox_blue)
	app.ctx.draw_text(20, 50, '- MOUSE LEFT BUTTON: CYCLE VOX MODELS', size: 10, color: vox_blue)
	app.ctx.draw_text(20, 70, '- VOX loading unavailable in gg', size: 10, color: vox_blue)
	app.ctx.draw_text(10, 10, 'VOX model file (placeholder): ${app.names[app.current]}',
		size: 20, color: vox_gray())
	app.ctx.end()
}

fn vox_gray() gg.Color {
	return gg.Color{128, 128, 128, 255}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.free, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - loading vox'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     vox_raywhite
		depth:        true
	)
	app.ctx.run()
}
