import gg

struct App {
mut:
	x f64 = 100.0
	y f64 = 100.0
}

mut app := &App{}
gg.start(
	window_title: 'Moving Square'
	width:        640
	height:       480
	update_fn:    fn [mut app] (dt f32, ctx &gg.Context) {
		println(' frame: ${ctx.frame:6} | dt: ${dt:9.6f}s')
		if ctx.pressed_keys[gg.KeyCode.right] {
			app.x = app.x + 200 * dt
		}
		if ctx.pressed_keys[gg.KeyCode.left] {
			app.x = app.x - 200 * dt
		}
		if ctx.pressed_keys[gg.KeyCode.down] {
			app.y = app.y + 200 * dt
		}
		if ctx.pressed_keys[gg.KeyCode.up] {
			app.y = app.y - 200 * dt
		}
	}
	frame_fn:     fn [mut app] (ctx &gg.Context) {
		ctx.begin()
		ctx.draw_rect_filled(int(app.x), int(app.y), 50, 50, gg.red)
		ctx.end()
	}
)
