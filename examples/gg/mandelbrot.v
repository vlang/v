import gg
import runtime
import time

const pwidth = 800

const pheight = 600

const zoom_factor = 1.1

struct ViewRect {
mut:
	x_min f64
	x_max f64
	y_min f64
	y_max f64
}

struct AppState {
mut:
	gg      &gg.Context = 0
	iidx    int
	pixels  []u32    = []u32{len: pwidth * pheight}
	npixels []u32    = []u32{len: pwidth * pheight} // all drawing happens here, results are copied at the end
	view    ViewRect = ViewRect{-2.7610033817025625, 1.1788897130338223, -1.824584023871934, 2.1153096311072788}
	ntasks  int      = runtime.nr_jobs()
}

const colors = [gg.black, gg.blue, gg.red, gg.green, gg.yellow, gg.orange, gg.purple, gg.white,
	gg.indigo, gg.violet, gg.black]

fn (mut state AppState) update() {
	mut sw := time.new_stopwatch()
	mut oview := ViewRect{}
	for {
		sw.restart()
		cview := state.view
		if oview == cview {
			time.sleep(5 * time.millisecond)
			continue
		}
		sheight := pheight / state.ntasks
		mut threads := []thread{}
		for start := 0; start < pheight; start += sheight {
			threads << go state.recalc_lines(cview, start, start + sheight)
		}
		threads.wait()
		state.pixels = state.npixels
		println('$state.ntasks threads; $sw.elapsed().milliseconds() ms / frame')
		oview = cview
	}
}

fn (mut state AppState) recalc_lines(cview ViewRect, ymin f64, ymax f64) {
	for y_pixel := ymin; y_pixel < ymax && y_pixel < pheight; y_pixel++ {
		y0 := (y_pixel / pheight) * (cview.y_max - cview.y_min) + cview.y_min
		for x_pixel := 0.0; x_pixel < pwidth; x_pixel++ {
			x0 := (x_pixel / pwidth) * (cview.x_max - cview.x_min) + cview.x_min
			mut x, mut y := x0, y0
			mut iter := 0
			for ; iter < 80; iter++ {
				x, y = x * x - y * y + x0, 2 * x * y + y0
				if x * x + y * y > 4 {
					break
				}
			}
			state.npixels[int(y_pixel) * pwidth + int(x_pixel)] = u32(colors[iter % 8].abgr8())
		}
	}
}

fn (mut state AppState) draw() {
	mut istream_image := state.gg.get_cached_image_by_idx(state.iidx)
	istream_image.update_pixel_data(&state.pixels[0])
	size := gg.window_size()
	state.gg.draw_image(0, 0, size.width, size.height, istream_image)
}

fn (mut state AppState) zoom(zoom_factor f64) {
	c_x, c_y := (state.view.x_max + state.view.x_min) / 2, (state.view.y_max + state.view.y_min) / 2
	d_x, d_y := c_x - state.view.x_min, c_y - state.view.y_min
	state.view.x_min = c_x - zoom_factor * d_x
	state.view.x_max = c_x + zoom_factor * d_x
	state.view.y_min = c_y - zoom_factor * d_y
	state.view.y_max = c_y + zoom_factor * d_y
}

fn (mut state AppState) center(s_x f64, s_y f64) {
	c_x, c_y := (state.view.x_max + state.view.x_min) / 2, (state.view.y_max + state.view.y_min) / 2
	d_x, d_y := c_x - state.view.x_min, c_y - state.view.y_min
	state.view.x_min = s_x - d_x
	state.view.x_max = s_x + d_x
	state.view.y_min = s_y - d_y
	state.view.y_max = s_y + d_y
}

// gg callbacks:

fn graphics_init(mut state AppState) {
	state.iidx = state.gg.new_streaming_image(pwidth, pheight, 4, pixel_format: .rgba8)
}

fn graphics_frame(mut state AppState) {
	state.gg.begin()
	state.draw()
	state.gg.end()
}

fn graphics_click(x f32, y f32, btn gg.MouseButton, mut state AppState) {
	if btn == .right {
		size := gg.window_size()
		m_x := (x / size.width) * (state.view.x_max - state.view.x_min) + state.view.x_min
		m_y := (y / size.height) * (state.view.y_max - state.view.y_min) + state.view.y_min
		state.center(m_x, m_y)
	}
}

fn graphics_move(x f32, y f32, mut state AppState) {
	if state.gg.mouse_buttons.has(.left) {
		size := gg.window_size()
		d_x := (f64(state.gg.mouse_dx) / size.width) * (state.view.x_max - state.view.x_min)
		d_y := (f64(state.gg.mouse_dy) / size.height) * (state.view.y_max - state.view.y_min)
		state.view.x_min -= d_x
		state.view.x_max -= d_x
		state.view.y_min -= d_y
		state.view.y_max -= d_y
	}
}

fn graphics_scroll(e &gg.Event, mut state AppState) {
	state.zoom(if e.scroll_y < 0 { zoom_factor } else { 1 / zoom_factor })
}

fn graphics_keydown(code gg.KeyCode, mod gg.Modifier, mut state AppState) {
	s_x := (state.view.x_max - state.view.x_min) / 5
	s_y := (state.view.y_max - state.view.y_min) / 5
	// movement
	mut d_x, mut d_y := 0.0, 0.0
	if code == .enter {
		println('> $state.view.x_min | $state.view.x_max | $state.view.y_min | $state.view.y_max')
	}
	if state.gg.pressed_keys[int(gg.KeyCode.left)] {
		d_x -= s_x
	}
	if state.gg.pressed_keys[int(gg.KeyCode.right)] {
		d_x += s_x
	}
	if state.gg.pressed_keys[int(gg.KeyCode.up)] {
		d_y -= s_y
	}
	if state.gg.pressed_keys[int(gg.KeyCode.down)] {
		d_y += s_y
	}
	state.view.x_min += d_x
	state.view.x_max += d_x
	state.view.y_min += d_y
	state.view.y_max += d_y
	// zoom in/out
	if state.gg.pressed_keys[int(gg.KeyCode.left_bracket)]
		|| state.gg.pressed_keys[int(gg.KeyCode.z)] {
		state.zoom(1 / zoom_factor)
		return
	}
	if state.gg.pressed_keys[int(gg.KeyCode.right_bracket)]
		|| state.gg.pressed_keys[int(gg.KeyCode.x)] {
		state.zoom(zoom_factor)
		return
	}
}

[console]
fn main() {
	mut state := &AppState{}
	state.gg = gg.new_context(
		width: 800
		height: 600
		create_window: true
		window_title: 'The Mandelbrot Set'
		init_fn: graphics_init
		frame_fn: graphics_frame
		click_fn: graphics_click
		move_fn: graphics_move
		keydown_fn: graphics_keydown
		scroll_fn: graphics_scroll
		user_data: state
	)
	go state.update()
	state.gg.run()
}
