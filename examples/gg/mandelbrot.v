import gg
import gx
import runtime
import time

const pwidth = 800

const pheight = 600

const chunk_height = 2 // the image is recalculated in chunks, each chunk processed in a separate thread

const zoom_factor = 1.1

const max_iterations = 255

struct ViewRect {
mut:
	x_min f64
	x_max f64
	y_min f64
	y_max f64
}

fn (v &ViewRect) width() f64 {
	return v.x_max - v.x_min
}

fn (v &ViewRect) height() f64 {
	return v.y_max - v.y_min
}

struct AppState {
mut:
	gg      &gg.Context = unsafe { nil }
	iidx    int
	pixels  &u32     = unsafe { vcalloc(pwidth * pheight * sizeof(u32)) }
	npixels &u32     = unsafe { vcalloc(pwidth * pheight * sizeof(u32)) } // all drawing happens here, results are swapped at the end
	view    ViewRect = ViewRect{-3.0773593290970673, 1.4952456603855397, -2.019938598189011, 2.3106642054225945}
	scale   int      = 1
	ntasks  int      = runtime.nr_jobs()
}

const colors = [gx.black, gx.blue, gx.red, gx.green, gx.yellow, gx.orange, gx.purple, gx.white,
	gx.indigo, gx.violet, gx.black, gx.blue, gx.orange, gx.yellow, gx.green].map(u32(it.abgr8()))

struct MandelChunk {
	cview ViewRect
	ymin  f64
	ymax  f64
}

fn (mut state AppState) update() {
	mut chunk_channel := chan MandelChunk{cap: state.ntasks}
	mut chunk_ready_channel := chan bool{cap: 1000}
	mut threads := []thread{cap: state.ntasks}
	defer {
		chunk_channel.close()
		threads.wait()
	}
	for t in 0 .. state.ntasks {
		threads << spawn state.worker(t, chunk_channel, chunk_ready_channel)
	}
	//
	mut oview := ViewRect{}
	mut sw := time.new_stopwatch()
	for {
		sw.restart()
		cview := state.view
		if oview == cview {
			time.sleep(5 * time.millisecond)
			continue
		}
		// schedule chunks, describing the work:
		mut nchunks := 0
		for start := 0; start < pheight; start += chunk_height {
			chunk_channel <- MandelChunk{
				cview: cview
				ymin: start
				ymax: start + chunk_height
			}
			nchunks++
		}
		// wait for all chunks to be processed:
		for _ in 0 .. nchunks {
			_ := <-chunk_ready_channel
		}
		// everything is done, swap the buffer pointers
		state.pixels, state.npixels = state.npixels, state.pixels
		println('${state.ntasks:2} threads; ${sw.elapsed().milliseconds():3} ms / frame; scale: ${state.scale:4}')
		oview = cview
	}
}

[direct_array_access]
fn (mut state AppState) worker(id int, input chan MandelChunk, ready chan bool) {
	for {
		chunk := <-input or { break }
		yscale := chunk.cview.height() / pheight
		xscale := chunk.cview.width() / pwidth
		mut x, mut y, mut iter := 0.0, 0.0, 0
		mut y0 := chunk.ymin * yscale + chunk.cview.y_min
		mut x0 := chunk.cview.x_min
		for y_pixel := chunk.ymin; y_pixel < chunk.ymax && y_pixel < pheight; y_pixel++ {
			yrow := unsafe { &state.npixels[int(y_pixel * pwidth)] }
			y0 += yscale
			x0 = chunk.cview.x_min
			for x_pixel := 0; x_pixel < pwidth; x_pixel++ {
				x0 += xscale
				x, y = x0, y0
				for iter = 0; iter < max_iterations; iter++ {
					x, y = x * x - y * y + x0, 2 * x * y + y0
					if x * x + y * y > 4 {
						break
					}
				}
				unsafe {
					yrow[x_pixel] = colors[iter & 15]
				}
			}
		}
		ready <- true
	}
}

fn (mut state AppState) draw() {
	mut istream_image := state.gg.get_cached_image_by_idx(state.iidx)
	istream_image.update_pixel_data(state.pixels)
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
	state.scale += if zoom_factor < 1 { 1 } else { -1 }
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
		m_x := (x / size.width) * state.view.width() + state.view.x_min
		m_y := (y / size.height) * state.view.height() + state.view.y_min
		state.center(m_x, m_y)
	}
}

fn graphics_move(x f32, y f32, mut state AppState) {
	if state.gg.mouse_buttons.has(.left) {
		size := gg.window_size()
		d_x := (f64(state.gg.mouse_dx) / size.width) * state.view.width()
		d_y := (f64(state.gg.mouse_dy) / size.height) * state.view.height()
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
	s_x := state.view.width() / 5
	s_y := state.view.height() / 5
	// movement
	mut d_x, mut d_y := 0.0, 0.0
	if code == .enter {
		println('> ViewRect{$state.view.x_min, $state.view.x_max, $state.view.y_min, $state.view.y_max}')
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
	spawn state.update()
	state.gg.run()
}
