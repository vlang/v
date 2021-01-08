import sokol
import sokol.sapp
import sokol.gfx
import sokol.sgl
import sokol.sfons
import os

struct AppState {
mut:
	pass_action C.sg_pass_action
	fons        &C.FONScontext
	font_normal int
}

fn main() {
	mut color_action := C.sg_color_attachment_action{
		action: gfx.Action(C.SG_ACTION_CLEAR)
	}
	color_action.val[0] = 0.3
	color_action.val[1] = 0.3
	color_action.val[2] = 0.32
	color_action.val[3] = 1.0
	mut pass_action := C.sg_pass_action{}
	pass_action.colors[0] = color_action
	state := &AppState{
		pass_action: pass_action
		fons: &C.FONScontext(0)
	}
	title := 'V Metal/GL Text Rendering'
	desc := C.sapp_desc{
		user_data: state
		init_userdata_cb: init
		frame_userdata_cb: frame
		window_title: title.str
		html5_canvas_name: title.str
	}
	sapp.run(&desc)
}

fn init(mut state AppState) {
	desc := sapp.create_desc()
	gfx.setup(&desc)
	s := &C.sgl_desc_t{}
	C.sgl_setup(s)
	state.fons = sfons.create(512, 512, 1)
	// or use DroidSerif-Regular.ttf
	if bytes := os.read_bytes(os.resource_abs_path('../assets/fonts/RobotoMono-Regular.ttf')) {
		println('loaded font: $bytes.len')
		state.font_normal = C.fonsAddFontMem(state.fons, 'sans', bytes.data, bytes.len, false)
	}
}

fn frame(user_data voidptr) {
	mut state := &AppState(user_data)
	state.render_font()
	gfx.begin_default_pass(&state.pass_action, sapp.width(), sapp.height())
	sgl.draw()
	gfx.end_pass()
	gfx.commit()
}

fn (state &AppState) render_font() {
	mut sx := 0.0
	mut sy := 0.0
	mut dx := 0.0
	mut dy := 0.0
	lh := f32(0.0)
	white := C.sfons_rgba(255, 255, 255, 255)
	black := C.sfons_rgba(0, 0, 0, 255)
	brown := C.sfons_rgba(192, 128, 0, 128)
	blue := C.sfons_rgba(0, 192, 255, 255)
	state.fons.clear_state()
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(C.sapp_width()), f32(C.sapp_height()), 0.0, -1.0, 1.0)
	sx = 0
	sy = 50
	dx = sx
	dy = sy
	state.fons.set_font(state.font_normal)
	state.fons.set_size(100.0)
	ascender := f32(0.0)
	descender := f32(0.0)
	state.fons.vert_metrics(&ascender, &descender, &lh)
	dx = sx
	dy += lh
	C.fonsSetColor(state.fons, white)
	dx = C.fonsDrawText(state.fons, dx, dy, c'The quick ', C.NULL)
	C.fonsSetFont(state.fons, state.font_normal)
	C.fonsSetSize(state.fons, 48.0)
	C.fonsSetColor(state.fons, brown)
	dx = C.fonsDrawText(state.fons, dx, dy, c'brown ', C.NULL)
	C.fonsSetFont(state.fons, state.font_normal)
	C.fonsSetSize(state.fons, 24.0)
	C.fonsSetColor(state.fons, white)
	dx = C.fonsDrawText(state.fons, dx, dy, c'fox ', C.NULL)
	dx = sx
	dy += lh * 1.2
	C.fonsSetSize(state.fons, 20.0)
	C.fonsSetFont(state.fons, state.font_normal)
	C.fonsSetColor(state.fons, blue)
	C.fonsDrawText(state.fons, dx, dy, c'Now is the time for all good men to come to the aid of the party.', C.NULL)
	dx = 300
	dy = 350
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_BASELINE)
	C.fonsSetSize(state.fons, 60.0)
	C.fonsSetFont(state.fons, state.font_normal)
	C.fonsSetColor(state.fons, white)
	C.fonsSetSpacing(state.fons, 5.0)
	C.fonsSetBlur(state.fons, 6.0)
	C.fonsDrawText(state.fons, dx, dy, c'Blurry...', C.NULL)
	dx = 300
	dy += 50.0
	C.fonsSetSize(state.fons, 28.0)
	C.fonsSetFont(state.fons, state.font_normal)
	C.fonsSetColor(state.fons, white)
	C.fonsSetSpacing(state.fons, 0.0)
	C.fonsSetBlur(state.fons, 3.0)
	C.fonsDrawText(state.fons, dx, dy + 2, c'DROP SHADOW', C.NULL)
	C.fonsSetColor(state.fons, black)
	C.fonsSetBlur(state.fons, 0)
	C.fonsDrawText(state.fons, dx, dy, c'DROP SHADOW', C.NULL)
	C.fonsSetSize(state.fons, 18.0)
	C.fonsSetFont(state.fons, state.font_normal)
	C.fonsSetColor(state.fons, white)
	dx = 50
	dy = 350
	line(f32(dx - 10), f32(dy), f32(dx + 250), f32(dy))
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_TOP)
	dx = C.fonsDrawText(state.fons, dx, dy, c'Top', C.NULL)
	dx += 10
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_MIDDLE)
	dx = C.fonsDrawText(state.fons, dx, dy, c'Middle', C.NULL)
	dx += 10
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_BASELINE)
	dx = C.fonsDrawText(state.fons, dx, dy, c'Baseline', C.NULL)
	dx += 10
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_BOTTOM)
	C.fonsDrawText(state.fons, dx, dy, c'Bottom', C.NULL)
	dx = 150
	dy = 400
	line(f32(dx), f32(dy - 30), f32(dx), f32(dy + 80.0))
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_BASELINE)
	C.fonsDrawText(state.fons, dx, dy, c'Left', C.NULL)
	dy += 30
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_CENTER | C.FONS_ALIGN_BASELINE)
	C.fonsDrawText(state.fons, dx, dy, c'Center', C.NULL)
	dy += 30
	C.fonsSetAlign(state.fons, C.FONS_ALIGN_RIGHT | C.FONS_ALIGN_BASELINE)
	C.fonsDrawText(state.fons, dx, dy, c'Right', C.NULL)
	C.sfons_flush(state.fons)
}

fn line(sx f32, sy f32, ex f32, ey f32) {
	sgl.begin_lines()
	sgl.c4b(255, 255, 0, 128)
	sgl.v2f(sx, sy)
	sgl.v2f(ex, ey)
	sgl.end()
}
