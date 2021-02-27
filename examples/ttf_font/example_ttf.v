import gg
import gx
import sokol.sapp
import sokol.sgl
import x.ttf
import os

// import math
const (
	win_width  = 600
	win_height = 700
	bg_color   = gx.white
	font_paths = [
		os.resource_abs_path('Imprima-Regular.ttf'),
		os.resource_abs_path('Graduate-Regular.ttf'),
	]
)

// UI
struct App_data {
pub mut:
	gg              &gg.Context
	sg_img          C.sg_image
	init_flag       bool
	frame_c         int
	tf              []ttf.TTF_File
	ttf_render      []ttf.TTF_render_Sokol
	text_ready_flag bool
	mouse_x         int = -1
	mouse_y         int = -1
}

fn my_init(mut app App_data) {
	app.init_flag = true
}

fn draw_frame(mut app App_data) {
	cframe_txt := 'Current Frame: $app.frame_c'
	app.gg.begin()
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
	sgl.c4b(0, 0, 0, 255) // black
	// draw a line as background
	sgl.begin_line_strip()
	sgl.v2f(10, 10)
	sgl.v2f(100, 100)
	sgl.end()
	// draw text only if the app is already initialized
	if app.init_flag == true {
		sgl.begin_line_strip()
		sgl.v2f(410, 400)
		sgl.v2f(510, 400)
		sgl.end()
		// update the text
		mut txt1 := &app.ttf_render[0]
		if app.frame_c % 2 == 0 {
			txt1.destroy_texture()
			txt1.create_text(cframe_txt, 43)
			txt1.create_texture()
		}
		// ----- decomment if you want text rotation ----
		// txt1.bmp.angle = 3.141592 / 180 * f32(app.frame_c % 360)
		// txt1.draw_text_bmp(app.gg, 300, 350)
		// txt1.bmp.angle =  0
		txt1.draw_text_bmp(app.gg, 30, 60)
		// block test
		block_txt := "Today it is a good day!
Tommorow I'm not so sure :(
Frame: $app.frame_c
But Vwill prevail for sure, V is the way!!
òàèì@ò!£$%&
"
		txt1 = &app.ttf_render[1]
		if app.frame_c % 2 == 0 {
			txt1.bmp.justify = false
			if (app.frame_c >> 6) % 2 == 0 {
				// txt1.align = .left
				txt1.bmp.justify = true
			}
			txt1.bmp.align = .left
			if (app.frame_c >> 6) % 3 == 0 {
				txt1.bmp.align = .right
			}
			txt1.destroy_texture()
			txt1.create_text_block(block_txt, 500, 500, 32)
			txt1.create_texture()
		}
		// decomment if want block color change
		// txt1.bmp.color = ttf.color_multiply(0xFF00FFFF, f32(app.frame_c % 255)/255.0)
		// decomment if want block rotation wanted
		// txt1.bmp.angle = 3.141592/180 * f32(app.frame_c % 45)
		txt1.draw_text_bmp(app.gg, 30 + (app.frame_c >> 1) & 0xFF, 200)
		// draw mouse position
		if app.mouse_x >= 0 {
			txt1 = &app.ttf_render[2]
			txt1.destroy_texture()
			txt1.create_text('$app.mouse_x,$app.mouse_y', 25)
			txt1.create_texture()
			r := app.mouse_x % 255
			g := app.mouse_y % 255
			color := u32(r << 24) | u32(g << 16) | 0xFF
			txt1.bmp.color = color
			txt1.draw_text_bmp(app.gg, app.mouse_x, app.mouse_y)
		}
		app.frame_c++
	}
	app.gg.end()
}

fn my_event_manager(mut ev gg.Event, mut app App_data) {
	if ev.typ == .mouse_move {
		app.mouse_x = int(ev.mouse_x)
		app.mouse_y = int(ev.mouse_y)
	}
}

[console]
fn main() {
	mut app := &App_data{
		gg: 0
	}
	app.gg = gg.new_context(
		width: win_width
		height: win_height
		use_ortho: true // This is needed for 2D drawing
		create_window: true
		window_title: 'Test TTF module'
		user_data: app
		bg_color: bg_color
		frame_fn: draw_frame
		event_fn: my_event_manager
		init_fn: my_init
	)
	// load TTF fonts
	for font_path in font_paths {
		mut tf := ttf.TTF_File{}
		tf.buf = os.read_bytes(font_path) or { panic(err) }
		println('TrueTypeFont file [$font_path] len: $tf.buf.len')
		tf.init()
		println('Unit per EM: $tf.units_per_em')
		app.tf << tf
	}
	// TTF render 0 Frame counter
	app.ttf_render << &ttf.TTF_render_Sokol{
		bmp: &ttf.BitMap{
			tf: &(app.tf[0])
			buf: unsafe { malloc(32000000) }
			buf_size: (32000000)
			color: 0xFF0000FF
			// style: .raw
			// use_font_metrics: true
		}
	}
	// TTF render 1 Text Block
	app.ttf_render << &ttf.TTF_render_Sokol{
		bmp: &ttf.BitMap{
			tf: &(app.tf[1])
			// color : 0xFF0000_10
			// style: .raw
			// use_font_metrics: true
		}
	}
	// TTF mouse position render
	app.ttf_render << &ttf.TTF_render_Sokol{
		bmp: &ttf.BitMap{
			tf: &(app.tf[0])
		}
	}
	// setup sokol_gfx
	app.gg.run()
}
