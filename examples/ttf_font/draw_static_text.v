import gg
import gx
import sokol.sapp
import sokol.sgl
import x.ttf
import os

const custom_font_path = os.args[1] or {
	os.resource_abs_path(os.join_path('..', 'assets', 'fonts', 'Imprima-Regular.ttf'))
}
const custom_txt_path = os.args[2] or { os.resource_abs_path('draw_static_text.txt') }

const custom_text_start_y = 80

const win_width = 400
const win_height = 400
const bg_color = gx.Color{50, 255, 50, 255}

const block_txt = os.read_file(custom_txt_path) or { '' }
const font_paths = [
	os.resource_abs_path(os.join_path('..', 'assets', 'fonts', 'RobotoMono-Regular.ttf')),
	custom_font_path,
]

// UI
struct App_data {
pub mut:
	gg              &gg.Context = unsafe { nil }
	init_flag       bool
	tf              []ttf.TTF_File
	ttf_render      []ttf.TTF_render_Sokol
	text_ready_flag bool
}

fn my_init(mut app App_data) {
	app.init_flag = true
	texts := ['Hello, font: ${os.file_name(custom_font_path)}', block_txt]!
	dump(texts[0])
	for i in 0 .. 2 {
		mut txt := unsafe { &app.ttf_render[i] }
		txt.destroy_texture()
		txt.create_text_block(texts[i], 1024, 1024, 24)
		txt.create_texture()
	}
}

fn draw_frame(mut app App_data) {
	app.gg.begin()
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
	sgl.c4b(0, 0, 0, 255) // black
	// draw text only if the app is already initialized
	if app.init_flag != true {
		app.gg.end()
	}

	// draw hello
	mut txt1 := unsafe { &app.ttf_render[0] }
	txt1.draw_text_bmp(app.gg, 5, 5)

	// draw the custom text
	txt2 := unsafe { &app.ttf_render[1] }
	txt2.draw_text_bmp(app.gg, 30, custom_text_start_y)
	app.gg.end()
}

fn main() {
	println('Use `v run draw_static_text.v [FONT_PATH] [TEXT_FILE_PATH]`')
	println('> Current command: os.args[0] ${custom_font_path} ${custom_txt_path}')
	mut app := &App_data{}
	app.gg = gg.new_context(
		width:        win_width
		height:       win_height
		window_title: 'Draw custom text, with custom font'
		user_data:    app
		bg_color:     bg_color
		frame_fn:     draw_frame
		init_fn:      my_init
	)

	// load the TTF fonts:
	for font_path in font_paths {
		mut tf := ttf.TTF_File{}
		tf.buf = os.read_bytes(font_path) or { panic(err) }
		println('Read TrueTypeFont file [${font_path}], len: ${tf.buf.len}')
		tf.init()
		println('Unit per EM: ${tf.units_per_em}')
		app.tf << tf
	}

	// TTF hello render
	app.ttf_render << &ttf.TTF_render_Sokol{
		bmp: &ttf.BitMap{
			tf:       &app.tf[0]
			buf:      unsafe { malloc_noscan(32000000) }
			buf_size: (32000000)
			color:    0xFF0000FF
		}
	}

	// TTF custom text render
	app.ttf_render << &ttf.TTF_render_Sokol{
		bmp: &ttf.BitMap{
			tf:       &app.tf[1]
			buf:      unsafe { malloc_noscan(32000000) }
			buf_size: (32000000)
			color:    0x2020EEFF
		}
	}

	// setup sokol_gfx
	app.gg.run()
}
