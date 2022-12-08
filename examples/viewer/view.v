/**********************************************************************
*
* simple Picture Viewer V. 0.9
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
* - add an example with shaders
**********************************************************************/
import os
import gg
import gx
import sokol.gfx
import sokol.sgl
import sokol.sapp
import stbi
import szip
import strings

// Help text
const (
	help_text_rows = [
		'Image Viwer 0.9 help.',
		'',
		'ESC/q - Quit',
		'cur. right - Next image',
		'cur. left  - Previous image',
		'cur. up    - Next folder',
		'cur. down  - Previous folder',
		'F - Toggle full screen',
		'R - Rotate image of 90 degree',
		'I - Toggle the info text',
		'',
		'mouse wheel - next/previous images',
		'keep pressed left  Mouse button - Pan on the image',
		'keep pressed rigth Mouse button - Zoom on the image',
	]
)

const (
	win_width       = 800
	win_height      = 800
	bg_color        = gx.black
	pi_2            = 3.14159265359 / 2.0
	uv              = [f32(0), 0, 1, 0, 1, 1, 0, 1]! // used for zoom icon during rotations

	text_drop_files = 'Drop here some images/folder/zip to navigate in the pics'
	text_scanning   = 'Scanning...'
	text_loading    = 'Loading...'
)

enum Viewer_state {
	loading
	scanning
	show
	error
}

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	pip_viewer  sgl.Pipeline
	texture     gfx.Image
	init_flag   bool
	frame_count int
	mouse_x     int = -1
	mouse_y     int = -1
	scroll_y    int

	state Viewer_state = .scanning
	// translation
	tr_flag   bool
	tr_x      f32 = 0.0
	tr_y      f32 = 0.0
	last_tr_x f32 = 0.0
	last_tr_y f32 = 0.0
	// scaling
	sc_flag   bool
	scale     f32 = 1.0
	sc_x      f32 = 0.0
	sc_y      f32 = 0.0
	last_sc_x f32 = 0.0
	last_sc_y f32 = 0.0
	// loaded image
	img_w     int
	img_h     int
	img_ratio f32 = 1.0
	// item list
	item_list &Item_list = unsafe { nil }
	// Text info and help
	show_info_flag bool = true
	show_help_flag bool
	// zip container
	zip       &szip.Zip = unsafe { nil } // pointer to the szip structure
	zip_index int       = -1 // index of the zip contaire item
	// memory buffer
	mem_buf      voidptr // buffer used to load items from files/containers
	mem_buf_size int     // size of the buffer
	// font
	font_path string // path to the temp font file
	// logo
	logo_path    string // path of the temp font logo
	logo_texture gfx.Image
	logo_w       int
	logo_h       int
	logo_ratio   f32 = 1.0
	// string builder
	bl strings.Builder = strings.new_builder(512)
}

/******************************************************************************
*
* Texture functions
*
******************************************************************************/
fn create_texture(w int, h int, buf &u8) gfx.Image {
	sz := w * h * 4
	mut img_desc := gfx.ImageDesc{
		width: w
		height: h
		num_mipmaps: 0
		min_filter: .linear
		mag_filter: .linear
		// usage: .dynamic
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: &u8(0)
		d3d11_texture: 0
	}
	// comment if .dynamic is enabled
	img_desc.data.subimage[0][0] = gfx.Range{
		ptr: buf
		size: usize(sz)
	}

	sg_img := gfx.make_image(&img_desc)
	return sg_img
}

fn destroy_texture(sg_img gfx.Image) {
	gfx.destroy_image(sg_img)
}

// Use only if: .dynamic is enabled
fn update_text_texture(sg_img gfx.Image, w int, h int, buf &u8) {
	sz := w * h * 4
	mut tmp_sbc := gfx.ImageData{}
	tmp_sbc.subimage[0][0] = gfx.Range{
		ptr: buf
		size: usize(sz)
	}
	gfx.update_image(sg_img, &tmp_sbc)
}

/******************************************************************************
*
* Memory buffer
*
******************************************************************************/
[inline]
fn (mut app App) resize_buf_if_needed(in_size int) {
	// manage the memory buffer
	if app.mem_buf_size < in_size {
		println('Managing FILE memory buffer, allocated [${in_size}]Bytes')
		// free previous buffer if any exist
		if app.mem_buf_size > 0 {
			unsafe {
				free(app.mem_buf)
			}
		}
		// allocate the memory
		unsafe {
			app.mem_buf = malloc(int(in_size))
			app.mem_buf_size = int(in_size)
		}
	}
}

/******************************************************************************
*
* Loading functions
*
******************************************************************************/
// read_bytes from file in `path` in the memory buffer of app.
[manualfree]
fn (mut app App) read_bytes(path string) bool {
	mut fp := os.vfopen(path, 'rb') or {
		eprintln('ERROR: Can not open the file [${path}].')
		return false
	}
	defer {
		C.fclose(fp)
	}
	cseek := C.fseek(fp, 0, C.SEEK_END)
	if cseek != 0 {
		eprintln('ERROR: Can not seek in the file [${path}].')
		return false
	}
	fsize := C.ftell(fp)
	if fsize < 0 {
		eprintln('ERROR: File [${path}] has size is 0.')
		return false
	}
	C.rewind(fp)

	app.resize_buf_if_needed(int(fsize))

	nr_read_elements := int(C.fread(app.mem_buf, fsize, 1, fp))
	if nr_read_elements == 0 && fsize > 0 {
		eprintln('ERROR: Can not read the file [${path}] in the memory buffer.')
		return false
	}
	return true
}

// read a file as []u8
pub fn read_bytes_from_file(file_path string) []u8 {
	mut buffer := []u8{}
	buffer = os.read_bytes(file_path) or {
		eprintln('ERROR: Texure file: [${file_path}] NOT FOUND.')
		exit(0)
	}
	return buffer
}

fn (mut app App) load_texture_from_buffer(buf voidptr, buf_len int) (gfx.Image, int, int) {
	// load image
	stbi.set_flip_vertically_on_load(true)
	img := stbi.load_from_memory(buf, buf_len) or {
		eprintln('ERROR: Can not load image from buffer, file: [${app.item_list.lst[app.item_list.item_index]}].')
		return app.logo_texture, app.logo_w, app.logo_h
		// exit(1)
	}
	res := create_texture(int(img.width), int(img.height), img.data)
	unsafe {
		img.free()
	}
	return res, int(img.width), int(img.height)
}

pub fn (mut app App) load_texture_from_file(file_name string) (gfx.Image, int, int) {
	app.read_bytes(file_name)
	return app.load_texture_from_buffer(app.mem_buf, app.mem_buf_size)
}

pub fn show_logo(mut app App) {
	clear_modifier_params(mut app)
	if app.texture != app.logo_texture {
		destroy_texture(app.texture)
	}
	app.texture = app.logo_texture
	app.img_w = app.logo_w
	app.img_h = app.logo_h
	app.img_ratio = f32(app.img_w) / f32(app.img_h)
	// app.gg.refresh_ui()
}

pub fn load_image(mut app App) {
	if app.item_list.loaded == false || app.init_flag == false {
		// show_logo(mut app)
		// app.state = .show
		return
	}
	app.state = .loading
	clear_modifier_params(mut app)
	// destroy the texture, avoid to destroy the logo
	if app.texture != app.logo_texture {
		destroy_texture(app.texture)
	}

	// load from .ZIP file
	if app.item_list.is_inside_a_container() == true {
		app.texture, app.img_w, app.img_h = app.load_texture_from_zip() or {
			eprintln('ERROR: Can not load image from .ZIP file [${app.item_list.lst[app.item_list.item_index]}].')
			show_logo(mut app)
			app.state = .show
			return
		}
		app.img_ratio = f32(app.img_w) / f32(app.img_h)
		app.state = .show
		// app.gg.refresh_ui()
		return
	}

	// if we are out of the zip, close it
	if app.zip_index >= 0 {
		app.zip_index = -1
		app.zip.close()
	}

	file_path := app.item_list.get_file_path()
	if file_path.len > 0 {
		// println("${app.item_list.lst[app.item_list.item_index]} $file_path ${app.item_list.lst.len}")
		app.texture, app.img_w, app.img_h = app.load_texture_from_file(file_path)
		app.img_ratio = f32(app.img_w) / f32(app.img_h)
		// println("texture: [${app.img_w},${app.img_h}] ratio: ${app.img_ratio}")
	} else {
		app.texture = app.logo_texture
		app.img_w = app.logo_w
		app.img_h = app.logo_h
		app.img_ratio = f32(app.img_w) / f32(app.img_h)
		println('texture NOT FOUND: use logo!')
	}
	app.state = .show
}

/******************************************************************************
*
* Init / Cleanup
*
******************************************************************************/
fn app_init(mut app App) {
	app.init_flag = true

	// 3d pipeline
	mut pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }

	color_state := gfx.ColorState{
		blend: gfx.BlendState{
			enabled: true
			src_factor_rgb: .src_alpha
			dst_factor_rgb: .one_minus_src_alpha
		}
	}
	pipdesc.colors[0] = color_state

	pipdesc.depth = gfx.DepthState{
		write_enabled: true
		compare: .less_equal
	}
	pipdesc.cull_mode = .back
	app.pip_viewer = sgl.make_pipeline(&pipdesc)

	// load logo
	app.logo_texture, app.logo_w, app.logo_h = app.load_texture_from_file(app.logo_path)
	app.logo_ratio = f32(app.img_w) / f32(app.img_h)

	app.img_w = app.logo_w
	app.img_h = app.logo_h
	app.img_ratio = app.logo_ratio
	app.texture = app.logo_texture

	println('INIT DONE!')

	// init done, load the first image if any
	load_image(mut app)
}

fn cleanup(mut app App) {
	gfx.shutdown()

	// delete temp files
	os.rm(app.font_path) or { eprintln('ERROR: Can not delete temp font file.') }
	os.rm(app.logo_path) or { eprintln('ERROR: Can not delete temp logo file.') }
	println('Cleaning done.')
}

/******************************************************************************
*
* Draw functions
*
******************************************************************************/
[manualfree]
fn frame(mut app App) {
	ws := gg.window_size_real_pixels()
	if ws.width <= 0 || ws.height <= 0 {
		return
	}

	mut ratio := f32(ws.width) / ws.height
	dw := ws.width
	dh := ws.height

	app.gg.begin()
	sgl.defaults()

	// set viewport
	sgl.viewport(0, 0, dw, dh, true)

	// enable our pipeline
	sgl.load_pipeline(app.pip_viewer)
	sgl.enable_texture()
	sgl.texture(app.texture)

	// translation
	tr_x := app.tr_x / app.img_w
	tr_y := -app.tr_y / app.img_h
	sgl.push_matrix()
	sgl.translate(tr_x, tr_y, 0.0)
	// scaling/zoom
	sgl.scale(2.0 * app.scale, 2.0 * app.scale, 0.0)
	// roation
	mut rotation := 0
	if app.state == .show && app.item_list.n_item > 0 {
		rotation = app.item_list.lst[app.item_list.item_index].rotation
		sgl.rotate(pi_2 * f32(rotation), 0.0, 0.0, -1.0)
	}

	// draw the image
	mut w := f32(0.5)
	mut h := f32(0.5)

	// for 90 and 270 degree invert w and h
	// rotation change image ratio, manage it
	if rotation & 1 == 1 {
		tmp := w
		w = h
		h = tmp
		h /= app.img_ratio * ratio
	} else {
		h /= app.img_ratio / ratio
	}

	// manage image overflow in case of strange scales
	if h > 0.5 {
		reduction_factor := 0.5 / h
		h = h * reduction_factor
		w = w * reduction_factor
	}
	if w > 0.5 {
		reduction_factor := 0.5 / w
		h = h * reduction_factor
		w = w * reduction_factor
	}

	// println("$w,$h")
	// white multiplicator for now
	mut c := [u8(255), 255, 255]!
	sgl.begin_quads()
	sgl.v2f_t2f_c3b(-w, -h, 0, 0, c[0], c[1], c[2])
	sgl.v2f_t2f_c3b(w, -h, 1, 0, c[0], c[1], c[2])
	sgl.v2f_t2f_c3b(w, h, 1, 1, c[0], c[1], c[2])
	sgl.v2f_t2f_c3b(-w, h, 0, 1, c[0], c[1], c[2])
	sgl.end()

	// restore all the transformations
	sgl.pop_matrix()

	// Zoom icon
	/*
	if app.show_info_flag == true && app.scale > 1 {
		mut bw := f32(0.25)
		mut bh := f32(0.25 / app.img_ratio)

		// manage the rotations
		if rotation & 1 == 1 {
			bw,bh = bh,bw
		}
		mut bx := f32(1 - bw)
		mut by := f32(1 - bh)
		if rotation & 1 == 1 {
			bx,by = by,bx
		}

		bh_old1 := bh
		bh *= ratio
		by += (bh_old1 - bh)

		// draw the zoom icon
		sgl.begin_quads()
		r := int(u32(rotation) << 1)
		sgl.v2f_t2f_c3b(bx     , by     , uv[(0 + r) & 7] , uv[(1 + r) & 7], c[0], c[1], c[2])
		sgl.v2f_t2f_c3b(bx + bw, by     , uv[(2 + r) & 7] , uv[(3 + r) & 7], c[0], c[1], c[2])
		sgl.v2f_t2f_c3b(bx + bw, by + bh, uv[(4 + r) & 7] , uv[(5 + r) & 7], c[0], c[1], c[2])
		sgl.v2f_t2f_c3b(bx     , by + bh, uv[(6 + r) & 7] , uv[(7 + r) & 7], c[0], c[1], c[2])
		sgl.end()

		// draw the zoom rectangle
		sgl.disable_texture()

		bw_old := bw
		bh_old := bh
		bw /=  app.scale
		bh /=  app.scale
		bx += (bw_old - bw) / 2 - (tr_x / 8) / app.scale
		by += (bh_old - bh) / 2 - ((tr_y / 8) / app.scale) * ratio

		c = [u8(255),255,0]! // yellow
		sgl.begin_line_strip()
		sgl.v2f_c3b(bx     , by     , c[0], c[1], c[2])
		sgl.v2f_c3b(bx + bw, by     , c[0], c[1], c[2])
		sgl.v2f_c3b(bx + bw, by + bh, c[0], c[1], c[2])
		sgl.v2f_c3b(bx     , by + bh, c[0], c[1], c[2])
		sgl.v2f_c3b(bx     , by     , c[0], c[1], c[2])
		sgl.end()
	}
	*/
	sgl.disable_texture()

	//
	// Draw info text
	//
	x := 10
	y := 10

	app.gg.begin()

	if app.state in [.scanning, .loading] {
		if app.state == .scanning {
			draw_text(mut app, text_scanning, x, y, 20)
		} else {
			draw_text(mut app, text_loading, x, y, 20)
		}
	} else if app.state == .show {
		// print the info text if needed
		if app.item_list.n_item > 0 && app.show_info_flag == true {
			/*
			// waiting for better autofree
			num := app.item_list.lst[app.item_list.item_index].n_item
			of_num := app.item_list.n_item
			x_screen := int(w*2*app.scale*dw)
			y_screen := int(h*2*app.scale*dw)
			rotation_angle := 90 * rotation
			scale_str := "${app.scale:.2}"
			text := "${num}/${of_num} [${app.img_w},${app.img_h}]=>[${x_screen},${y_screen}] ${app.item_list.lst[app.item_list.item_index].name} scale: ${scale_str} rotation: ${rotation_angle}"
			//text := "${num}/${of_num}"
			draw_text(mut app, text, 10, 10, 20)
			unsafe{
				text.free()
			}
			*/

			// Using string builder to avoid memory leak
			num := app.item_list.lst[app.item_list.item_index].n_item
			of_num := app.item_list.n_item
			x_screen := int(w * 2 * app.scale * dw)
			y_screen := int(h * 2 * app.scale * dw)
			rotation_angle := 90 * rotation
			scale_str := '${app.scale:.2}'
			app.bl.clear()
			app.bl.write_string('${num}/${of_num}')
			app.bl.write_string(' [${app.img_w}x${app.img_h}]=>[${x_screen}x${y_screen}]')
			app.bl.write_string(' ${app.item_list.lst[app.item_list.item_index].name}')
			app.bl.write_string(' scale: ${scale_str} rotation: ${rotation_angle}')
			draw_text(mut app, app.bl.str(), 10, 10, 20)
		} else {
			if app.item_list.n_item <= 0 {
				draw_text(mut app, text_drop_files, 10, 10, 20)
			}
		}
	}

	//
	// Draw Help text
	//
	if app.show_help_flag == true {
		mut txt_y := 30
		for r in help_text_rows {
			draw_text(mut app, r, 10, txt_y, 20)
			txt_y += 20
		}
	}

	app.gg.end()
	app.frame_count++
}

// draw readable text
fn draw_text(mut app App, in_txt string, in_x int, in_y int, fnt_sz f32) {
	scale := app.gg.scale
	font_size := int(fnt_sz * scale)

	mut txt_conf_c0 := gx.TextCfg{
		color: gx.white // gx.rgb( (c >> 16) & 0xff, (c >> 8) & 0xff, c & 0xff)
		align: .left
		size: font_size
	}
	mut txt_conf_c1 := gx.TextCfg{
		color: gx.black // gx.rgb( (c >> 16) & 0xff, (c >> 8) & 0xff, c & 0xff)
		align: .left
		size: font_size
	}

	x := int(in_x * scale)
	y := int(in_y * scale)
	app.gg.draw_text(x + 2, y + 2, in_txt, txt_conf_c0)
	app.gg.draw_text(x, y, in_txt, txt_conf_c1)
}

/******************************************************************************
*
* events management
*
******************************************************************************/
fn clear_modifier_params(mut app App) {
	app.scale = 1.0

	app.sc_flag = false
	app.sc_x = 0
	app.sc_y = 0
	app.last_sc_x = 0
	app.last_sc_y = 0

	app.tr_flag = false
	app.tr_x = 0
	app.tr_y = 0
	app.last_tr_x = 0
	app.last_tr_y = 0
}

fn my_event_manager(mut ev gg.Event, mut app App) {
	// navigation using the mouse wheel
	app.scroll_y = int(ev.scroll_y)
	if app.scroll_y != 0 {
		inc := int(-1 * app.scroll_y / 4)
		if app.item_list.n_item > 0 {
			app.item_list.get_next_item(inc)
			load_image(mut app)
		}
	}

	if ev.typ == .mouse_move {
		app.mouse_x = int(ev.mouse_x)
		app.mouse_y = int(ev.mouse_y)
	}
	if ev.typ == .touches_began || ev.typ == .touches_moved {
		if ev.num_touches > 0 {
			touch_point := ev.touches[0]
			app.mouse_x = int(touch_point.pos_x)
			app.mouse_y = int(touch_point.pos_y)
		}
	}

	// clear all parameters
	if ev.typ == .mouse_down && ev.mouse_button == .middle {
		clear_modifier_params(mut app)
	}

	// ws := gg.window_size_real_pixels()
	// ratio := f32(ws.width) / ws.height
	// dw := ws.width
	// dh := ws.height

	// --- translate ---
	if ev.typ == .mouse_down && ev.mouse_button == .left {
		app.tr_flag = true
		app.last_tr_x = app.mouse_x
		app.last_tr_y = app.mouse_y
	}
	if ev.typ == .mouse_up && ev.mouse_button == .left && app.tr_flag == true {
		app.tr_flag = false
	}
	if ev.typ == .mouse_move && app.tr_flag == true {
		app.tr_x += (app.mouse_x - app.last_tr_x) * 3 * app.gg.scale
		app.tr_y += (app.mouse_y - app.last_tr_y) * 3 * app.gg.scale
		app.last_tr_x = app.mouse_x
		app.last_tr_y = app.mouse_y
		// println("Translate: ${app.tr_x} ${app.tr_y}")
	}

	// --- scaling ---
	if ev.typ == .mouse_down && ev.mouse_button == .right && app.sc_flag == false {
		app.sc_flag = true
		app.last_sc_x = app.mouse_x
		app.last_sc_y = app.mouse_y
	}
	if ev.typ == .mouse_up && ev.mouse_button == .right && app.sc_flag == true {
		app.sc_flag = false
	}
	if ev.typ == .mouse_move && app.sc_flag == true {
		app.sc_x = app.mouse_x - app.last_sc_x
		app.sc_y = app.mouse_y - app.last_sc_y
		app.last_sc_x = app.mouse_x
		app.last_sc_y = app.mouse_y

		app.scale += f32(app.sc_x / 100)
		if app.scale < 0.1 {
			app.scale = 0.1
		}
		if app.scale > 32 {
			app.scale = 32
		}
	}

	if ev.typ == .key_down {
		// println(ev.key_code)

		// Exit using the ESC key or Q key
		if ev.key_code == .escape || ev.key_code == .q {
			cleanup(mut app)
			exit(0)
		}
		// Toggle info text OSD
		if ev.key_code == .i {
			app.show_info_flag = !app.show_info_flag
		}
		// Toggle help text
		if ev.key_code == .h {
			app.show_help_flag = !app.show_help_flag
		}

		// do actions only if there are items in the list
		if app.item_list.loaded == true && app.item_list.n_item > 0 {
			// show previous image
			if ev.key_code == .left {
				app.item_list.get_next_item(-1)
				load_image(mut app)
			}
			// show next image
			if ev.key_code == .right {
				app.item_list.get_next_item(1)
				load_image(mut app)
			}

			// jump to the next container if possible
			if ev.key_code == .up {
				app.item_list.go_to_next_container(1)
				load_image(mut app)
			}
			// jump to the previous container if possible
			if ev.key_code == .down {
				app.item_list.go_to_next_container(-1)
				load_image(mut app)
			}

			// rotate the image
			if ev.key_code == .r {
				app.item_list.rotate(1)
			}

			// full screen
			if ev.key_code == .f {
				println('Full screen state: ${sapp.is_fullscreen()}')
				sapp.toggle_fullscreen()
			}
		}
	}

	// drag&drop
	if ev.typ == .files_droped {
		app.state = .scanning
		// set logo texture during scanning
		show_logo(mut app)

		num := sapp.get_num_dropped_files()
		mut file_list := []string{}
		for i in 0 .. num {
			file_list << sapp.get_dropped_file_path(i)
		}
		println('Scanning: ${file_list}')
		app.item_list = &Item_list{}
		app.item_list.loaded = false

		// load_image(mut app)
		// go app.item_list.get_items_list(file_list)

		load_and_show(file_list, mut app)
	}
}

fn load_and_show(file_list []string, mut app App) {
	app.item_list.get_items_list(file_list)
	load_image(mut app)
}

/******************************************************************************
*
* Main
*
******************************************************************************/
fn main() {
	// mut font_path := os.resource_abs_path(os.join_path('../assets/fonts/', 'RobotoMono-Regular.ttf'))
	font_name := 'RobotoMono-Regular.ttf'
	font_path := os.join_path(os.temp_dir(), font_name)
	println('Temporary path for the font file: [${font_path}]')

	// if the font doesn't exist create it from the ebedded one
	if os.exists(font_path) == false {
		println('Write font [${font_name}] in temp folder.')
		embedded_file := $embed_file('../assets/fonts/RobotoMono-Regular.ttf')
		os.write_file(font_path, embedded_file.to_string()) or {
			eprintln('ERROR: not able to write font file to [${font_path}]')
			exit(1)
		}
	}

	// logo image
	logo_name := 'logo.png'
	logo_path := os.join_path(os.temp_dir(), logo_name)
	println('Temporary path for the logo: [${logo_path}]')
	// if the logo doesn't exist create it from the ebedded one
	if os.exists(logo_path) == false {
		println('Write logo [${logo_name}] in temp folder.')
		embedded_file := $embed_file('../assets/logo.png')
		os.write_file(logo_path, embedded_file.to_string()) or {
			eprintln('ERROR: not able to write logo file to [${logo_path}]')
			exit(1)
		}
	}

	// App init
	mut app := &App{
		gg: 0
		// zip fields
		zip: 0
		item_list: 0
	}

	app.state = .scanning
	app.logo_path = logo_path
	app.font_path = font_path

	// Scan all the arguments to find images
	app.item_list = &Item_list{}
	// app.item_list.get_items_list(os.args[1..])
	load_and_show(os.args[1..], mut app)

	app.gg = gg.new_context(
		width: win_width
		height: win_height
		create_window: true
		window_title: 'V Image viewer 0.8'
		user_data: app
		bg_color: bg_color
		frame_fn: frame
		init_fn: app_init
		cleanup_fn: cleanup
		event_fn: my_event_manager
		font_path: font_path
		enable_dragndrop: true
		max_dropped_files: 64
		max_dropped_file_path_length: 2048
		// ui_mode: true
	)

	app.gg.run()
}
