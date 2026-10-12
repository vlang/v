module gg

// Native AppKit code is built once with the SDK and cached independently.
#flag darwin @VEXEROOT/vlib/gg/gg_darwin.o

@[c_extern; typedef]
struct C.v_gg_color {
	r u8
	g u8
	b u8
	a u8
}

@[c_extern; typedef]
struct C.v_gg_string {
	str &char
	len usize
}

@[c_extern; typedef]
struct C.v_gg_text_cfg {
	color C.v_gg_color
	size  i32
	bold  bool
	mono  bool
}

@[c_extern; typedef]
struct C.v_gg_size {
	width  i32
	height i32
}

@[c_extern; typedef]
struct C.v_gg_image {
	width  i32
	height i32
	ok     bool
	data   voidptr
}

@[c_extern]
fn C.gg_get_screen_size() C.v_gg_size

@[c_extern]
fn C.darwin_draw_string(x i32, y i32, s C.v_gg_string, cfg C.v_gg_text_cfg)

@[c_extern]
fn C.darwin_text_width(s C.v_gg_string) i32

@[c_extern]
fn C.darwin_text_width_with_cfg(s C.v_gg_string, cfg C.v_gg_text_cfg) i32

@[c_extern]
fn C.darwin_window_refresh()

@[c_extern]
fn C.darwin_draw_rect(f32, f32, f32, f32, C.v_gg_color)

@[c_extern]
fn C.darwin_create_image(path C.v_gg_string) C.v_gg_image

@[c_extern]
fn C.darwin_draw_image(f32, f32, f32, f32, voidptr)

@[c_extern]
fn C.darwin_draw_circle(f32, f32, f32, C.v_gg_color)

@[c_extern]
fn C.darwin_draw_circle_empty(f32, f32, f32, C.v_gg_color)

fn darwin_native_color(c Color) C.v_gg_color {
	return C.v_gg_color{c.r, c.g, c.b, c.a}
}

fn darwin_native_string(s string) C.v_gg_string {
	return C.v_gg_string{&char(s.str), usize(s.len)}
}

fn darwin_native_text_cfg(cfg TextCfg) C.v_gg_text_cfg {
	return C.v_gg_text_cfg{darwin_native_color(cfg.color), i32(cfg.size), cfg.bold, cfg.mono}
}

fn darwin_get_screen_size() Size {
	size := C.gg_get_screen_size()
	return Size{int(size.width), int(size.height)}
}

fn darwin_draw_string(x int, y int, s string, cfg TextCfg) {
	C.darwin_draw_string(i32(x), i32(y), darwin_native_string(s), darwin_native_text_cfg(cfg))
}

fn darwin_text_width(s string) int {
	return int(C.darwin_text_width(darwin_native_string(s)))
}

fn darwin_text_width_with_cfg(s string, cfg TextCfg) int {
	return int(C.darwin_text_width_with_cfg(darwin_native_string(s), darwin_native_text_cfg(cfg)))
}

fn darwin_create_image(path string) Image {
	image := C.darwin_create_image(darwin_native_string(path))
	return Image{ width: int(image.width), height: int(image.height), ok: image.ok, data: image.data, path: path }
}

fn darwin_draw_image(x f32, y f32, w f32, h f32, image &Image) {
	C.darwin_draw_image(x, y, w, h, image.data)
}

fn darwin_draw_rect(x f32, y f32, w f32, h f32, c Color) {
	C.darwin_draw_rect(x, y, w, h, darwin_native_color(c))
}

fn darwin_draw_circle(x f32, y f32, diameter f32, c Color) {
	C.darwin_draw_circle(x, y, diameter, darwin_native_color(c))
}

fn darwin_draw_circle_empty(x f32, y f32, diameter f32, c Color) {
	C.darwin_draw_circle_empty(x, y, diameter, darwin_native_color(c))
}
