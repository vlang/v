module gg

import gx
import js.dom

pub enum DOMEventType {
	invalid
	key_down
	key_up
	char
	mouse_down
	mouse_up
	mouse_scroll
	mouse_move
	mouse_enter
	mouse_leave
	touches_began
	touches_moved
	touches_ended
	touches_cancelled
	resized
	iconified
	restored
	focused
	unfocused
	suspended
	resumed
	update_cursor
	quit_requested
	clipboard_pasted
	files_droped
	num
}

pub struct Event {
pub mut:
	frame_count  u64
	typ          DOMEventType
	key_code     DOMKeyCode
	char_code    u32
	key_repeat   bool
	modifiers    u32
	mouse_button DOMMouseButton
	mouse_x      f32
	mouse_y      f32
	mouse_dx     f32
	mouse_dy     f32
	scroll_x     f32
	scroll_y     f32
	// todo(playX): add touches API support in js.dom
	// num_touches        int
	// touches            [8]C.sapp_touchpoint
	window_width       int
	window_height      int
	framebuffer_width  int
	framebuffer_height int
}

pub enum DOMMouseButton {
	invalid = -1
	left = 0
	right = 1
	middle = 2
}

pub enum DOMModifier {
	shift = 1 //(1<<0)
	ctrl = 2 //(1<<1)
	alt = 4 //(1<<2)
	super = 8 //(1<<3)
	lmb = 0x100
	rmb = 0x200
	mmb = 0x400
}

pub enum DOMKeyCode {
	invalid = 0
	space = 32
	apostrophe = 39 //'
	comma = 44 //,
	minus = 45 //-
	period = 46 //.
	slash = 47 ///
	_0 = 48
	_1 = 49
	_2 = 50
	_3 = 51
	_4 = 52
	_5 = 53
	_6 = 54
	_7 = 55
	_8 = 56
	_9 = 57
	semicolon = 59 //;
	equal = 61 //=
	a = 65
	b = 66
	c = 67
	d = 68
	e = 69
	f = 70
	g = 71
	h = 72
	i = 73
	j = 74
	k = 75
	l = 76
	m = 77
	n = 78
	o = 79
	p = 80
	q = 81
	r = 82
	s = 83
	t = 84
	u = 85
	v = 86
	w = 87
	x = 88
	y = 89
	z = 90
	left_bracket = 91 //[
	backslash = 92 //\
	right_bracket = 93 //]
	grave_accent = 96 //`
	world_1 = 161 // non-us #1
	world_2 = 162 // non-us #2
	escape = 256
	enter = 257
	tab = 258
	backspace = 259
	insert = 260
	delete = 261
	right = 262
	left = 263
	down = 264
	up = 265
	page_up = 266
	page_down = 267
	home = 268
	end = 269
	caps_lock = 280
	scroll_lock = 281
	num_lock = 282
	print_screen = 283
	pause = 284
	f1 = 290
	f2 = 291
	f3 = 292
	f4 = 293
	f5 = 294
	f6 = 295
	f7 = 296
	f8 = 297
	f9 = 298
	f10 = 299
	f11 = 300
	f12 = 301
	f13 = 302
	f14 = 303
	f15 = 304
	f16 = 305
	f17 = 306
	f18 = 307
	f19 = 308
	f20 = 309
	f21 = 310
	f22 = 311
	f23 = 312
	f24 = 313
	f25 = 314
	kp_0 = 320
	kp_1 = 321
	kp_2 = 322
	kp_3 = 323
	kp_4 = 324
	kp_5 = 325
	kp_6 = 326
	kp_7 = 327
	kp_8 = 328
	kp_9 = 329
	kp_decimal = 330
	kp_divide = 331
	kp_multiply = 332
	kp_subtract = 333
	kp_add = 334
	kp_enter = 335
	kp_equal = 336
	left_shift = 340
	left_control = 341
	left_alt = 342
	left_super = 343
	right_shift = 344
	right_control = 345
	right_alt = 346
	right_super = 347
	menu = 348
}

pub struct Context {
mut:
	render_text   bool = true
	image_cache   []Image
	needs_refresh bool = true
	ticks         int
pub mut:
	scale         f32 = 1.0
	width         int
	height        int
	window        JS.Window    [noinit]
	config        Config
	user_data     voidptr
	ui_mode       bool
	frame         u64
	mbtn_mask     byte
	mouse_buttons MouseButtons
	mouse_pos_x   int
	mouse_pos_y   int
	mouse_dx      int
	mouse_dy      int
	scroll_x      int
	scroll_y      int
	//
	key_modifiers     Modifier // the current key modifiers
	key_repeat        bool     // whether the pressed key was an autorepeated one
	pressed_keys      [key_code_max]bool // an array representing all currently pressed keys
	pressed_keys_edge [key_code_max]bool // true when the previous state of pressed_keys,
	canvas            JS.CanvasRenderingContext2D [noinit]
	// *before* the current event was different
}

pub fn new_context(cfg Config, canvas JS.CanvasRenderingContext2D) &Context {
	mut g := &Context{}
	g.user_data = cfg.user_data
	g.width = cfg.width
	g.height = cfg.height
	g.ui_mode = cfg.ui_mode
	g.canvas = canvas
	g.config = cfg
	if isnil(cfg.user_data) {
		g.user_data = g
	}
	g.window = dom.window()

	return g
}

pub fn (mut ctx Context) run() {
	gg_animation_frame_fn(mut ctx)
}

pub fn (mut ctx Context) begin() {
	// ctx.canvas.beginPath()
}

pub fn (mut ctx Context) end() {
	// ctx.canvas.closePath()
}

pub fn (mut ctx Context) draw_line(x1 f32, y1 f32, x2 f32, y2 f32, c gx.Color) {
	ctx.canvas.beginPath()
	ctx.canvas.strokeStyle = c.to_css_string().str
	ctx.canvas.moveTo(x1, y1)
	ctx.canvas.lineTo(x2, y2)
	ctx.canvas.stroke()
	ctx.canvas.closePath()
}

pub fn (mut ctx Context) draw_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	ctx.canvas.beginPath()
	ctx.canvas.fillStyle = c.to_css_string().str
	ctx.canvas.fillRect(x, y, w, h)
	ctx.canvas.closePath()
}

fn gg_animation_frame_fn(mut g Context) {
	g.frame++
	g.canvas.clearRect(0, 0, g.config.width, g.config.height)
	// todo(playXE): handle events
	if !isnil(g.config.frame_fn) {
		f := g.config.frame_fn
		f(g.user_data)
		g.needs_refresh = false
	}

	g.window.requestAnimationFrame(fn [mut g] (time JS.Number) {
		gg_animation_frame_fn(mut g)
	})
}
