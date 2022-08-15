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
	key_code     KeyCode
	char_code    u32
	key_repeat   bool
	modifiers    u32
	mouse_button MouseButton
	mouse_x      f32
	mouse_y      f32
	mouse_dx     f32
	mouse_dy     f32
	scroll_x     f32
	scroll_y     f32
	// todo(playX): add touches API support in js.dom
	// num_touches        int
	// touches            [8]TouchPoint
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

pub struct Config {
pub:
	width         int
	height        int
	use_ortho     bool // unused, still here just for backwards compatibility
	retina        bool
	resizable     bool
	user_data     voidptr
	font_size     int
	create_window bool
	// window_user_ptr voidptr
	window_title      string
	borderless_window bool
	always_on_top     bool
	bg_color          gx.Color
	init_fn           FNCb   = unsafe { nil }
	frame_fn          FNCb   = unsafe { nil }
	native_frame_fn   FNCb   = unsafe { nil }
	cleanup_fn        FNCb   = unsafe { nil }
	fail_fn           FNFail = unsafe { nil }
	//
	event_fn FNEvent = unsafe { nil }
	quit_fn  FNEvent = unsafe { nil }
	//
	keydown_fn FNKeyDown = unsafe { nil }
	keyup_fn   FNKeyUp   = unsafe { nil }
	char_fn    FNChar    = unsafe { nil }
	//
	move_fn    FNMove    = unsafe { nil }
	click_fn   FNClick   = unsafe { nil }
	unclick_fn FNUnClick = unsafe { nil }
	leave_fn   FNEvent   = unsafe { nil }
	enter_fn   FNEvent   = unsafe { nil }
	resized_fn FNEvent   = unsafe { nil }
	scroll_fn  FNEvent   = unsafe { nil }
	// wait_events       bool // set this to true for UIs, to save power
	fullscreen    bool
	scale         f32 = 1.0
	sample_count  int
	swap_interval int = 1 // 1 = 60fps, 2 = 30fps etc. The preferred swap interval (ignored on some platforms)
	// ved needs this
	// init_text bool
	font_path             string
	custom_bold_font_path string
	ui_mode               bool // refreshes only on events to save CPU usage
	// font bytes for embedding
	font_bytes_normal []u8
	font_bytes_bold   []u8
	font_bytes_mono   []u8
	font_bytes_italic []u8
	native_rendering  bool // Cocoa on macOS/iOS, GDI+ on Windows
	// drag&drop
	enable_dragndrop             bool // enable file dropping (drag'n'drop), default is false
	max_dropped_files            int = 1 // max number of dropped files to process (default: 1)
	max_dropped_file_path_length int = 2048 // max length in bytes of a dropped UTF-8 file path (default: 2048)
	canvas                       string
}

const size = Size{0, 0}

pub fn window_size() Size {
	return gg.size
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
	mbtn_mask     u8
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
	context           JS.CanvasRenderingContext2D [noinit]
	canvas            JS.HTMLCanvasElement        [noinit]
	// *before* the current event was different
}

fn get_canvas(elem JS.HTMLElement) JS.HTMLCanvasElement {
	match elem {
		JS.HTMLCanvasElement {
			return elem
		}
		else {
			panic('gg: element is not an HTMLCanvasElement')
		}
	}
}

fn get_context(canvas JS.HTMLCanvasElement) JS.CanvasRenderingContext2D {
	ctx := canvas.getContext('2d'.str, js_undefined()) or { panic('cannot get context') }
	match ctx {
		JS.CanvasRenderingContext2D {
			return ctx
		}
		else {
			panic('failed to get 2D context')
		}
	}
}

pub fn new_context(cfg Config) &Context {
	mut g := &Context{}

	g.user_data = cfg.user_data
	g.width = cfg.width
	g.height = cfg.height
	g.ui_mode = cfg.ui_mode
	mut sz := gg.size
	sz.height = g.height
	sz.width = g.width
	g.config = cfg
	if isnil(cfg.user_data) {
		g.user_data = g
	}
	g.window = dom.window()
	document := dom.document
	canvas_elem := document.getElementById(cfg.canvas.str) or {
		panic('gg: cannot get canvas element')
	}
	canvas := get_canvas(canvas_elem)
	g.canvas = canvas
	g.context = get_context(g.canvas)

	mouse_down_event_handler := fn [mut g] (event JS.Event) {
		match event {
			JS.MouseEvent {
				e := g.handle_mouse_event(event, .mouse_down)
				if !isnil(g.config.event_fn) {
					f := g.config.event_fn
					f(e, g.config.user_data)
				}
				if !isnil(g.config.click_fn) {
					f := g.config.click_fn
					f(e.mouse_x, e.mouse_y, e.mouse_button, g.config.user_data)
				}
			}
			else {}
		}
	}

	mouse_up_event_handler := fn [mut g] (event JS.Event) {
		match event {
			JS.MouseEvent {
				e := g.handle_mouse_event(event, .mouse_up)
				if !isnil(g.config.event_fn) {
					f := g.config.event_fn
					f(e, g.config.user_data)
				}
				if !isnil(g.config.unclick_fn) {
					f := g.config.unclick_fn
					f(e.mouse_x, e.mouse_y, e.mouse_button, g.config.user_data)
				}
			}
			else {}
		}
	}
	mouse_move_event_handler := fn [mut g] (event JS.Event) {
		match event {
			JS.MouseEvent {
				e := g.handle_mouse_event(event, .mouse_move)
				if !isnil(g.config.event_fn) {
					f := g.config.event_fn
					f(e, g.config.user_data)
				}
				if !isnil(g.config.move_fn) {
					f := g.config.move_fn
					f(e.mouse_x, e.mouse_y, g.config.user_data)
				}
			}
			else {}
		}
	}

	mouse_leave_event_handler := fn [mut g] (event JS.Event) {
		match event {
			JS.MouseEvent {
				e := g.handle_mouse_event(event, .mouse_leave)
				if !isnil(g.config.event_fn) {
					f := g.config.event_fn
					f(e, g.config.user_data)
				}
				if !isnil(g.config.leave_fn) {
					f := g.config.leave_fn
					f(e, g.config.user_data)
				}
			}
			else {}
		}
	}

	mouse_enter_event_handler := fn [mut g] (event JS.Event) {
		match event {
			JS.MouseEvent {
				e := g.handle_mouse_event(event, .mouse_enter)
				if !isnil(g.config.event_fn) {
					f := g.config.event_fn
					f(e, g.config.user_data)
				}
				if !isnil(g.config.enter_fn) {
					f := g.config.enter_fn
					f(e, g.config.user_data)
				}
			}
			else {}
		}
	}

	keydown_event_handler := fn [mut g] (event JS.Event) {
		println('keyboard')
		match event {
			JS.KeyboardEvent {
				e := g.handle_keyboard_event(event, .key_down)

				if !isnil(g.config.event_fn) {
					f := g.config.event_fn
					f(e, g.config.user_data)
				}
				if !isnil(g.config.keydown_fn) {
					f := g.config.keydown_fn
					// todo: modifiers
					f(e.key_code, .super, g.config.user_data)
				}
			}
			else {}
		}
	}
	g.canvas.addEventListener('mousedown'.str, mouse_down_event_handler, JS.EventListenerOptions{})
	dom.window().addEventListener('mouseup'.str, mouse_up_event_handler, JS.EventListenerOptions{})
	g.canvas.addEventListener('mousemove'.str, mouse_move_event_handler, JS.EventListenerOptions{})
	g.canvas.addEventListener('mouseleave'.str, mouse_leave_event_handler, JS.EventListenerOptions{})
	g.canvas.addEventListener('mouseenter'.str, mouse_enter_event_handler, JS.EventListenerOptions{})
	dom.document.addEventListener('keydown'.str, keydown_event_handler, JS.EventListenerOptions{})
	return g
}

pub fn (mut ctx Context) run() {
	gg_animation_frame_fn(mut ctx)
}

pub fn (mut ctx Context) begin() {
	// ctx.context.beginPath()
}

pub fn (mut ctx Context) end() {
	// ctx.context.closePath()
}

pub fn (mut ctx Context) draw_line(x1 f32, y1 f32, x2 f32, y2 f32, c gx.Color) {
	ctx.context.beginPath()
	ctx.context.strokeStyle = c.to_css_string().str
	ctx.context.moveTo(x1, y1)
	ctx.context.lineTo(x2, y2)
	ctx.context.stroke()
	ctx.context.closePath()
}

pub fn (mut ctx Context) quit() {
}

pub fn (mut ctx Context) draw_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	ctx.context.beginPath()
	ctx.context.fillStyle = c.to_css_string().str
	ctx.context.fillRect(x, y, w, h)
	ctx.context.closePath()
}

fn gg_animation_frame_fn(mut g Context) {
	g.frame++
	g.context.clearRect(0, 0, g.config.width, g.config.height)
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

fn (mut g Context) handle_mouse_event(event JS.MouseEvent, typ DOMEventType) Event {
	mut e := Event{}

	e.typ = typ
	e.frame_count = g.frame

	match int(event.button) {
		0 {
			e.mouse_button = .left
		}
		1 {
			e.mouse_button = .middle
		}
		2 {
			e.mouse_button = .right
		}
		else {
			e.mouse_button = .invalid
		}
	}
	e.mouse_x = int(event.offsetX)
	e.mouse_y = int(event.offsetY)
	e.mouse_dx = int(event.movementX)
	e.mouse_dy = int(event.movementY)
	bitplace := int(event.button)
	g.mbtn_mask |= u8(1 << bitplace)
	// g.mouse_buttons = MouseButtons(g.mbtn_mask)

	g.mouse_pos_x = int(event.offsetX)
	g.mouse_pos_y = int(event.offsetY)
	g.mouse_dx = int(event.movementX)
	g.mouse_dy = int(event.movementY)
	return e
}

fn (mut g Context) handle_keyboard_event(event JS.KeyboardEvent, typ DOMEventType) Event {
	mut e := Event{}
	e.typ = typ
	e.frame_count = g.frame

	match string(event.code) {
		'Space' {
			e.key_code = .space
		}
		'Minus' {
			e.key_code = .minus
		}
		'Quote' {
			e.key_code = .apostrophe
		}
		'Comma' {
			e.key_code = .comma
		}
		'Period' {
			e.key_code = .period
		}
		'Digit0' {
			e.key_code = ._0
		}
		'Digit1' {
			e.key_code = ._1
		}
		'Digit2' {
			e.key_code = ._2
		}
		'Digit3' {
			e.key_code = ._3
		}
		'Digit4' {
			e.key_code = ._4
		}
		'Digit5' {
			e.key_code = ._5
		}
		'Digit6' {
			e.key_code = ._6
		}
		'Digit7' {
			e.key_code = ._7
		}
		'Digit8' {
			e.key_code = ._8
		}
		'Digit9' {
			e.key_code = ._9
		}
		'Semicolon' {
			e.key_code = .semicolon
		}
		'Equal' {
			e.key_code = .equal
		}
		'KeyA' {
			e.key_code = .a
		}
		'KeyB' {
			e.key_code = .b
		}
		'KeyC' {
			e.key_code = .c
		}
		'KeyD' {
			e.key_code = .d
		}
		'KeyE' {
			e.key_code = .e
		}
		'KeyF' {
			e.key_code = .f
		}
		'KeyG' {
			e.key_code = .g
		}
		'KeyH' {
			e.key_code = .h
		}
		'KeyI' {
			e.key_code = .i
		}
		'KeyJ' {
			e.key_code = .j
		}
		'KeyK' {
			e.key_code = .k
		}
		'KeyL' {
			e.key_code = .l
		}
		'KeyM' {
			e.key_code = .m
		}
		'KeyN' {
			e.key_code = .n
		}
		'KeyO' {
			e.key_code = .o
		}
		'KeyP' {
			e.key_code = .p
		}
		'KeyQ' {
			e.key_code = .q
		}
		'KeyR' {
			e.key_code = .r
		}
		'KeyS' {
			e.key_code = .s
		}
		'KeyT' {
			e.key_code = .t
		}
		'KeyU' {
			e.key_code = .u
		}
		'KeyV' {
			e.key_code = .v
		}
		'KeyW' {
			e.key_code = .w
		}
		'KeyX' {
			e.key_code = .x
		}
		'KeyY' {
			e.key_code = .y
		}
		'KeyZ' {
			e.key_code = .z
		}
		'BracketLeft' {
			e.key_code = .left_bracket
		}
		'BracketRight' {
			e.key_code = .right_bracket
		}
		'Backslash' {
			e.key_code = .backslash
		}
		'Backquote' {
			e.key_code = .grave_accent
		}
		'Escape' {
			e.key_code = .escape
		}
		'Enter' {
			e.key_code = .enter
		}
		'Tab' {
			e.key_code = .tab
		}
		'Backspace' {
			e.key_code = .backspace
		}
		'Insert' {
			e.key_code = .insert
		}
		'Delete' {
			e.key_code = .delete
		}
		'ArrowRight' {
			e.key_code = .right
		}
		'ArrowLeft' {
			e.key_code = .left
		}
		'ArrowUp' {
			e.key_code = .up
		}
		'ArrowDown' {
			e.key_code = .down
		}
		'PageUp' {
			e.key_code = .page_up
		}
		'PageDown' {
			e.key_code = .page_down
		}
		'Home' {
			e.key_code = .home
		}
		'End' {
			e.key_code = .end
		}
		'CapsLock' {
			e.key_code = .caps_lock
		}
		'ScrollLock' {
			e.key_code = .scroll_lock
		}
		'NumLock' {
			e.key_code = .num_lock
		}
		'PrintScreen' {
			e.key_code = .print_screen
		}
		'Pause' {
			e.key_code = .pause
		}
		'ShiftLeft' {
			e.key_code = .left_shift
		}
		'ShiftRight' {
			e.key_code = .right_shift
		}
		'AltLeft' {
			e.key_code = .left_alt
		}
		'AltRight' {
			e.key_code = .right_alt
		}
		'ControlLeft' {
			e.key_code = .left_control
		}
		'ControlRight' {
			e.key_code = .right_control
		}
		else {
			panic('todo: more keycodes (${string(event.code)})')
		}
	}

	return e
}
