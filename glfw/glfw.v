// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module glfw

// Debugging a custom build
#flag darwin -L/var/tmp/glfw/src/
#flag darwin -lglfw
#flag linux -lglfw
#flag windows -I/usr/local/Cellar/glfw/3.2.1/include/
// #include <glad/glad.h> // !gen.go include GLFW.v
#include <GLFW/glfw3.h>
// #flag darwin -framework Carbon
// #flag darwin -framework Cocoa
// #flag darwin -framework CoreVideo
// #flag darwin -framework IOKit
// struct C.GL
// @GLFWwindow* C.glfwCreateWindow
// #int gconst_init = 0;
const (
	RESIZABLE = 1
	DECORATED = 2
)

import const (
	GLFW_RESIZABLE
	GLFW_DECORATED
)

import const (
	GLFW_KEY_ENTER
	GLFW_KEY_A
	GLFW_KEY_B
	GLFW_KEY_P
	GLFW_KEY_F
	GLFW_KEY_M
	GLFW_KEY_L
	GLFW_KEY_V
	GLFW_KEY_R
	GLFW_KEY_D
	GLFW_KEY_7
	GLFW_KEY_Z
	GLFW_KEY_UP
	GLFW_KEY_DOWN
	GLFW_KEY_UP
	GLFW_KEY_LEFT
	GLFW_KEY_RIGHT
	GLFW_KEY_BACKSPACE
	GLFW_KEY_ENTER
	GLFW_KEY_ESCAPE
	GLFW_KEY_N
	GLFW_KEY_PERIOD
	GLFW_KEY_SLASH
	GLFW_KEY_F5
	GLFW_KEY_F6
	GLFW_KEY_MINUS
	GLFW_KEY_EQUAL
	GLFW_KEY_C
	GLFW_KEY_G
	GLFW_KEY_I
	GLFW_KEY_J
	GLFW_KEY_E
	GLFW_KEY_K
	GLFW_KEY_O
	GLFW_KEY_T
	GLFW_KEY_H
	GLFW_KEY_L
	GLFW_KEY_N
	GLFW_KEY_U
	GLFW_KEY_X
	GLFW_KEY_W
	GLFW_KEY_Y
	GLFW_KEY_Q
	GLFW_KEY_RIGHT_BRACKET
	GLFW_KEY_LEFT_BRACKET
	GLFW_KEY_8
	GLFW_KEY_TAB
	GLFW_KEY_COMMA
	GLFW_KEY_QUESTION
)

const (
	KEY_ESCAPE     = 256
	KEY_LEFT_SUPER = 343
)

const (
	KeyUp    = 265
	KeyLeft  = 263
	KeyRight = 262
	KeyDown  = 264
)

// TODO COPY PASTA
struct WinCfg {
	width      int
	height     int
	title      string
	ptr        voidptr
	borderless bool
	is_modal   int
	is_browser bool
	url        string
}

// data  *C.GLFWwindow
// TODO change data to cobj
struct Window {
	data  voidptr
	title string
	mx    int
	my    int
}

struct Size {
pub:
	width  int
	height int
}

struct Pos {
	x int
	y int
}

// type clickfn fn (window * GLFWwindow, button, action, mods int)
type clickfn fn (window voidptr, button, action, mods int)

fn init() {
	C.glfwInit()
	# glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	# glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	# glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	# glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
}

// fn mouse_move(w * GLFWwindow, x, y double) {
fn mouse_move(w voidptr, x, y double) {
	// #printf("%f : %f => %d \n", x,y);
}

// fn create_window(title string, w, h int) * Window {
fn window_hint(key, val int) {
	C.glfwWindowHint(key, val)
}

fn create_window(c WinCfg) *Window {
	// TODO why i need this in stdlib? extern?
	// # if (!gconst_init)   {  init_consts(); gconst_init = 1; }
	// ChatsRepo
	if c.borderless {
		window_hint(GLFW_RESIZABLE, 0)
		window_hint(GLFW_DECORATED, 0)
	}
	cwindow := C.glfwCreateWindow(c.width, c.height, c.title.str, 0, 0)
	# if (!cwindow)
	// if cwindow == 0
	{
		println('failed to credate glfw  window')
		C.glfwTerminate()
	}
	// # glfwSetCursorPosCallback(cwindow, glfw__mouse_move) ;
	// C.glfwSetCursorPosCallback(cwindow, mouse_move)
	C.printf('create window wnd=%p ptr==%p\n', cwindow, c.ptr)
	C.glfwSetWindowUserPointer(cwindow, c.ptr)
	// # void *a =glfwGetWindowUserPointer(cwindow);
	// # printf("aaaaaa=%p d=%d\n", a,a);
	window := &Window {
		data: cwindow,
		title: c.title,
	}
	// user_ptr: ptr,
	// repo: repo,
	// for !C.glfwWindowShouldClose(cwindow) {
	// C.glfwPollEvents()
	// wait_events()
	// }
	// C.glfwTerminate()
	return window
}

fn (w &Window) set_title(title string) {
	C.glfwSetWindowTitle(w.data, title.str)
}

fn (w &Window) make_context_current() {
	// ChatsRepo
	kkk := 0
	// println('making context current' )
	C.glfwMakeContextCurrent(w.data)
}

fn swap_interval(interval int) {
	C.glfwSwapInterval(interval)
}

fn wait_events() {
	C.glfwWaitEvents()
}

fn poll_events() {
	C.glfwPollEvents()
}

fn (w &Window) should_close() bool {
	// ChatsRepo
	return C.glfwWindowShouldClose(w.data)
}

fn (w &Window) swap_buffers() {
	C.glfwSwapBuffers(w.data)
}

fn (w mut Window) onmousemove(cb voidptr) {
	C.glfwSetCursorPosCallback(w.data, cb)
}

fn (w mut Window) set_mouse_button_callback(cb voidptr) {
	C.glfwSetMouseButtonCallback(w.data, cb)
}

fn (w mut Window) on_click(cb voidptr) {
	C.glfwSetMouseButtonCallback(w.data, cb)
}

fn (w &Window) set_scroll_callback(cb voidptr) {
	C.glfwSetScrollCallback(w.data, cb)
}

fn (w &Window) on_scroll(cb voidptr) {
	C.glfwSetScrollCallback(w.data, cb)
}

fn post_empty_event() {
	C.glfwPostEmptyEvent()
}

fn (w mut Window) onkeydown(cb voidptr) {
	C.glfwSetKeyCallback(w.data, cb)
}

fn (w mut Window) onchar(cb voidptr) {
	C.glfwSetCharModsCallback(w.data, cb)
}

fn get_time() double {
	return C.glfwGetTime()
}

fn key_pressed(wnd voidptr, key int) bool {
	# return glfwGetKey(wnd, key) == GLFW_PRESS;
	return false
}

// TODO not mut
fn (w mut Window) get_clipboard_text() string {
	return tos2(C.glfwGetClipboardString(w.data))
	// # char *c = glfwGetClipboardString(w->data);
	// # return tos_no_len(c);
	// return ''
}

fn (w &Window) set_clipboard_text(s string) {
	C.glfwSetClipboardString(w.data, s.str)
}

fn (w &Window) get_cursor_pos() Pos {
	x := double(0)
	y := double(0)
	C.glfwGetCursorPos(w.data, &x, &y)
	return Pos {
		x: int(x)
		y: int(y)
	}
}

fn (w &Window) user_ptr() voidptr {
	return C.glfwGetWindowUserPointer(w.data)
}

fn (w &Window) set_user_ptr(ptr voidptr) {
	C.glfwSetWindowUserPointer(w.data, ptr)
}

fn C.glfwGetVideoMode() C.GLFWvideoMode

fn get_monitor_size() Size {
	# GLFWvidmode* mode = glfwGetVideoMode(glfwGetPrimaryMonitor());
	// window_width = mode->width;
	// window_height = mode->height;
	// monitor := C.glfwGetPrimaryMonitor()
	res := Size{}
	# res.width=mode->width;
	# res.height=mode->height;
	// C.glfwGetMonitorPhysicalSize(monitor, &res.width, &res.height)
	return res
}

fn (size Size) str() string {
	return '{$size.width, $size.height}'
}

fn get_window_user_pointer(gwnd voidptr) voidptr {
	return C.glfwGetWindowUserPointer(gwnd)
}

