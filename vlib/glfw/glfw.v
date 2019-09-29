// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module glfw

// note: we might need special case for this
// see TmpGlImportHack below (joe-c)
import gl

#flag -I @VROOT/thirdparty/glfw
#flag -L @VROOT/thirdparty/glfw

// Debugging a custom build
//-#flag darwin -L/var/tmp/glfw/src/

// MacPorts
#flag darwin -L/opt/local/lib

#flag darwin -lglfw
#flag freebsd -I/usr/local/include
#flag freebsd -Wl,-L/usr/local/lib,-lglfw
#flag linux -lglfw
#flag windows -lglfw3
#include <GLFW/glfw3.h>
// #flag darwin -framework Carbon
// #flag darwin -framework Cocoa
// #flag darwin -framework CoreVideo
// #flag darwin -framework IOKit
const (
	RESIZABLE = 1
	DECORATED = 2
)

const (
	KEY_ESCAPE     = 256
	key_space     = 32
	KEY_LEFT_SUPER = 343
)

const (
	KeyUp    = 265
	KeyLeft  = 263
	KeyRight = 262
	KeyDown  = 264
)

// joe-c: fix & remove
struct TmpGlImportHack {
	hack gl.TmpGlImportHack
}

struct WinCfg {
	width      int
	height     int
	title      string
	ptr        voidptr
	borderless bool
	is_modal   int
	is_browser bool
	url        string
	always_on_top     bool
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
pub:
	x int
	y int
}

// type clickpub fn pub fn (window * GLFWwindow, button, action, mods int)
type clickpubfn fn (window voidptr, button, action, mods int)

pub fn init() {
	C.glfwInit()
	C.glfwWindowHint(C.GLFW_CONTEXT_VERSION_MAJOR, 3)
	C.glfwWindowHint(C.GLFW_CONTEXT_VERSION_MINOR, 3)
	C.glfwWindowHint(C.GLFW_OPENGL_FORWARD_COMPAT, C.GL_TRUE)
	C.glfwWindowHint(C.GLFW_OPENGL_PROFILE, C.GLFW_OPENGL_CORE_PROFILE)
}

pub fn (w &Window) destroy() {
	C.glfwDestroyWindow(w.data)
}

pub fn terminate() {
	C.glfwTerminate()
}

// pub fn mouse_move(w * GLFWwindow, x, y double) {
pub fn mouse_move(w voidptr, x, y f64) {
	// #printf("%f : %f => %d \n", x,y);
}

pub fn window_hint(key, val int) {
	C.glfwWindowHint(key, val)
}

pub fn create_window(c WinCfg) &Window {
	if c.borderless {
		window_hint(C.GLFW_RESIZABLE, 0)
		window_hint(C.GLFW_DECORATED, 0)
	}
	if c.always_on_top {
		window_hint(C.GLFW_FLOATING, 1)
	}
	cwindow := C.glfwCreateWindow(c.width, c.height, c.title.str, 0, 0)
	if isnil(cwindow) {
		println('failed to create a glfw window, make sure you have a GPU driver installed')
		C.glfwTerminate()
	}
	println('create window wnd=$cwindow ptr==$c.ptr')
	C.glfwSetWindowUserPointer(cwindow, c.ptr)
	window := &Window {
		data: cwindow,
		title: c.title,
	}
	return window
}

pub fn (w &Window) set_title(title string) {
	C.glfwSetWindowTitle(w.data, title.str)
}

pub fn (w &Window) make_context_current() {
	C.glfwMakeContextCurrent(w.data)
}

pub fn swap_interval(interval int) {
	C.glfwSwapInterval(interval)
}

pub fn wait_events() {
	C.glfwWaitEvents()
}

pub fn poll_events() {
	C.glfwPollEvents()
}

pub fn set_should_close(w voidptr, close bool) {
	C.glfwSetWindowShouldClose(w, close)
}

pub fn (w &Window) should_close() bool {
	return C.glfwWindowShouldClose(w.data)
}

pub fn (w &Window) swap_buffers() {
	C.glfwSwapBuffers(w.data)
}

pub fn (w mut Window) onmousemove(cb voidptr) {
	C.glfwSetCursorPosCallback(w.data, cb)
}

pub fn (w mut Window) set_mouse_button_callback(cb voidptr) {
	C.glfwSetMouseButtonCallback(w.data, cb)
}

pub fn (w mut Window) on_click(cb voidptr) {
	C.glfwSetMouseButtonCallback(w.data, cb)
}

pub fn (w &Window) set_scroll_callback(cb voidptr) {
	C.glfwSetScrollCallback(w.data, cb)
}

pub fn (w &Window) on_scroll(cb voidptr) {
	C.glfwSetScrollCallback(w.data, cb)
}

pub fn post_empty_event() {
	C.glfwPostEmptyEvent()
}

pub fn (w mut Window) onkeydown(cb voidptr) {
	C.glfwSetKeyCallback(w.data, cb)
}

pub fn (w mut Window) onchar(cb voidptr) {
	C.glfwSetCharModsCallback(w.data, cb)
}

pub fn get_time() f64 {
	return C.glfwGetTime()
}

pub fn key_pressed(wnd voidptr, key int) bool {
	return int(C.glfwGetKey(wnd, key)) == C.GLFW_PRESS
}

pub fn (w &Window) get_clipboard_text() string {
	return string(byteptr(C.glfwGetClipboardString(w.data)))
}

pub fn (w &Window) set_clipboard_text(s string) {
	C.glfwSetClipboardString(w.data, s.str)
}

pub fn (w &Window) get_cursor_pos() Pos {
	x := f64(0)
	y := f64(0)
	C.glfwGetCursorPos(w.data, &x, &y)
	return Pos {
		x: int(x)
		y: int(y)
	}
}

pub fn (w &Window) user_ptr() voidptr {
	return C.glfwGetWindowUserPointer(w.data)
}

pub fn (w &Window) set_user_ptr(ptr voidptr) {
	C.glfwSetWindowUserPointer(w.data, ptr)
}

struct C.GLFWvidmode {
	width int
	height int
}

pub fn C.glfwGetVideoMode() &C.GLFWvidmode

pub fn get_monitor_size() Size {
	//# GLFWvidmode* mode = glfwGetVideoMode(glfwGetPrimaryMonitor());
	mode := C.glfwGetVideoMode(C.glfwGetPrimaryMonitor())
	return Size{mode.width, mode.height}
}

pub fn (size Size) str() string {
	return '{$size.width, $size.height}'
}

pub fn get_window_user_pointer(gwnd voidptr) voidptr {
	return C.glfwGetWindowUserPointer(gwnd)
}

