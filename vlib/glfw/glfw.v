// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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

#flag solaris -I/opt/local/include
#flag solaris -L/opt/local/lib
#flag solaris -lglfw

#flag linux -lglfw
#flag windows -lgdi32 -lshell32 -lglfw3
#flag mingw -mwindows
#include <GLFW/glfw3.h>
// #flag darwin -framework Carbon
// #flag darwin -framework Cocoa
// #flag darwin -framework CoreVideo
// #flag darwin -framework IOKit
pub const (
	resizable = 1
	decorated = 2
)

pub const (
	key_escape     = 256
	key_space      = 32
	key_left_super = 343

	key_up    = 265
	key_left  = 263
	key_right = 262
	key_down  = 264
)

fn C.glfwGetWindowUserPointer() voidptr
fn C.glfwGetPrimaryMonitor() voidptr
fn C.glfwSetWindowUserPointer()
fn C.glfwSetCursor()
fn C.glfwGetCursorPos()
fn C.glfwSetClipboardString()
fn C.glfwGetWindowContentScale()
fn C.glfwGetClipboardString()
fn C.glfwGetKey()
fn C.glfwGetTime() f64
fn C.glfwSetCharModsCallback()
fn C.glfwSetKeyCallback()
fn C.glfwPostEmptyEvent()
fn C.glfwSetScrollCallback()
fn C.glfwSetWindowSizeCallback()
fn C.glfwSetMouseButtonCallback()
fn C.glfwSetCursorPosCallback()
fn C.glfwSwapBuffers()
fn C.glfwWindowShouldClose() bool
fn C.glfwSetWindowShouldClose()
fn C.glfwWaitEvents()
fn C.glfwPollEvents()
fn C.glfwSwapInterval()
fn C.glfwMakeContextCurrent()
fn C.glfwSetWindowTitle()
fn C.glfwTerminate()
fn C.glfwCreateWindow(w int, h int, title charptr, m voidptr, sh voidptr) voidptr
fn C.glfwWindowHint()
fn C.glfwDestroyWindow()
fn C.glfwInit()
fn C.glGetIntegerv()

// joe-c: fix & remove
struct TmpGlImportHack {
	hack gl.TmpGlImportHack
}

pub struct WinCfg {
	width             int
	height            int
	title             string
	ptr               voidptr
	borderless        bool
	is_modal          int
	is_browser        bool
	url        		  string
	always_on_top     bool
	scale_to_monitor  bool = true
}

// data  *C.GLFWwindow
// TODO change data to cobj
pub struct Window {
	data    voidptr
	title   string
	mx      int
	my      int
	scale_  f32
}

pub struct Size {
pub:
	width  int
	height int
}

pub struct Pos {
pub:
	x int
	y int
}

// type clickpub fn pub fn (window * GLFWwindow, button, action, mods int)
type ClickPubFn fn (window voidptr, button, action, mods int)

/*
 * TODO broken
fn init() {
	init_glfw()
}
*/

pub fn init_glfw() {
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
	if c.scale_to_monitor {
		$if windows {
			window_hint(C.GLFW_SCALE_TO_MONITOR, 1)
		}
	}
	cwindow := C.glfwCreateWindow(c.width, c.height, c.title.str, 0, 0)
	if isnil(cwindow) {
		println('failed to create a glfw window, make sure you have a GPU driver installed')
		C.glfwTerminate()
	}
	// println('create window wnd=$cwindow ptr==$c.ptr')
	C.glfwSetWindowUserPointer(cwindow, c.ptr)

	mut scale := f32(1.0)
	$if windows {
		C.glfwGetWindowContentScale(cwindow, &scale, &scale)
	}
	$else {
		scale = 1.0
	}

	window := &Window {
		data: cwindow,
		title: c.title,
		scale_: scale
	}
	return window
}

pub fn (w &Window) set_title(title string) {
	C.glfwSetWindowTitle(w.data, title.str)
}

pub fn (w &Window) make_context_current() {
	C.glfwMakeContextCurrent(w.data)
}

pub fn (w &Window) scale() f32 {
	return w.scale_
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

pub fn (w &Window) set_should_close(close bool) {
	C.glfwSetWindowShouldClose(w.data, close)
}

pub fn (w &Window) should_close() bool {
	return C.glfwWindowShouldClose(w.data)
}

pub fn (w &Window) swap_buffers() {
	C.glfwSwapBuffers(w.data)
}

pub fn (mut w Window) onmousemove(cb voidptr) {
	C.glfwSetCursorPosCallback(w.data, cb)
}

pub fn (mut w Window) set_mouse_button_callback(cb voidptr) {
	C.glfwSetMouseButtonCallback(w.data, cb)
}

pub fn (mut w Window) on_resize(cb voidptr) {
	C.glfwSetWindowSizeCallback(w.data, cb)
}

pub fn (mut w Window) on_click(cb voidptr) {
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

pub fn (mut w Window) onkeydown(cb voidptr) {
	C.glfwSetKeyCallback(w.data, cb)
}

pub fn (mut w Window) onchar(cb voidptr) {
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

pub fn get_cursor_pos(cwindow voidptr) (f64, f64) {
	x := f64(0)
	y := f64(0)
	C.glfwGetCursorPos(cwindow, &x, &y)

	mut scale := f32(1.0)
	$if windows {
		C.glfwGetWindowContentScale(cwindow, &scale, &scale)
	}
	$else {
		scale = 1.0
	}
	return x/scale, y/scale
}

pub fn (w &Window) get_cursor_pos() Pos {
	x := f64(0)
	y := f64(0)
	C.glfwGetCursorPos(w.data, &x, &y)

	return Pos {
		x: int(x/w.scale_)
		y: int(y/w.scale_)
	}
}

enum Cursor {
	arrow
	ibeam
	hand
}

pub fn set_cursor(c Cursor) {
	C.glfwSetCursor(0, C.GLFW_IBEAM_CURSOR)
}

pub fn (w &Window) set_cursor(c Cursor) {
	C.glfwSetCursor(w.data, C.GLFW_IBEAM_CURSOR)

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

fn C.glfwGetWindowSize(window &Window, width &int, height &int) // screen coordinates
fn C.glfwGetFramebufferSize(window &Window, width &int, height &int) // pixels

// get_window_size in screen coordinates
pub fn (w &Window) get_window_size() Size {
	res := Size {0, 0}
	C.glfwGetWindowSize(w.data, &res.width, &res.height)
	return res
}

// get_framebuffer_size in pixels
pub fn (w &Window) get_framebuffer_size() Size {
	res := Size {0, 0}
	C.glfwGetFramebufferSize(w.data, &res.width, &res.height)
	return res
}

pub fn (size Size) str() string {
	return '{$size.width, $size.height}'
}

pub fn get_window_user_pointer(gwnd voidptr) voidptr {
	return C.glfwGetWindowUserPointer(gwnd)
}
