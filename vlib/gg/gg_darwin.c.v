module gg

import macos
import math

#flag darwin -framework Cocoa
#flag darwin -framework CoreGraphics
#flag darwin -framework Foundation

#include "gg_darwin_force_mono.h"

fn C.gg_darwin_application() macos.Id
fn C.gg_darwin_color_attribute() macos.Id
fn C.gg_darwin_font_attribute() macos.Id
fn C.gg_darwin_device_size() macos.Id
fn C.gg_darwin_fill_rect(ctx voidptr, x f64, y f64, width f64, height f64)
fn C.gg_darwin_fill_ellipse(ctx voidptr, x f64, y f64, width f64, height f64)
fn C.gg_darwin_stroke_ellipse(ctx voidptr, x f64, y f64, width f64, height f64)
fn C.gg_darwin_mouse_in_rect(x f64, y f64, rx f64, ry f64, width f64, height f64, flipped bool) bool

fn C.CGColorCreateGenericRGB(r f64, g f64, b f64, a f64) voidptr
fn C.CGContextSetFillColorWithColor(ctx voidptr, color voidptr)
fn C.CGContextSetStrokeColorWithColor(ctx voidptr, color voidptr)
fn C.CGContextSetLineWidth(ctx voidptr, width f64)
fn C.CGContextSaveGState(ctx voidptr)
fn C.CGContextRestoreGState(ctx voidptr)
fn C.CGContextBeginPath(ctx voidptr)
fn C.CGContextMoveToPoint(ctx voidptr, x f64, y f64)
fn C.CGContextAddLineToPoint(ctx voidptr, x f64, y f64)
fn C.CGContextStrokePath(ctx voidptr)
fn C.CGColorRelease(color voidptr)

fn C.dispatch_get_main_queue() voidptr
fn C.dispatch_async_f(queue voidptr, context voidptr, work voidptr)

fn C.gg_set_force_mono(on int)
fn C.gg_get_force_mono() int

fn gg_color_key(c Color) u32 {
	return (u32(c.r)) |
		(u32(c.g) << 8) |
		(u32(c.b) << 16) |
		(u32(c.a) << 24)
}

fn gg_color_channel(value u8) f64 {
	return f64(value) / 255.0
}

fn nscolor(c Color) macos.Id {
	color := macos.msg_id_four_f64(macos.get_class('NSColor'), 'colorWithDeviceRed:green:blue:alpha:', gg_color_channel(c.r), gg_color_channel(c.g), gg_color_channel(c.b), gg_color_channel(c.a))
	return color
}

fn cgcolor(c Color) voidptr {
	color := C.CGColorCreateGenericRGB(gg_color_channel(c.r), gg_color_channel(c.g), gg_color_channel(c.b), gg_color_channel(c.a))
	return color
}

fn current_cg_context() voidptr {
	ns_context := macos.msg_id(macos.get_class('NSGraphicsContext'), 'currentContext')
	if ns_context == unsafe { nil } {
		return unsafe { nil }
	}
	return macos.msg_id(ns_context, 'CGContext')
}

pub fn gg_set_force_mono(on int) {
	C.gg_set_force_mono(on)
}

fn darwin_text_attrs(cfg TextCfg) macos.Id {
	mut font := macos.msg_id_f64(macos.get_class('NSFont'), 'userFontOfSize:', f64(cfg.size))
	if C.gg_get_force_mono() != 0 {
		font = macos.msg_id_id_f64(macos.get_class('NSFont'), 'fontWithName:size:', macos.nsstring('Menlo'), f64(cfg.size) - 1)
	} else if cfg.mono {
		font = macos.msg_id_id_f64(macos.get_class('NSFont'), 'fontWithName:size:', macos.nsstring('Menlo'), f64(cfg.size) - 5)
	}
	if cfg.bold {
		fm := macos.msg_id(macos.get_class('NSFontManager'), 'sharedFontManager')
		font = macos.msg_id_id_u64(fm, 'convertFont:toHaveTrait:', font, u64(2))
	}

	mut dict := macos.msg_id(macos.get_class('NSMutableDictionary'), 'alloc')
	dict = macos.msg_id(dict, 'init')
	macos.msg_void2(dict, 'setObject:forKey:', nscolor(cfg.color), C.gg_darwin_color_attribute())
	macos.msg_void2(dict, 'setObject:forKey:', font, C.gg_darwin_font_attribute())
	return dict
}

fn darwin_measure_attrs() macos.Id {
	mut dict := macos.msg_id(macos.get_class('NSMutableDictionary'), 'alloc')
	dict = macos.msg_id(dict, 'init')
	if C.gg_get_force_mono() != 0 {
		font := macos.msg_id_id_f64(macos.get_class('NSFont'), 'fontWithName:size:', macos.nsstring('Menlo'), 15.0)
		macos.msg_void2(dict, 'setObject:forKey:', font, C.gg_darwin_font_attribute())
	} else {
		font := macos.msg_id_f64(macos.get_class('NSFont'), 'userFontOfSize:', 16.0)
		macos.msg_void2(dict, 'setObject:forKey:', font, C.gg_darwin_font_attribute())
	}
	return dict
}

fn darwin_text_width_with_attrs(s string, attrs macos.Id) int {
	n := macos.nsstring(s)
	pt := macos.msg_point_id(n, 'sizeWithAttributes:', attrs)
	return int(math.ceil(pt.x))
}

pub fn darwin_text_width(s string) int {
	return darwin_text_width_with_attrs(s, darwin_measure_attrs())
}

pub fn darwin_text_width_with_cfg(s string, cfg TextCfg) int {
	return darwin_text_width_with_attrs(s, darwin_text_attrs(cfg))
}

pub fn darwin_draw_string(x int, y int, s string, cfg TextCfg) {
	attr := darwin_text_attrs(cfg)
	n := macos.nsstring(s)
	macos.msg_void_point_id(n, 'drawAtPoint:withAttributes:', macos.point(f64(x), f64(y - 15)), attr)
}

// darwin_draw_rect fills a rectangle in the current macOS graphics context.
pub fn darwin_draw_rect(x f32, y f32, width f32, height f32, c Color) {
	if width <= 0 || height <= 0 {
		return
	}
	context := current_cg_context()
	if context == unsafe { nil } {
		return
	}
	C.CGContextSetFillColorWithColor(context, cgcolor(c))
	C.gg_darwin_fill_rect(context, f64(x), f64(y), f64(width), f64(height))
}

// darwin_stroke_line draws a one-point line in native graphics coordinates.
fn darwin_stroke_line(context voidptr, x f32, y f32, x2 f32, y2 f32, c Color) {
	if context == unsafe { nil } {
		return
	}
	color := cgcolor(c)
	defer { C.CGColorRelease(color) }
	C.CGContextSaveGState(context)
	defer { C.CGContextRestoreGState(context) }
	C.CGContextSetStrokeColorWithColor(context, color)
	C.CGContextSetLineWidth(context, 1)
	C.CGContextBeginPath(context)
	C.CGContextMoveToPoint(context, f64(x), f64(y))
	C.CGContextAddLineToPoint(context, f64(x2), f64(y2))
	C.CGContextStrokePath(context)
}

fn mark_view_tree_needs_display(view macos.Id) {
	if view == unsafe { nil } {
		return
	}
	macos.msg_void_bool(view, 'setNeedsDisplay:', true)
	subviews := macos.msg_id(view, 'subviews')
	count := int(macos.msg_u64(subviews, 'count'))
	for i in 0 .. count {
		subview := macos.msg_id_u64(subviews, 'objectAtIndex:', u64(i))
		mark_view_tree_needs_display(subview)
	}
}

@[export: 'darwin_window_refresh_cb']
fn darwin_window_refresh_cb(ctx voidptr) {
	_ = ctx
	mut window := macos.msg_id(C.gg_darwin_application(), 'mainWindow')
	if window == unsafe { nil } {
		window = macos.msg_id(C.gg_darwin_application(), 'keyWindow')
	}
	if window == unsafe { nil } {
		return
	}
	content_view := macos.msg_id(window, 'contentView')
	if content_view == unsafe { nil } {
		return
	}
	mark_view_tree_needs_display(content_view)
	macos.msg_void(window, 'displayIfNeeded')
}

pub fn darwin_window_refresh() {
	queue := C.dispatch_get_main_queue()
	C.dispatch_async_f(queue, unsafe { nil }, voidptr(darwin_window_refresh_cb))
}

// gg_get_screen_size returns the size of the current window's screen, or the main screen.
pub fn gg_get_screen_size() Size {
	mut current_screen := macos.Id(unsafe { nil })
	main_window := macos.msg_id(C.gg_darwin_application(), 'mainWindow')
	if main_window != unsafe { nil } {
		current_screen = macos.msg_id(main_window, 'screen')
	}
	if current_screen == unsafe { nil } {
		key_window := macos.msg_id(C.gg_darwin_application(), 'keyWindow')
		if key_window != unsafe { nil } {
			current_screen = macos.msg_id(key_window, 'screen')
		}
	}
	if current_screen == unsafe { nil } {
		mouse_location := macos.msg_point(macos.get_class('NSEvent'), 'mouseLocation')
		screens := macos.msg_id(macos.get_class('NSScreen'), 'screens')
		count := int(macos.msg_u64(screens, 'count'))
		for i in 0 .. count {
			screen := macos.msg_id_u64(screens, 'objectAtIndex:', u64(i))
			frame := macos.msg_rect(screen, 'frame')
			if C.gg_darwin_mouse_in_rect(mouse_location.x, mouse_location.y, frame.x, frame.y, frame.width, frame.height, false) {
				current_screen = screen
				break
			}
		}
	}
	if current_screen == unsafe { nil } {
		current_screen = macos.msg_id(macos.get_class('NSScreen'), 'mainScreen')
	}
	desc := macos.msg_id(current_screen, 'deviceDescription')
	size_obj := macos.msg_id1(desc, 'objectForKey:', C.gg_darwin_device_size())
	display_size := macos.msg_point(size_obj, 'sizeValue')
	return Size{ width: int(display_size.x), height: int(display_size.y) }
}

pub fn gg_macos_resize_window(window_ptr voidptr, width int, height int) {
	if window_ptr == unsafe { nil } || width <= 0 || height <= 0 {
		return
	}
	ns_window := macos.Id(window_ptr)
	macos.msg_void_point(ns_window, 'setContentSize:', macos.point(f64(width), f64(height)))
}

pub fn gg_macos_set_window_resizable(window_ptr voidptr, resizable bool) {
	if window_ptr == unsafe { nil } {
		return
	}
	ns_window := macos.Id(window_ptr)
	mask := macos.msg_u64(ns_window, 'styleMask')
	mut new_mask := mask
	if resizable {
		new_mask |= u64(8)
	} else {
		new_mask &= ~u64(8)
	}
	macos.msg_void_u64(ns_window, 'setStyleMask:', new_mask)
}

pub fn darwin_create_image(path string) Image {
	path_ns := macos.nsstring(path)
	mut img := macos.msg_id(macos.get_class('NSImage'), 'alloc')
	img = macos.msg_id1(img, 'initWithContentsOfFile:', path_ns)
	size := macos.msg_point(img, 'size')
	mut res := Image{}
	res.width = int(size.x)
	res.height = int(size.y)
	res.path = path
	res.ok = true
	res.data = img
	return res
}

pub fn darwin_draw_image(x f32, y f32, w f32, h f32, img &Image) {
	i := macos.Id(img.data)
	macos.msg_void_rect(i, 'drawInRect:', macos.rect(f64(x), f64(y), f64(w), f64(h)))
}

// darwin_draw_circle fills a circle in the current macOS graphics context.
pub fn darwin_draw_circle(x f32, y f32, d f32, color Color) {
	if d <= 0 {
		return
	}
	context := current_cg_context()
	if context == unsafe { nil } {
		return
	}
	C.CGContextSetFillColorWithColor(context, cgcolor(color))
	C.gg_darwin_fill_ellipse(context, f64(x), f64(y), f64(d * 2), f64(d * 2))
}

// darwin_draw_circle_empty draws a circle outline in the current macOS graphics context.
pub fn darwin_draw_circle_empty(x f32, y f32, d f32, color Color) {
	if d <= 0 {
		return
	}
	context := current_cg_context()
	if context == unsafe { nil } {
		return
	}
	C.CGContextSaveGState(context)
	C.CGContextSetStrokeColorWithColor(context, cgcolor(color))
	C.CGContextSetLineWidth(context, 1.0)
	C.gg_darwin_stroke_ellipse(context, f64(x), f64(y), f64(d * 2), f64(d * 2))
	C.CGContextRestoreGState(context)
}
