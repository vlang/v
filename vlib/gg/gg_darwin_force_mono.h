#ifndef GG_DARWIN_FORCE_MONO_H
#define GG_DARWIN_FORCE_MONO_H

#include <Cocoa/Cocoa.h>

static inline void* gg_darwin_application(void) {
	return (__bridge void*)NSApp;
}

static inline void* gg_darwin_color_attribute(void) {
	return (__bridge void*)NSForegroundColorAttributeName;
}

static inline void* gg_darwin_font_attribute(void) {
	return (__bridge void*)NSFontAttributeName;
}

static inline void* gg_darwin_device_size(void) {
	return (__bridge void*)NSDeviceSize;
}

static inline void gg_darwin_fill_rect(void* ctx, double x, double y, double width, double height) {
	CGContextFillRect((CGContextRef)ctx, CGRectMake(x, y, width, height));
}

static inline void gg_darwin_fill_ellipse(void* ctx, double x, double y, double width, double height) {
	CGContextFillEllipseInRect((CGContextRef)ctx, CGRectMake(x, y, width, height));
}

static inline void gg_darwin_stroke_ellipse(void* ctx, double x, double y, double width, double height) {
	CGContextStrokeEllipseInRect((CGContextRef)ctx, CGRectMake(x, y, width, height));
}

static inline bool gg_darwin_mouse_in_rect(double x, double y, double rx, double ry, double width, double height, bool flipped) {
	return NSMouseInRect(NSMakePoint(x, y), NSMakeRect(rx, ry, width, height), flipped);
}

static int g_gg_force_mono = 0;

static inline void gg_set_force_mono(int on) {
	g_gg_force_mono = on;
}

static inline int gg_get_force_mono(void) {
	return g_gg_force_mono;
}

#endif
