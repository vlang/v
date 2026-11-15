// CoreText and CoreGraphics calls used by gg.canvas text. They mirror the
// platform calls of Skia's SkTypeface_Mac and SkScalerContext_Mac (BSD
// license, see shaders/LICENSE-skia); the surrounding arithmetic lives in V.
#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CoreGraphics.h>
#include <CoreText/CoreText.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// ctfont_from_skdata(): a CTFont of the default size for in-memory font data.
static void* v_canvas_ct_font_from_data(const uint8_t* bytes, size_t length) {
	CFDataRef data = CFDataCreate(kCFAllocatorDefault, bytes, (CFIndex)length);
	if (!data) return NULL;
	CTFontDescriptorRef desc = CTFontManagerCreateFontDescriptorFromData(data);
	CFRelease(data);
	if (!desc) return NULL;
	CTFontRef font = CTFontCreateWithFontDescriptor(desc, 0, NULL);
	CFRelease(desc);
	return (void*)font;
}

static void* v_canvas_cf_retain(void* ref) {
	return ref ? (void*)CFRetain((CFTypeRef)ref) : NULL;
}

static void v_canvas_cf_release(void* ref) {
	if (ref) CFRelease((CFTypeRef)ref);
}

static uint32_t v_canvas_ct_font_traits(void* font) {
	return (uint32_t)CTFontGetSymbolicTraits((CTFontRef)font);
}

static void v_canvas_add_opsz_attr(CFMutableDictionaryRef attr, double opsz) {
	CFNumberRef value = CFNumberCreate(kCFAllocatorDefault, kCFNumberDoubleType, &opsz);
	CFDictionarySetValue(attr, CFSTR("NSCTFontOpticalSizeAttribute"), value);
	CFRelease(value);
}

// SkCTFontCreateExactCopy(); opsz_set selects the explicit opsz variation.
static void* v_canvas_ct_font_exact_copy(void* base_font, double text_size, int opsz_set,
		double opsz_value) {
	CTFontRef base = (CTFontRef)base_font;
	CFMutableDictionaryRef attr = CFDictionaryCreateMutable(kCFAllocatorDefault, 0,
		&kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	if (opsz_set) {
		v_canvas_add_opsz_attr(attr, opsz_value);
	} else {
		CFTypeRef opsz = CTFontCopyAttribute(base, CFSTR("NSCTFontOpticalSizeAttribute"));
		double opsz_val = 0;
		if (!opsz || CFGetTypeID(opsz) != CFNumberGetTypeID()
			|| !CFNumberGetValue((CFNumberRef)opsz, kCFNumberDoubleType, &opsz_val)
			|| opsz_val <= 0) {
			opsz_val = CTFontGetSize(base);
		}
		if (opsz) CFRelease(opsz);
		v_canvas_add_opsz_attr(attr, opsz_val);
	}
	int zero = 0;
	CFNumberRef tracking = CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &zero);
	CFDictionarySetValue(attr, CFSTR("NSCTFontUnscaledTrackingAttribute"), tracking);
	CFRelease(tracking);
	CTFontDescriptorRef desc = CTFontDescriptorCreateWithAttributes(attr);
	CFRelease(attr);
	CTFontRef copy = CTFontCreateCopyWithAttributes(base, text_size, NULL, desc);
	CFRelease(desc);
	return (void*)copy;
}

// The glyph metrics of SkScalerContext_Mac::generateMetrics() before Skia's
// own rounding: the advance and the CG bounds, both mapped by the transform.
// Returns 0 when the glyph has no bounds.
static int v_canvas_ct_glyph_metrics(void* font, const double transform[6], uint16_t glyph,
		float advance[2], double bounds[4]) {
	CTFontRef ct = (CTFontRef)font;
	CGAffineTransform t = CGAffineTransformMake(transform[0], transform[1], transform[2],
		transform[3], transform[4], transform[5]);
	CGGlyph cg = glyph;
	CGSize cg_advance;
	CTFontGetAdvancesForGlyphs(ct, kCTFontOrientationHorizontal, &cg, &cg_advance, 1);
	cg_advance = CGSizeApplyAffineTransform(cg_advance, t);
	advance[0] = (float)cg_advance.width;
	advance[1] = -(float)cg_advance.height;
	CGRect cg_bounds;
	CTFontGetBoundingRectsForGlyphs(ct, kCTFontOrientationHorizontal, &cg, &cg_bounds, 1);
	cg_bounds = CGRectApplyAffineTransform(cg_bounds, t);
	if (0 == cg_advance.width && 0 == cg_advance.height) {
		CGPathRef path = CTFontCreatePathForGlyph(ct, cg, NULL);
		int empty = !path || CGPathIsEmpty(path);
		if (path) CGPathRelease(path);
		if (empty) return 0;
	}
	if (cg_bounds.size.width <= 0 || cg_bounds.size.height <= 0) return 0;
	bounds[0] = cg_bounds.origin.x;
	bounds[1] = cg_bounds.origin.y;
	bounds[2] = cg_bounds.size.width;
	bounds[3] = cg_bounds.size.height;
	return 1;
}

// SkScalerContext_Mac::Offscreen: one CG bitmap per scaler context that only
// grows (to powers of two) as larger glyphs are drawn.
typedef struct {
	CGContextRef cg;
	CGColorSpaceRef rgb_space;
	uint32_t* storage;
	int width;
	int height;
	int do_aa;
	int do_lcd;
} v_canvas_ct_offscreen;

static v_canvas_ct_offscreen* v_canvas_ct_offscreen_new(void) {
	return (v_canvas_ct_offscreen*)calloc(1, sizeof(v_canvas_ct_offscreen));
}

static void v_canvas_ct_offscreen_free(v_canvas_ct_offscreen* o) {
	if (!o) return;
	if (o->cg) CGContextRelease(o->cg);
	if (o->rgb_space) CGColorSpaceRelease(o->rgb_space);
	free(o->storage);
	free(o);
}

static int v_canvas_next_pow2(int n) {
	// SkNextPow2(): the smallest power of two >= n, for n >= 1.
	return n <= 1 ? 1 : 1 << (32 - __builtin_clz((unsigned)(n - 1)));
}

// Offscreen::getCG() for a non-color glyph: draws the glyph black on white
// and returns its first row; *row_bytes receives the bitmap stride.
static uint32_t* v_canvas_ct_draw_glyph(v_canvas_ct_offscreen* o, void* font,
		const double transform[6], const double inverse[6], uint16_t glyph, int left, int top,
		int width, int height, float sub_x, float sub_y, int do_aa, int do_lcd, size_t* row_bytes) {
	if (!o->rgb_space) o->rgb_space = CGColorSpaceCreateDeviceRGB();
	size_t rb = (size_t)o->width * 4;
	if (!o->cg || o->width < width || o->height < height) {
		if (o->width < width) o->width = v_canvas_next_pow2(width);
		if (o->height < height) o->height = v_canvas_next_pow2(height);
		rb = (size_t)o->width * 4;
		free(o->storage);
		o->storage = (uint32_t*)malloc(rb * (size_t)o->height);
		if (o->cg) CGContextRelease(o->cg);
		o->cg = CGBitmapContextCreate(o->storage, o->width, o->height, 8, rb, o->rgb_space,
			kCGBitmapByteOrder32Host | (CGBitmapInfo)kCGImageAlphaNoneSkipFirst);
		CGContextSetAllowsFontSubpixelQuantization(o->cg, false);
		CGContextSetShouldSubpixelQuantizeFonts(o->cg, false);
		CGContextSetAllowsFontSubpixelPositioning(o->cg, true);
		CGContextSetShouldSubpixelPositionFonts(o->cg, true);
		CGContextSetTextDrawingMode(o->cg, kCGTextFill);
		CGContextSetGrayFillColor(o->cg, 0.0f, 1.0f);
		o->do_aa = !do_aa;
		o->do_lcd = !do_lcd;
		CGContextSetTextMatrix(o->cg, CGAffineTransformMake(transform[0], transform[1],
			transform[2], transform[3], transform[4], transform[5]));
	}
	if (o->do_aa != do_aa) {
		CGContextSetShouldAntialias(o->cg, do_aa);
		o->do_aa = do_aa;
	}
	if (o->do_lcd != do_lcd) {
		CGContextSetShouldSmoothFonts(o->cg, do_lcd);
		o->do_lcd = do_lcd;
	}
	uint32_t* image = o->storage + (size_t)(o->height - height) * (size_t)o->width;
	for (int y = 0; y < height; ++y) {
		uint32_t* row = (uint32_t*)((char*)image + rb * (size_t)y);
		for (int x = 0; x < width; ++x) row[x] = 0xFFFFFFFF;
	}
	CGPoint point = CGPointMake(-left + sub_x, top + height - sub_y);
	CGAffineTransform inv = CGAffineTransformMake(inverse[0], inverse[1], inverse[2], inverse[3],
		inverse[4], inverse[5]);
	point = CGPointApplyAffineTransform(point, inv);
	CGGlyph cg = glyph;
	CTFontDrawGlyphs((CTFontRef)font, &cg, &point, 1, o->cg);
	*row_bytes = rb;
	return image;
}

// CGAffineTransformInvert() that keeps a non-invertible transform unchanged,
// as SkScalerContext_Mac does.
static void v_canvas_cg_invert(const double t[6], double out[6]) {
	CGAffineTransform m = CGAffineTransformMake(t[0], t[1], t[2], t[3], t[4], t[5]);
	CGAffineTransform inv = CGAffineTransformInvert(m);
	out[0] = inv.a; out[1] = inv.b; out[2] = inv.c; out[3] = inv.d;
	out[4] = inv.tx; out[5] = inv.ty;
}

// SkCTFontGetSmoothBehavior(): 0 none, 1 some, 2 subpixel. Probes CoreGraphics
// with Skia's spider glyph.
#include "text_spider_font.h"
static int v_canvas_ct_smooth_behavior(void) {
	static int behavior = -1;
	if (behavior >= 0) return behavior;
	uint32_t no_smooth[16][16] = {0};
	uint32_t smooth[16][16] = {0};
	CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
	CGBitmapInfo info = (CGBitmapInfo)kCGImageAlphaNoneSkipFirst | kCGBitmapByteOrder32Host;
	CGContextRef no_ctx = CGBitmapContextCreate(&no_smooth, 16, 16, 8, 16 * 4, cs, info);
	CGContextRef ctx = CGBitmapContextCreate(&smooth, 16, 16, 8, 16 * 4, cs, info);
	CFDataRef data = CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, v_canvas_spider_ttf,
		sizeof(v_canvas_spider_ttf), kCFAllocatorNull);
	CTFontDescriptorRef desc = CTFontManagerCreateFontDescriptorFromData(data);
	CTFontRef font = CTFontCreateWithFontDescriptor(desc, 16, NULL);
	CGContextSetShouldSmoothFonts(no_ctx, false);
	CGContextSetShouldAntialias(no_ctx, true);
	CGContextSetTextDrawingMode(no_ctx, kCGTextFill);
	CGContextSetGrayFillColor(no_ctx, 1, 1);
	CGContextSetShouldSmoothFonts(ctx, true);
	CGContextSetShouldAntialias(ctx, true);
	CGContextSetTextDrawingMode(ctx, kCGTextFill);
	CGContextSetGrayFillColor(ctx, 1, 1);
	CGPoint point = CGPointMake(0, 3);
	CGGlyph spider = 3;
	CTFontDrawGlyphs(font, &spider, &point, 1, no_ctx);
	CTFontDrawGlyphs(font, &spider, &point, 1, ctx);
	int result = 0;
	for (int x = 0; x < 16 && result != 2; ++x) {
		for (int y = 0; y < 16; ++y) {
			uint32_t p = smooth[x][y];
			uint32_t r = (p >> 16) & 0xFF, g = (p >> 8) & 0xFF, b = p & 0xFF;
			if (r != g || r != b) {
				result = 2;
				break;
			}
			if (no_smooth[x][y] != p) result = 1;
		}
	}
	CFRelease(font);
	CFRelease(desc);
	CFRelease(data);
	CGContextRelease(ctx);
	CGContextRelease(no_ctx);
	CGColorSpaceRelease(cs);
	behavior = result;
	return behavior;
}
