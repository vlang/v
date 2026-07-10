#include <stdint.h>
#include <Cocoa/Cocoa.h>

static uint32_t gg_color_key(gg__Color c) {
	return ((uint32_t)c.r)
		| ((uint32_t)c.g << 8)
		| ((uint32_t)c.b << 16)
		| ((uint32_t)c.a << 24);
}

static CGFloat gg_color_channel(uint8_t value) {
	return (CGFloat)value / 255.0;
}

static NSMutableDictionary<NSNumber*, NSColor*>* g_ns_color_cache = nil;
static NSMutableDictionary<NSNumber*, id>* g_cg_color_cache = nil;

NSColor* nscolor(gg__Color c) {
	if (g_ns_color_cache == nil) {
		g_ns_color_cache = [[NSMutableDictionary alloc] init];
	}
	NSNumber* key = @(gg_color_key(c));
	NSColor* cached = [g_ns_color_cache objectForKey:key];
	if (cached != nil) {
		return cached;
	}
	NSColor* color = [NSColor colorWithDeviceRed:gg_color_channel(c.r)
		green:gg_color_channel(c.g)
		blue:gg_color_channel(c.b)
		alpha:gg_color_channel(c.a)];
	[g_ns_color_cache setObject:color forKey:key];
	return color;
}

static CGColorRef cgcolor(gg__Color c) {
	if (g_cg_color_cache == nil) {
		g_cg_color_cache = [[NSMutableDictionary alloc] init];
	}
	NSNumber* key = @(gg_color_key(c));
	id cached = [g_cg_color_cache objectForKey:key];
	if (cached != nil) {
		return (__bridge CGColorRef)cached;
	}
	CGColorRef color = CGColorCreateGenericRGB(gg_color_channel(c.r),
		gg_color_channel(c.g),
		gg_color_channel(c.b),
		gg_color_channel(c.a));
	[g_cg_color_cache setObject:(__bridge id)color forKey:key];
	CGColorRelease(color);
	return (__bridge CGColorRef)[g_cg_color_cache objectForKey:key];
}

static CGContextRef current_cg_context() {
	NSGraphicsContext* context = [NSGraphicsContext currentContext];
	if (context == nil) {
		return NULL;
	}
	return [context CGContext];
}

CFStringRef cfstring(string s) {
	return CFStringCreateWithBytesNoCopy(kCFAllocatorDefault,
	                                     (const UInt8*)s.str,
	                                     s.len,
	                                     kCFStringEncodingUTF8,
	                                     false,
	                                     kCFAllocatorNull);
}

NSString* nsstring(string s) {
	CFStringRef cf = cfstring(s);
	if (cf == nil) {
		return @"";
	}
	return CFBridgingRelease(cf);
}

gg__Size gg_get_screen_size() {
	NSScreen *currentScreen = nil;
	NSWindow *mainWindow = [NSApp mainWindow];
	// 1. Try screen containing the main window
	if (mainWindow) {
		currentScreen = [mainWindow screen];
	}
	// 2. If no main window, try the key window (might be different, e.g., a panel)
	if (!currentScreen) {
		NSWindow *keyWindow = [NSApp keyWindow];
		if (keyWindow) {
			currentScreen = [keyWindow screen];
		}
	}
	// 3. If no relevant window, find the screen containing the mouse cursor
	if (!currentScreen) {
		// Get mouse location in global screen coordinates (bottom-left origin)
		NSPoint mouseLocation = [NSEvent mouseLocation];
		NSArray<NSScreen *> *screens = [NSScreen screens];
		for (NSScreen *screen in screens) {
			// Check if the mouse location is within the screen's frame
			// Note: Both mouseLocation and screen.frame use bottom-left origin coordinates
			if (NSMouseInRect(mouseLocation, [screen frame], NO)) {
				currentScreen = screen;
				break; // Found the screen with the mouse
			}
		}
	}
	// 4. As a last resort, fall back to the main screen
	if (!currentScreen) {
		NSLog(@"Warning: Could not determine current screen based on window or mouse. Falling back to mainScreen.");
		currentScreen = [NSScreen mainScreen];
	}
	// Now get the size of the determined screen
	NSDictionary *description = [currentScreen deviceDescription];
	// Use NSDeviceSize to get pixel dimensions
	NSSize displayPixelSize = [[description objectForKey:NSDeviceSize] sizeValue];
	// Create the V gg.Size object
	gg__Size res;
	res.width = displayPixelSize.width;
	res.height = displayPixelSize.height;
	return res;
}

void gg_macos_resize_window(void *window_ptr, int width, int height) {
	if (window_ptr == nil || width <= 0 || height <= 0) {
		return;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	[window setContentSize:NSMakeSize((CGFloat)width, (CGFloat)height)];
}

void gg_macos_set_window_resizable(void *window_ptr, bool resizable) {
	if (window_ptr == nil) {
		return;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	NSWindowStyleMask style = [window styleMask];
	if (resizable) {
		style |= NSWindowStyleMaskResizable;
	} else {
		style &= ~NSWindowStyleMaskResizable;
	}
	[window setStyleMask:style];
}

// When non-zero, every string is drawn in a monospace font regardless of the
// per-call cfg.mono flag. Lets an app switch the whole UI to a fixed-width font
// so text widths can be computed as char_count * advance (no CoreText layout).
// Default 0 => behaviour unchanged for every other gg app.
int g_gg_force_mono = 0;
void gg_set_force_mono(int on) { g_gg_force_mono = on; }

// Cache of the (font + color) attribute dictionaries passed to -drawAtPoint:.
// darwin_draw_string is called once per text run, every frame; building a fresh
// NSFont (font lookup), NSColor, and NSDictionary on each call was pure ObjC
// churn (CPU + autorelease-pool pressure). The set of distinct (size, style,
// color) combinations a UI actually uses is tiny (a few sizes × a theme palette),
// so we memoize the finished attribute dict. Keyed on everything that changes the
// dict — color rgba, size, bold, mono, and the global
// force-mono flag. The dictionary retains each cached attr dict (ARC), so they
// persist for the process; bounded by the number of distinct combinations.
static NSMutableDictionary<NSNumber*, NSDictionary*>* g_text_attr_cache = nil;

static NSDictionary* darwin_text_attrs(gg__TextCfg cfg) {
	uint64_t key = ((uint64_t)cfg.color.r)
		| ((uint64_t)cfg.color.g << 8)
		| ((uint64_t)cfg.color.b << 16)
		| ((uint64_t)cfg.color.a << 24)
		| (((uint64_t)cfg.size & 0xFFFF) << 32)
		| ((uint64_t)(cfg.bold ? 1 : 0) << 48)
		| ((uint64_t)(cfg.mono ? 1 : 0) << 49)
		| ((uint64_t)(g_gg_force_mono ? 1 : 0) << 50);
	if (g_text_attr_cache == nil) {
		g_text_attr_cache = [[NSMutableDictionary alloc] init];
	}
	NSNumber* k = @(key);
	NSDictionary* cached = [g_text_attr_cache objectForKey:k];
	if (cached != nil) {
		return cached;
	}

	NSFont* font = [NSFont userFontOfSize:cfg.size];
	// # NSFont*    font = [NSFont fontWithName:@"Roboto Mono" size:cfg.size];
	if (g_gg_force_mono) {
		// Global monospace mode: requested size minus 1, kept in sync with the
		// app-side width metric (see fuse_text_width / mono_char_advance).
		font = [NSFont fontWithName:@"Menlo" size:cfg.size - 1];
	} else if (cfg.mono) {
		// # font = [NSFont fontWithName:@"Roboto Mono" size:cfg.size];
		font = [NSFont fontWithName:@"Menlo" size:cfg.size - 5];
	}
	if (cfg.bold) {
		font = [[NSFontManager sharedFontManager] convertFont:font toHaveTrait:NSBoldFontMask];
	}

	NSDictionary* attr = @{
		NSForegroundColorAttributeName : nscolor(cfg.color),
		// NSParagraphStyleAttributeName: paragraphStyle,
		NSFontAttributeName : font,
	};
	[g_text_attr_cache setObject:attr forKey:k];
	return attr;
}

void darwin_draw_string(int x, int y, string s, gg__TextCfg cfg) {
	NSDictionary* attr = darwin_text_attrs(cfg);
	CFStringRef cf = cfstring(s);
	if (cf == nil) {
		return;
	}
	NSString* n = (__bridge NSString*)cf;
	[n drawAtPoint:NSMakePoint(x, y - 15) withAttributes:attr];
	CFRelease(cf);
}

int darwin_text_width(string s) {
	// println('text_width "${s}" len=${s.len}')
	NSString* n = @"";
	CFStringRef cf = nil;
	if (s.len == 1) {
		// println('len=1')
		n = [NSString stringWithFormat:@"%c", s.str[0]];
	} else {
		cf = cfstring(s);
		if (cf == nil) {
			return 0;
		}
		n = (__bridge NSString*)cf;
	}
	/*
	# if (!defaultFont){
	# defaultFont = [NSFont userFontOfSize: ui__DEFAULT_FONT_SIZE];
	# }
	# NSDictionary *attrs = @{
	# NSFontAttributeName: defaultFont,
	# };
	*/
	NSSize size = [n sizeWithAttributes:nil];
	if (cf != nil) {
		CFRelease(cf);
	}
	// # printf("!!!%f\n", ceil(size.width));
	return (int)(ceil(size.width));
}

void darwin_draw_rect(float x, float y, float width, float height, gg__Color c) {
	if (width <= 0 || height <= 0) {
		return;
	}
	CGContextRef context = current_cg_context();
	if (context == NULL) {
		return;
	}
	CGContextSetFillColorWithColor(context, cgcolor(c));
	CGContextFillRect(context, CGRectMake(x, y, width, height));
}

static void mark_view_tree_needs_display(NSView *view) {
	if (view == nil) {
		return;
	}
	[view setNeedsDisplay:YES];
	for (NSView *subview in [view subviews]) {
		mark_view_tree_needs_display(subview);
	}
}

void darwin_window_refresh() {
	dispatch_async(dispatch_get_main_queue(), ^{
		NSWindow *window = [NSApp mainWindow];
		if (window == nil) {
			window = [NSApp keyWindow];
		}
		if (window == nil) {
			return;
		}
		NSView *contentView = [window contentView];
		if (contentView == nil) {
			return;
		}
		mark_view_tree_needs_display(contentView);
		[window displayIfNeeded];
	});

	// puts("refresh");
	//[[NSApp mainWindow].contentView drawRect:NSMakeRect(0,0,2000,2000)];
	//[[NSGraphicsContext currentContext] flushGraphics];
}

gg__Image darwin_create_image(string path_) {
	// file = file.trim_space()
	NSString* path = nsstring(path_);
	NSImage* img = [[NSImage alloc] initWithContentsOfFile:path];
	if (img == 0) {
	}
	NSSize size = [img size];
	gg__Image res;
	res.width = size.width;
	res.height = size.height;
	res.path = path_;
	res.ok = true;
	// printf("inited img width=%d\n", res.width) ;
	//  need __bridge_retained so that the pointer is not freed by ARC
	res.data = (__bridge_retained voidptr)(img);
	return res;
}

void darwin_draw_image(float x, float y, float w, float h, gg__Image* img) {
	NSImage* i = (__bridge NSImage*)(img->data);
	[i drawInRect:NSMakeRect(x, y, w, h)];
}

void darwin_draw_circle(float x, float y, float d, gg__Color color) {
	if (d <= 0) {
		return;
	}
	CGContextRef context = current_cg_context();
	if (context == NULL) {
		return;
	}
	CGContextSetFillColorWithColor(context, cgcolor(color));
	CGContextFillEllipseInRect(context, CGRectMake(x, y, d * 2, d * 2));
}

void darwin_draw_circle_empty(float x, float y, float d, gg__Color color) {
	if (d <= 0) {
		return;
	}
	CGContextRef context = current_cg_context();
	if (context == NULL) {
		return;
	}
	CGContextSaveGState(context);
	CGContextSetStrokeColorWithColor(context, cgcolor(color));
	CGContextSetLineWidth(context, 1.0);
	CGContextStrokeEllipseInRect(context, CGRectMake(x, y, d * 2, d * 2));
	CGContextRestoreGState(context);
}
