#include <Cocoa/Cocoa.h>

NSColor* nscolor(gx__Color c) {
	float red = (float)c.r / 255.0f;
	float green = (float)c.g / 255.0f;
	float blue = (float)c.b / 255.0f;
	return [NSColor colorWithDeviceRed:red green:green blue:blue alpha:1.0f];
}

NSString* nsstring(string s) {
	return [[NSString alloc] initWithBytesNoCopy:s.str
	                                      length:s.len
	                                    encoding:NSUTF8StringEncoding
	                                freeWhenDone:false];
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

void darwin_draw_string(int x, int y, string s, gx__TextCfg cfg) {
	NSFont* font = [NSFont userFontOfSize:0]; // cfg.size];
	// # NSFont*    font = [NSFont fontWithName:@"Roboto Mono" size:cfg.size];
	if (cfg.mono) {
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
	[nsstring(s) drawAtPoint:NSMakePoint(x, y - 15) withAttributes:attr];
}

int darwin_text_width(string s) {
	// println('text_width "$s" len=$s.len')
	NSString* n = @"";
	if (s.len == 1) {
		// println('len=1')
		n = [NSString stringWithFormat:@"%c", s.str[0]];
	} else {
		n = nsstring(s);
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
	// # printf("!!!%f\n", ceil(size.width));
	return (int)(ceil(size.width));
}

void darwin_draw_rect(float x, float y, float width, float height, gx__Color c) {
	NSColor* color = nscolor(c);
	NSRect rect = NSMakeRect(x, y, width, height);
	[color setFill];
	NSRectFill(rect);
}

void darwin_window_refresh() {
	//[g_view setNeedsDisplay:YES];
	// update UI on the main thread TODO separate fn

	dispatch_async(dispatch_get_main_queue(), ^{
	  [g_view setNeedsDisplay:YES];
	});

	// puts("refresh");
	//[g_view drawRect:NSMakeRect(0,0,2000,2000)];
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

void darwin_draw_circle(float x, float y, float d, gx__Color color) {
	NSColor* c = nscolor(color);
	NSRect rect = NSMakeRect(x, y, d * 2, d * 2);
	NSBezierPath* circlePath = [NSBezierPath bezierPath];
	[circlePath appendBezierPathWithOvalInRect:rect];
	[c setFill];
	// [circlePath stroke];
	[circlePath fill];
	// NSRectFill(rect);
}

void darwin_draw_circle_empty(float x, float y, float d, gx__Color color) {
	NSColor* outlineColor = nscolor(color);
	CGFloat outlineWidth = 1.0; //2.0;

	NSRect rect = NSMakeRect(x, y, d * 2, d * 2);
	NSBezierPath* circlePath = [NSBezierPath bezierPath];
	[circlePath appendBezierPathWithOvalInRect:rect];

	[outlineColor setStroke];
	[circlePath setLineWidth:outlineWidth];
	[circlePath stroke];
}
