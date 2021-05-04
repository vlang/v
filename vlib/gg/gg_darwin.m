#include <Cocoa/Cocoa.h>

NSColor* nscolor(gx__Color c) {
        float red= (float)c.r / 255.0f;
        float green= (float)c.g / 255.0f;
        float blue= (float)c.b / 255.0f;
        return [NSColor colorWithDeviceRed:red green:green blue:blue alpha:1.0f];
}

NSString* nsstring(string s) {
        return [ [ NSString alloc ] initWithBytesNoCopy:s.str  length:s.len
          encoding:NSUTF8StringEncoding freeWhenDone: false];
}


gg__Size gg_get_screen_size() {
	NSScreen *screen = [NSScreen mainScreen];
	NSDictionary *description = [screen deviceDescription];
	NSSize displayPixelSize = [[description objectForKey:NSDeviceSize] sizeValue];
	CGSize displayPhysicalSize = CGDisplayScreenSize(
		[[description objectForKey:@"NSScreenNumber"] unsignedIntValue]);
	gg__Size res;
	res.width = displayPixelSize.width;
	res.height = displayPixelSize.height;
	return res;
}

void darwin_draw_string(int x, int y, string s, gx__TextCfg cfg) {
	 NSFont*       font = [NSFont userFontOfSize: 0]; //cfg.size];
 // # NSFont*    font = [NSFont fontWithName:@"Roboto Mono" size:cfg.size];
 if (cfg.mono) {
         // # font = [NSFont fontWithName:@"Roboto Mono" size:cfg.size];
         font = [NSFont fontWithName:@"Menlo" size:cfg.size-5];
 }
if (cfg.bold) {
        font = [[NSFontManager sharedFontManager] convertFont:font toHaveTrait:NSBoldFontMask];
}


	NSDictionary* attr = @{
NSForegroundColorAttributeName: nscolor(cfg.color),
//NSParagraphStyleAttributeName: paragraphStyle,
NSFontAttributeName: font,
};
	[nsstring(s) drawAtPoint:NSMakePoint(x,y-15) withAttributes:attr];
}

int darwin_text_width(string s) {
	// println('text_width "$s" len=$s.len')
	NSString* n = @"";
	if (s.len == 1) {
	        // println('len=1')
	       n=[NSString stringWithFormat:@"%c" , s.str[0]];
	}
	else {
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

	//puts("refresh");
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
	 res.width =  size.width;
	 res.height =  size.height;
	res.path = path_;
	res.ok = true;
	//printf("inited img width=%d\n", res.width) ;
	// need __brige_retained so that the pointer is not freed by ARC
	 res.data = (__bridge_retained voidptr)(img);
	return res;
}

void darwin_draw_image(float x, float y, float w, float h, gg__Image* img) {
	NSImage* i= (__bridge NSImage*)(img->data);
	[i drawInRect:NSMakeRect(x,y,w,h)];
}

void darwin_draw_circle(float x, float y, float d,  gx__Color color) {
	NSColor*        c = nscolor(color);
	NSRect        rect = NSMakeRect(x, y, d * 2, d * 2);
	NSBezierPath* circlePath = [NSBezierPath bezierPath];
	[circlePath appendBezierPathWithOvalInRect: rect];
	[c setFill];
	// [circlePath stroke];
	[circlePath fill];
	// NSRectFill(rect);
}

