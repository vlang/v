#include <Cocoa/Cocoa.h>

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

