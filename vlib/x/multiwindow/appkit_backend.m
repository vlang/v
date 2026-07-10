#include <Cocoa/Cocoa.h>
#include <Metal/Metal.h>
#include <QuartzCore/CAMetalLayer.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include "appkit_backend_helpers.h"

@interface VMultiwindowAppKitWindowState : NSObject <NSWindowDelegate>
@property(strong) NSWindow *window;
@property(strong) NSView *view;
@property(strong) CAMetalLayer *layer;
@property(strong) id<MTLTexture> depthTexture;
@property(strong) id<CAMetalDrawable> currentDrawable;
@property(assign) BOOL highDpi;
@property(assign) BOOL closeRequested;
@property(assign) BOOL destroyed;
@property(assign) BOOL resized;
@property(assign) int width;
@property(assign) int height;
@property(assign) int framebufferWidth;
@property(assign) int framebufferHeight;
- (void)updateDimensions;
- (BOOL)ensureDepthTextureWithDevice:(id<MTLDevice>)device;
@end

@implementation VMultiwindowAppKitWindowState
- (void)updateDimensions {
	NSView *content = self.window.contentView != nil ? self.window.contentView : self.view;
	NSRect bounds = content != nil ? content.bounds : NSMakeRect(0, 0, 1, 1);
	int logical_width = (int)lrint(NSWidth(bounds));
	int logical_height = (int)lrint(NSHeight(bounds));
	if (logical_width <= 0) {
		logical_width = 1;
	}
	if (logical_height <= 0) {
		logical_height = 1;
	}
	int framebuffer_width = logical_width;
	int framebuffer_height = logical_height;
	if (self.highDpi && content != nil) {
		NSRect backing = [content convertRectToBacking:bounds];
		framebuffer_width = (int)lrint(NSWidth(backing));
		framebuffer_height = (int)lrint(NSHeight(backing));
	}
	if (framebuffer_width <= 0) {
		framebuffer_width = 1;
	}
	if (framebuffer_height <= 0) {
		framebuffer_height = 1;
	}
	self.width = logical_width;
	self.height = logical_height;
	self.framebufferWidth = framebuffer_width;
	self.framebufferHeight = framebuffer_height;
	if (self.layer != nil) {
		CGFloat scale = 1.0;
		if (self.highDpi && self.window.screen != nil) {
			scale = self.window.screen.backingScaleFactor;
		}
		self.layer.contentsScale = scale;
		self.layer.drawableSize = CGSizeMake((CGFloat)framebuffer_width, (CGFloat)framebuffer_height);
	}
}

- (BOOL)ensureDepthTextureWithDevice:(id<MTLDevice>)device {
	if (device == nil || self.framebufferWidth <= 0 || self.framebufferHeight <= 0) {
		return NO;
	}
	if (self.depthTexture != nil &&
	    self.depthTexture.width == (NSUInteger)self.framebufferWidth &&
	    self.depthTexture.height == (NSUInteger)self.framebufferHeight) {
		return YES;
	}
	MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatDepth32Float_Stencil8
	                                                                                width:(NSUInteger)self.framebufferWidth
	                                                                               height:(NSUInteger)self.framebufferHeight
	                                                                            mipmapped:NO];
	desc.usage = MTLTextureUsageRenderTarget;
	desc.storageMode = MTLStorageModePrivate;
	self.depthTexture = [device newTextureWithDescriptor:desc];
	return self.depthTexture != nil;
}

- (BOOL)windowShouldClose:(id)sender {
	(void)sender;
	self.closeRequested = YES;
	return NO;
}

- (void)windowWillClose:(NSNotification *)notification {
	(void)notification;
	self.destroyed = YES;
}

- (void)windowDidResize:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height) {
		self.resized = YES;
	}
}

- (void)windowDidChangeBackingProperties:(NSNotification *)notification {
	(void)notification;
	[self updateDimensions];
	self.depthTexture = nil;
	self.resized = YES;
}

- (void)windowDidChangeScreen:(NSNotification *)notification {
	(void)notification;
	[self updateDimensions];
	self.depthTexture = nil;
	self.resized = YES;
}
@end

static NSString *v_multiwindow_appkit_string(const char *value) {
	if (value == NULL) {
		return @"";
	}
	return [NSString stringWithUTF8String:value] ?: @"";
}

static int v_multiwindow_appkit_max_int(int value, int minimum) {
	return minimum > 0 && value < minimum ? minimum : value;
}

int v_multiwindow_appkit_is_main_thread(void) {
	return [NSThread isMainThread] ? 1 : 0;
}

int v_multiwindow_appkit_prepare_application(void) {
	@autoreleasepool {
		if (![NSThread isMainThread]) {
			return 0;
		}
		[NSApplication sharedApplication];
		[NSApp finishLaunching];
		if (NSApp.activationPolicy == NSApplicationActivationPolicyProhibited) {
			[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
		}
		return 1;
	}
}

int v_multiwindow_appkit_create_metal_device(void **out_device) {
	@autoreleasepool {
		if (out_device == NULL || ![NSThread isMainThread]) {
			return 0;
		}
		id<MTLDevice> device = MTLCreateSystemDefaultDevice();
		if (device == nil) {
			*out_device = NULL;
			return 0;
		}
		*out_device = (void *)CFBridgingRetain(device);
		return 1;
	}
}

void v_multiwindow_appkit_release_metal_device(void *device) {
	if (device != NULL) {
		CFBridgingRelease(device);
	}
}

int v_multiwindow_appkit_create_window(void *device_ptr, const char *title, int width, int height, int min_width, int min_height, int resizable, int visible, int high_dpi, int borderless, int fullscreen, void **out_state, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height) {
	@autoreleasepool {
		if (out_state == NULL || ![NSThread isMainThread] || width <= 0 || height <= 0) {
			return 0;
		}
		NSWindowStyleMask style = borderless ? NSWindowStyleMaskBorderless :
			(NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable);
		if (resizable) {
			style |= NSWindowStyleMaskResizable;
		}
		int content_width = v_multiwindow_appkit_max_int(width, min_width);
		int content_height = v_multiwindow_appkit_max_int(height, min_height);
		NSRect rect = NSMakeRect(0, 0, (CGFloat)content_width, (CGFloat)content_height);
		NSWindow *window = [[NSWindow alloc] initWithContentRect:rect
		                                               styleMask:style
		                                                 backing:NSBackingStoreBuffered
		                                                   defer:NO];
		if (window == nil) {
			return 0;
		}
		window.releasedWhenClosed = NO;
		window.title = v_multiwindow_appkit_string(title);
		window.acceptsMouseMovedEvents = YES;
		window.restorable = NO;
		if (min_width > 0 || min_height > 0) {
			[window setMinSize:NSMakeSize((CGFloat)(min_width > 0 ? min_width : 1),
			                              (CGFloat)(min_height > 0 ? min_height : 1))];
		}
		NSView *view = [[NSView alloc] initWithFrame:rect];
		if (view == nil) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = [[VMultiwindowAppKitWindowState alloc] init];
		state.window = window;
		state.view = view;
		state.highDpi = high_dpi ? YES : NO;
		if (device_ptr != NULL) {
			id<MTLDevice> device = (__bridge id<MTLDevice>)device_ptr;
			CAMetalLayer *layer = [CAMetalLayer layer];
			if (layer == nil) {
				return 0;
			}
			layer.device = device;
			layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
			layer.framebufferOnly = YES;
			layer.opaque = YES;
			view.wantsLayer = YES;
			view.layer = layer;
			state.layer = layer;
		}
		window.contentView = view;
		window.delegate = state;
		[window center];
		[state updateDimensions];
		state.resized = NO;
		state.closeRequested = NO;
		state.destroyed = NO;
		if (visible) {
			[window makeKeyAndOrderFront:nil];
			[NSApp activateIgnoringOtherApps:YES];
		} else {
			[window orderOut:nil];
		}
		if (fullscreen) {
			[window toggleFullScreen:nil];
		}
		if (out_width != NULL) {
			*out_width = state.width;
		}
		if (out_height != NULL) {
			*out_height = state.height;
		}
		if (out_framebuffer_width != NULL) {
			*out_framebuffer_width = state.framebufferWidth;
		}
		if (out_framebuffer_height != NULL) {
			*out_framebuffer_height = state.framebufferHeight;
		}
		*out_state = (void *)CFBridgingRetain(state);
		return 1;
	}
}

void v_multiwindow_appkit_destroy_window(void *state_ptr) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) {
			return;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		state.currentDrawable = nil;
		state.depthTexture = nil;
		if (state.window != nil) {
			state.window.delegate = nil;
			[state.window orderOut:nil];
			[state.window close];
		}
		state.window = nil;
		state.view = nil;
		state.layer = nil;
		state.destroyed = YES;
	}
}

void v_multiwindow_appkit_release_window(void *state_ptr) {
	if (state_ptr != NULL) {
		CFBridgingRelease(state_ptr);
	}
}

int v_multiwindow_appkit_set_window_title(void *state_ptr, const char *title) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread]) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil) {
			return 0;
		}
		state.window.title = v_multiwindow_appkit_string(title);
		return 1;
	}
}

int v_multiwindow_appkit_resize_window(void *state_ptr, int width, int height, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread] || width <= 0 || height <= 0) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil) {
			return 0;
		}
		[state.window setContentSize:NSMakeSize((CGFloat)width, (CGFloat)height)];
		[state updateDimensions];
		state.depthTexture = nil;
		state.resized = NO;
		if (out_width != NULL) {
			*out_width = state.width;
		}
		if (out_height != NULL) {
			*out_height = state.height;
		}
		if (out_framebuffer_width != NULL) {
			*out_framebuffer_width = state.framebufferWidth;
		}
		if (out_framebuffer_height != NULL) {
			*out_framebuffer_height = state.framebufferHeight;
		}
		return 1;
	}
}

void v_multiwindow_appkit_poll_events(void) {
	@autoreleasepool {
		if (![NSThread isMainThread] || NSApp == nil) {
			return;
		}
		for (;;) {
			NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
			                                    untilDate:[NSDate distantPast]
			                                       inMode:NSDefaultRunLoopMode
			                                      dequeue:YES];
			if (event == nil) {
				break;
			}
			[NSApp sendEvent:event];
		}
		[NSApp updateWindows];
	}
}

int v_multiwindow_appkit_take_close_requested(void *state_ptr) {
	if (state_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	if (!state.closeRequested) {
		return 0;
	}
	state.closeRequested = NO;
	return 1;
}

int v_multiwindow_appkit_take_resized(void *state_ptr, int *out_width, int *out_height, int *out_framebuffer_width, int *out_framebuffer_height) {
	if (state_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	if (!state.resized) {
		return 0;
	}
	state.resized = NO;
	if (out_width != NULL) {
		*out_width = state.width;
	}
	if (out_height != NULL) {
		*out_height = state.height;
	}
	if (out_framebuffer_width != NULL) {
		*out_framebuffer_width = state.framebufferWidth;
	}
	if (out_framebuffer_height != NULL) {
		*out_framebuffer_height = state.framebufferHeight;
	}
	return 1;
}

int v_multiwindow_appkit_take_destroyed(void *state_ptr) {
	if (state_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	if (!state.destroyed) {
		return 0;
	}
	state.destroyed = NO;
	return 1;
}

static void v_multiwindow_appkit_clear_frame(void *state_ptr) {
	if (state_ptr == NULL || ![NSThread isMainThread]) {
		return;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	state.currentDrawable = nil;
}

int v_multiwindow_appkit_begin_frame(void *state_ptr, void *device_ptr, void **out_drawable, void **out_depth_texture, int *out_framebuffer_width, int *out_framebuffer_height) {
	@autoreleasepool {
		if (state_ptr == NULL || device_ptr == NULL || out_drawable == NULL || out_depth_texture == NULL ||
		    ![NSThread isMainThread]) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		id<MTLDevice> device = (__bridge id<MTLDevice>)device_ptr;
		if (state.layer == nil || device == nil) {
			return 0;
		}
		[state updateDimensions];
		if (![state ensureDepthTextureWithDevice:device]) {
			return 0;
		}
		state.currentDrawable = [state.layer nextDrawable];
		if (state.currentDrawable == nil) {
			return 0;
		}
		*out_drawable = (__bridge void *)state.currentDrawable;
		*out_depth_texture = (__bridge void *)state.depthTexture;
		if (out_framebuffer_width != NULL) {
			*out_framebuffer_width = state.framebufferWidth;
		}
		if (out_framebuffer_height != NULL) {
			*out_framebuffer_height = state.framebufferHeight;
		}
		return 1;
	}
}

void v_multiwindow_appkit_end_frame(void *state_ptr) {
	@autoreleasepool {
		v_multiwindow_appkit_clear_frame(state_ptr);
	}
}

void v_multiwindow_appkit_abort_frame(void *state_ptr) {
	@autoreleasepool {
		v_multiwindow_appkit_clear_frame(state_ptr);
	}
}
