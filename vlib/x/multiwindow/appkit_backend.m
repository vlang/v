#include <Cocoa/Cocoa.h>
#include <IOKit/hidsystem/IOLLEvent.h>
#include <Metal/Metal.h>
#include <QuartzCore/CAMetalLayer.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "appkit_backend_helpers.h"

#define V_MULTIWINDOW_CURSOR_SHAPE_DEFAULT 0
#define V_MULTIWINDOW_CURSOR_SHAPE_POINTER 1
#define V_MULTIWINDOW_CURSOR_SHAPE_MOVE 2
#define V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE 3
#define V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE 4
#define V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE 5
#define V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE 6
#define V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE 7
#define V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE 8
#define V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE 9
#define V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE 10
#define V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE 11
#define V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE 12
#define V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE 13
#define V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE 14
#define V_MULTIWINDOW_CURSOR_SHAPE_GRAB 15
#define V_MULTIWINDOW_CURSOR_SHAPE_GRABBING 16

@class VMultiwindowAppKitWindowState;

@interface VMultiwindowAppKitView : NSView <NSDraggingDestination>
@property(weak) VMultiwindowAppKitWindowState *state;
@property(strong) NSTrackingArea *trackingArea;
@end

@interface VMultiwindowAppKitWindowState : NSObject <NSWindowDelegate>
{
	BOOL keyDown[512];
}
@property(strong) NSWindow *window;
@property(strong) VMultiwindowAppKitView *view;
@property(strong) CAMetalLayer *layer;
@property(strong) id<MTLTexture> depthTexture;
@property(strong) id<CAMetalDrawable> currentDrawable;
@property(strong) NSMutableArray<NSValue *> *queuedEvents;
@property(assign) BOOL highDpi;
@property(assign) BOOL suppressResizeEvent;
@property(assign) uint32_t flagsChangedStore;
@property(assign) uint8_t mouseButtons;
@property(assign) float mouseX;
@property(assign) float mouseY;
@property(assign) float mouseDx;
@property(assign) float mouseDy;
@property(assign) BOOL mousePosValid;
@property(assign) int width;
@property(assign) int height;
@property(assign) int framebufferWidth;
@property(assign) int framebufferHeight;
@property(assign) int cursorShape;
- (void)updateDimensions;
- (BOOL)ensureDepthTextureWithDevice:(id<MTLDevice>)device;
- (void)queueLifecycleEvent:(int)kind;
- (void)queueResizeEvents;
- (void)queueInputEvent:(VMultiwindowAppKitQueuedEvent)event;
- (void)queueKeyEvent:(int)kind keyCode:(int)keyCode repeat:(BOOL)repeat modifiers:(uint32_t)modifiers;
- (void)queueCharEventsFromString:(NSString *)characters repeat:(BOOL)repeat modifiers:(uint32_t)modifiers;
- (void)queueTouchEvent:(int)kind changedPhase:(NSTouchPhase)phase event:(NSEvent *)event view:(NSView *)view;
- (void)updateMousePositionFromEvent:(NSEvent *)event clearDelta:(BOOL)clearDelta;
- (void)updateMousePositionFromWindowPoint:(NSPoint)point clearDelta:(BOOL)clearDelta;
- (void)setKeyDown:(BOOL)down forKeyCode:(int)keyCode;
- (void)clearKeyDownState;
- (void)clearQueuedEvents;
- (void)applyCursorShape;
@end

static uint64_t v_multiwindow_appkit_next_event_sequence(void) {
	static uint64_t sequence = 1;
	return sequence++;
}

static VMultiwindowAppKitQueuedEvent v_multiwindow_appkit_zero_event(void) {
	VMultiwindowAppKitQueuedEvent event = {0};
	event.mouse_button = V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID;
	return event;
}

static NSCursor *v_multiwindow_appkit_cursor_for_shape(int shape) {
	switch (shape) {
	case V_MULTIWINDOW_CURSOR_SHAPE_POINTER:
		return [NSCursor pointingHandCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_MOVE:
	case V_MULTIWINDOW_CURSOR_SHAPE_GRAB:
		return [NSCursor openHandCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_GRABBING:
		return [NSCursor closedHandCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_N_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_S_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NS_RESIZE:
		return [NSCursor resizeUpDownCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_E_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_W_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_EW_RESIZE:
		return [NSCursor resizeLeftRightCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_NE_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_SW_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NESW_RESIZE:
		return [NSCursor resizeUpDownCursor];
	case V_MULTIWINDOW_CURSOR_SHAPE_NW_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_SE_RESIZE:
	case V_MULTIWINDOW_CURSOR_SHAPE_NWSE_RESIZE:
		return [NSCursor resizeLeftRightCursor];
	default:
		return [NSCursor arrowCursor];
	}
}

static char *v_multiwindow_appkit_copy_cstring(const char *value) {
	if (value == NULL) {
		return NULL;
	}
	size_t len = strlen(value);
	char *copy = (char *)calloc(len + 1, 1);
	if (copy == NULL) {
		return NULL;
	}
	memcpy(copy, value, len);
	return copy;
}

void v_multiwindow_appkit_release_queued_event_resources(VMultiwindowAppKitQueuedEvent *event) {
	if (event == NULL || event->dropped_files == NULL) {
		return;
	}
	for (int i = 0; i < event->dropped_file_count; i++) {
		free(event->dropped_files[i]);
	}
	free(event->dropped_files);
	event->dropped_files = NULL;
	event->dropped_file_count = 0;
}

static uint32_t v_multiwindow_appkit_modifiers(NSEvent *event) {
	NSEventModifierFlags flags = event != nil ? event.modifierFlags : NSEvent.modifierFlags;
	NSUInteger buttons = NSEvent.pressedMouseButtons;
	uint32_t modifiers = 0;
	if (flags & NSEventModifierFlagShift) {
		modifiers |= 1;
	}
	if (flags & NSEventModifierFlagControl) {
		modifiers |= 2;
	}
	if (flags & NSEventModifierFlagOption) {
		modifiers |= 4;
	}
	if (flags & NSEventModifierFlagCommand) {
		modifiers |= 8;
	}
	if (buttons & (1u << 0)) {
		modifiers |= V_MULTIWINDOW_APPKIT_MODIFIER_LMB;
	}
	if (buttons & (1u << 1)) {
		modifiers |= V_MULTIWINDOW_APPKIT_MODIFIER_RMB;
	}
	if (buttons & (1u << 2)) {
		modifiers |= V_MULTIWINDOW_APPKIT_MODIFIER_MMB;
	}
	return modifiers;
}

static const int *v_multiwindow_appkit_keycodes(void) {
	static int keycodes[128];
	static int initialized = 0;
	if (initialized) {
		return keycodes;
	}
	initialized = 1;
	keycodes[0x1D] = 48;
	keycodes[0x12] = 49;
	keycodes[0x13] = 50;
	keycodes[0x14] = 51;
	keycodes[0x15] = 52;
	keycodes[0x17] = 53;
	keycodes[0x16] = 54;
	keycodes[0x1A] = 55;
	keycodes[0x1C] = 56;
	keycodes[0x19] = 57;
	keycodes[0x00] = 65;
	keycodes[0x0B] = 66;
	keycodes[0x08] = 67;
	keycodes[0x02] = 68;
	keycodes[0x0E] = 69;
	keycodes[0x03] = 70;
	keycodes[0x05] = 71;
	keycodes[0x04] = 72;
	keycodes[0x22] = 73;
	keycodes[0x26] = 74;
	keycodes[0x28] = 75;
	keycodes[0x25] = 76;
	keycodes[0x2E] = 77;
	keycodes[0x2D] = 78;
	keycodes[0x1F] = 79;
	keycodes[0x23] = 80;
	keycodes[0x0C] = 81;
	keycodes[0x0F] = 82;
	keycodes[0x01] = 83;
	keycodes[0x11] = 84;
	keycodes[0x20] = 85;
	keycodes[0x09] = 86;
	keycodes[0x0D] = 87;
	keycodes[0x07] = 88;
	keycodes[0x10] = 89;
	keycodes[0x06] = 90;
	keycodes[0x27] = 39;
	keycodes[0x2A] = 92;
	keycodes[0x2B] = 44;
	keycodes[0x18] = 61;
	keycodes[0x32] = 96;
	keycodes[0x21] = 91;
	keycodes[0x1B] = 45;
	keycodes[0x2F] = 46;
	keycodes[0x1E] = 93;
	keycodes[0x29] = 59;
	keycodes[0x2C] = 47;
	keycodes[0x0A] = 161;
	keycodes[0x33] = 259;
	keycodes[0x39] = 280;
	keycodes[0x75] = 261;
	keycodes[0x7D] = 264;
	keycodes[0x77] = 269;
	keycodes[0x24] = 257;
	keycodes[0x35] = 256;
	keycodes[0x7A] = 290;
	keycodes[0x78] = 291;
	keycodes[0x63] = 292;
	keycodes[0x76] = 293;
	keycodes[0x60] = 294;
	keycodes[0x61] = 295;
	keycodes[0x62] = 296;
	keycodes[0x64] = 297;
	keycodes[0x65] = 298;
	keycodes[0x6D] = 299;
	keycodes[0x67] = 300;
	keycodes[0x6F] = 301;
	keycodes[0x69] = 302;
	keycodes[0x6B] = 303;
	keycodes[0x71] = 304;
	keycodes[0x6A] = 305;
	keycodes[0x40] = 306;
	keycodes[0x4F] = 307;
	keycodes[0x50] = 308;
	keycodes[0x5A] = 309;
	keycodes[0x73] = 268;
	keycodes[0x72] = 260;
	keycodes[0x7B] = 263;
	keycodes[0x3A] = 342;
	keycodes[0x3B] = 341;
	keycodes[0x38] = 340;
	keycodes[0x37] = 343;
	keycodes[0x6E] = 348;
	keycodes[0x47] = 282;
	keycodes[0x79] = 267;
	keycodes[0x74] = 266;
	keycodes[0x7C] = 262;
	keycodes[0x3D] = 346;
	keycodes[0x3E] = 345;
	keycodes[0x3C] = 344;
	keycodes[0x36] = 347;
	keycodes[0x31] = 32;
	keycodes[0x30] = 258;
	keycodes[0x7E] = 265;
	keycodes[0x52] = 320;
	keycodes[0x53] = 321;
	keycodes[0x54] = 322;
	keycodes[0x55] = 323;
	keycodes[0x56] = 324;
	keycodes[0x57] = 325;
	keycodes[0x58] = 326;
	keycodes[0x59] = 327;
	keycodes[0x5B] = 328;
	keycodes[0x5C] = 329;
	keycodes[0x45] = 334;
	keycodes[0x41] = 330;
	keycodes[0x4B] = 331;
	keycodes[0x4C] = 335;
	keycodes[0x51] = 336;
	keycodes[0x43] = 332;
	keycodes[0x4E] = 333;
	return keycodes;
}

static int v_multiwindow_appkit_key_code(unsigned short native_key_code) {
	const int *keycodes = v_multiwindow_appkit_keycodes();
	if (native_key_code < 128) {
		return keycodes[native_key_code];
	}
	return 0;
}

static NSEventModifierFlags v_multiwindow_appkit_modifier_flag_for_key_code(int key_code) {
	switch (key_code) {
	case 280:
		return NSEventModifierFlagCapsLock;
	case 340:
	case 344:
		return NSEventModifierFlagShift;
	case 341:
	case 345:
		return NSEventModifierFlagControl;
	case 342:
	case 346:
		return NSEventModifierFlagOption;
	case 343:
	case 347:
		return NSEventModifierFlagCommand;
	default:
		return 0;
	}
}

static BOOL v_multiwindow_appkit_modifier_key_is_pressed(NSEventModifierFlags flags, int key_code, BOOL *out_pressed) {
	if (out_pressed == NULL) {
		return NO;
	}
	switch (key_code) {
	case 340:
		*out_pressed = (flags & NX_DEVICELSHIFTKEYMASK) != 0;
		return YES;
	case 344:
		*out_pressed = (flags & NX_DEVICERSHIFTKEYMASK) != 0;
		return YES;
	case 341:
		*out_pressed = (flags & NX_DEVICELCTLKEYMASK) != 0;
		return YES;
	case 345:
		*out_pressed = (flags & NX_DEVICERCTLKEYMASK) != 0;
		return YES;
	case 342:
		*out_pressed = (flags & NX_DEVICELALTKEYMASK) != 0;
		return YES;
	case 346:
		*out_pressed = (flags & NX_DEVICERALTKEYMASK) != 0;
		return YES;
	case 343:
		*out_pressed = (flags & NX_DEVICELCMDKEYMASK) != 0;
		return YES;
	case 347:
		*out_pressed = (flags & NX_DEVICERCMDKEYMASK) != 0;
		return YES;
	default:
		break;
	}
	NSEventModifierFlags modifierFlag = v_multiwindow_appkit_modifier_flag_for_key_code(key_code);
	if (modifierFlag == 0) {
		return NO;
	}
	*out_pressed = (flags & modifierFlag) != 0;
	return YES;
}

static BOOL v_multiwindow_appkit_touch_sets_share_identity(NSSet<NSTouch *> *touches, NSTouch *target) {
	if (target == nil) {
		return NO;
	}
	id targetIdentity = target.identity;
	for (NSTouch *touch in touches) {
		if (touch == target) {
			return YES;
		}
		id identity = touch.identity;
		if (identity != nil && targetIdentity != nil && identity == targetIdentity) {
			return YES;
		}
		if (identity != nil && targetIdentity != nil && [identity isEqual:targetIdentity]) {
			return YES;
		}
	}
	return NO;
}

static float v_multiwindow_appkit_clamp_unit(CGFloat value) {
	if (value < 0.0) {
		return 0.0f;
	}
	if (value > 1.0) {
		return 1.0f;
	}
	return (float)value;
}

static void v_multiwindow_appkit_fill_touch_point(VMultiwindowAppKitWindowState *state, VMultiwindowAppKitQueuedEvent *event, int index, NSTouch *touch, BOOL changed) {
	if (state == nil || event == NULL || touch == nil || index < 0 || index >= V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS) {
		return;
	}
	id identity = touch.identity;
	const void *identity_ptr = identity != nil ? (__bridge const void *)identity : (__bridge const void *)touch;
	float width = state.framebufferWidth > 1 ? (float)(state.framebufferWidth - 1) : 0.0f;
	float height = state.framebufferHeight > 1 ? (float)(state.framebufferHeight - 1) : 0.0f;
	NSPoint normalized = touch.normalizedPosition;
	float x = v_multiwindow_appkit_clamp_unit(normalized.x) * width;
	float y = (1.0f - v_multiwindow_appkit_clamp_unit(normalized.y)) * height;
	event->touch_ids[index] = (uint64_t)(uintptr_t)identity_ptr;
	event->touch_x[index] = x;
	event->touch_y[index] = y;
	event->touch_changed[index] = changed ? 1 : 0;
}

@implementation VMultiwindowAppKitWindowState
- (instancetype)init {
	self = [super init];
	if (self != nil) {
		self.queuedEvents = [NSMutableArray array];
	}
	return self;
}

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

- (void)fillDimensionsForEvent:(VMultiwindowAppKitQueuedEvent *)event {
	if (event == NULL) {
		return;
	}
	event->window_width = self.width;
	event->window_height = self.height;
	event->framebuffer_width = self.framebufferWidth;
	event->framebuffer_height = self.framebufferHeight;
}

- (void)queueEvent:(VMultiwindowAppKitQueuedEvent)event {
	if (self.queuedEvents == nil) {
		self.queuedEvents = [NSMutableArray array];
	}
	event.sequence = v_multiwindow_appkit_next_event_sequence();
	VMultiwindowAppKitQueuedEvent *queued = calloc(1, sizeof(VMultiwindowAppKitQueuedEvent));
	if (queued == NULL) {
		v_multiwindow_appkit_release_queued_event_resources(&event);
		return;
	}
	*queued = event;
	[self.queuedEvents addObject:[NSValue valueWithPointer:queued]];
}

- (void)queueLifecycleEvent:(int)kind {
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.event_kind = V_MULTIWINDOW_APPKIT_EVENT_LIFECYCLE;
	event.lifecycle_kind = kind;
	[self fillDimensionsForEvent:&event];
	[self queueEvent:event];
}

- (void)queueResizeEvents {
	[self queueLifecycleEvent:V_MULTIWINDOW_APPKIT_LIFECYCLE_RESIZED];
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.event_kind = V_MULTIWINDOW_APPKIT_EVENT_INPUT;
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_RESIZED;
	[self fillDimensionsForEvent:&event];
	[self queueEvent:event];
}

- (void)queueInputEvent:(VMultiwindowAppKitQueuedEvent)event {
	event.event_kind = V_MULTIWINDOW_APPKIT_EVENT_INPUT;
	[self fillDimensionsForEvent:&event];
	[self queueEvent:event];
}

- (void)queueKeyEvent:(int)kind keyCode:(int)keyCode repeat:(BOOL)repeat modifiers:(uint32_t)modifiers {
	if (keyCode == 0) {
		return;
	}
	if (keyCode >= 0 && keyCode < 512) {
		keyDown[keyCode] = kind == V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN;
	}
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = kind;
	event.key_code = keyCode;
	event.key_repeat = repeat ? 1 : 0;
	event.modifiers = modifiers;
	[self queueInputEvent:event];
}

- (void)queueCharEventsFromString:(NSString *)characters repeat:(BOOL)repeat modifiers:(uint32_t)modifiers {
	NSUInteger length = characters.length;
	for (NSUInteger i = 0; i < length; i++) {
		unichar unit = [characters characterAtIndex:i];
		uint32_t codepoint = unit;
		if (unit >= 0xD800 && unit <= 0xDBFF) {
			if ((i + 1) >= length) {
				continue;
			}
			unichar low = [characters characterAtIndex:i + 1];
			if (low < 0xDC00 || low > 0xDFFF) {
				continue;
			}
			codepoint = 0x10000u + (((uint32_t)unit - 0xD800u) << 10) + ((uint32_t)low - 0xDC00u);
			i++;
		} else if (unit >= 0xDC00 && unit <= 0xDFFF) {
			continue;
		}
		if ((codepoint & 0xFF00u) == 0xF700u) {
			continue;
		}
		VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
		event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_CHAR;
		event.char_code = codepoint;
		event.key_repeat = repeat ? 1 : 0;
		event.modifiers = modifiers;
		[self queueInputEvent:event];
	}
}

- (void)queueTouchEvent:(int)kind changedPhase:(NSTouchPhase)phase event:(NSEvent *)event view:(NSView *)view {
	if (event == nil || view == nil) {
		return;
	}
	NSSet<NSTouch *> *changedTouches = [event touchesMatchingPhase:phase inView:view];
	NSSet<NSTouch *> *allTouches = [event touchesMatchingPhase:NSTouchPhaseAny inView:view];
	if (allTouches.count == 0) {
		allTouches = changedTouches;
	}
	if (allTouches.count == 0) {
		return;
	}
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = kind;
	queued.modifiers = v_multiwindow_appkit_modifiers(event);
	int index = 0;
	for (NSTouch *touch in allTouches) {
		if (index >= V_MULTIWINDOW_APPKIT_MAX_TOUCH_POINTS) {
			break;
		}
		BOOL changed = v_multiwindow_appkit_touch_sets_share_identity(changedTouches, touch);
		v_multiwindow_appkit_fill_touch_point(self, &queued, index, touch, changed);
		index++;
	}
	if (index == 0) {
		return;
	}
	queued.touch_count = index;
	queued.mouse_x = queued.touch_x[0];
	queued.mouse_y = queued.touch_y[0];
	[self queueInputEvent:queued];
}

- (void)updateMousePositionFromEvent:(NSEvent *)event clearDelta:(BOOL)clearDelta {
	NSPoint point = event.locationInWindow;
	[self updateMousePositionFromWindowPoint:point clearDelta:clearDelta];
}

- (void)updateMousePositionFromWindowPoint:(NSPoint)point clearDelta:(BOOL)clearDelta {
	NSView *content = self.window.contentView != nil ? self.window.contentView : self.view;
	if (content != nil) {
		point = [content convertPoint:point fromView:nil];
	}
	NSRect bounds = content != nil ? content.bounds : NSMakeRect(0, 0, self.width, self.height);
	CGFloat logicalWidth = NSWidth(bounds) > 0 ? NSWidth(bounds) : (CGFloat)self.width;
	CGFloat logicalHeight = NSHeight(bounds) > 0 ? NSHeight(bounds) : (CGFloat)self.height;
	CGFloat scaleX = logicalWidth > 0 ? ((CGFloat)self.framebufferWidth / logicalWidth) : 1.0;
	CGFloat scaleY = logicalHeight > 0 ? ((CGFloat)self.framebufferHeight / logicalHeight) : 1.0;
	float newX = (float)(point.x * scaleX);
	float newY = (float)((CGFloat)self.framebufferHeight - (point.y * scaleY) - 1.0);
	if (clearDelta || !self.mousePosValid) {
		self.mouseDx = 0.0f;
		self.mouseDy = 0.0f;
	} else {
		self.mouseDx = newX - self.mouseX;
		self.mouseDy = newY - self.mouseY;
	}
	self.mouseX = newX;
	self.mouseY = newY;
	self.mousePosValid = YES;
}

- (void)setKeyDown:(BOOL)down forKeyCode:(int)keyCode {
	if (keyCode < 0 || keyCode >= 512) {
		return;
	}
	keyDown[keyCode] = down;
}

- (void)clearKeyDownState {
	for (NSUInteger i = 0; i < sizeof(keyDown) / sizeof(keyDown[0]); i++) {
		keyDown[i] = NO;
	}
}

- (void)clearQueuedEvents {
	for (NSValue *value in self.queuedEvents) {
		VMultiwindowAppKitQueuedEvent *event = (VMultiwindowAppKitQueuedEvent *)value.pointerValue;
		v_multiwindow_appkit_release_queued_event_resources(event);
		free(event);
	}
	[self.queuedEvents removeAllObjects];
}

- (void)applyCursorShape {
	[v_multiwindow_appkit_cursor_for_shape(self.cursorShape) set];
}

- (BOOL)windowShouldClose:(id)sender {
	(void)sender;
	[self queueLifecycleEvent:V_MULTIWINDOW_APPKIT_LIFECYCLE_CLOSE_REQUESTED];
	return NO;
}

- (void)windowWillClose:(NSNotification *)notification {
	(void)notification;
	self.currentDrawable = nil;
	self.depthTexture = nil;
	[self queueLifecycleEvent:V_MULTIWINDOW_APPKIT_LIFECYCLE_DESTROYED];
}

- (void)windowDidResize:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	int old_framebuffer_width = self.framebufferWidth;
	int old_framebuffer_height = self.framebufferHeight;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height ||
	    self.framebufferWidth != old_framebuffer_width ||
	    self.framebufferHeight != old_framebuffer_height) {
		if (!self.suppressResizeEvent) {
			[self queueResizeEvents];
		}
	}
}

- (void)windowDidChangeBackingProperties:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	int old_framebuffer_width = self.framebufferWidth;
	int old_framebuffer_height = self.framebufferHeight;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height ||
	    self.framebufferWidth != old_framebuffer_width ||
	    self.framebufferHeight != old_framebuffer_height) {
		if (!self.suppressResizeEvent) {
			[self queueResizeEvents];
		}
	}
}

- (void)windowDidChangeScreen:(NSNotification *)notification {
	(void)notification;
	int old_width = self.width;
	int old_height = self.height;
	int old_framebuffer_width = self.framebufferWidth;
	int old_framebuffer_height = self.framebufferHeight;
	[self updateDimensions];
	self.depthTexture = nil;
	if (self.width != old_width || self.height != old_height ||
	    self.framebufferWidth != old_framebuffer_width ||
	    self.framebufferHeight != old_framebuffer_height) {
		if (!self.suppressResizeEvent) {
			[self queueResizeEvents];
		}
	}
}

- (void)windowDidBecomeKey:(NSNotification *)notification {
	(void)notification;
	[self clearKeyDownState];
	self.flagsChangedStore = (uint32_t)NSEvent.modifierFlags;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_FOCUSED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}

- (void)windowDidResignKey:(NSNotification *)notification {
	(void)notification;
	[self clearKeyDownState];
	self.flagsChangedStore = (uint32_t)NSEvent.modifierFlags;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_UNFOCUSED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}

- (void)windowDidMiniaturize:(NSNotification *)notification {
	(void)notification;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_ICONIFIED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}

- (void)windowDidDeminiaturize:(NSNotification *)notification {
	(void)notification;
	VMultiwindowAppKitQueuedEvent event = v_multiwindow_appkit_zero_event();
	event.input_kind = V_MULTIWINDOW_APPKIT_INPUT_RESTORED;
	event.modifiers = v_multiwindow_appkit_modifiers(nil);
	[self queueInputEvent:event];
}
@end

@implementation VMultiwindowAppKitView
- (BOOL)acceptsFirstResponder {
	return YES;
}

- (BOOL)canBecomeKeyView {
	return YES;
}

- (BOOL)acceptsFirstMouse:(NSEvent *)event {
	(void)event;
	return YES;
}

- (void)updateTrackingAreas {
	if (self.trackingArea != nil) {
		[self removeTrackingArea:self.trackingArea];
		self.trackingArea = nil;
	}
	NSTrackingAreaOptions options = NSTrackingMouseEnteredAndExited |
		NSTrackingActiveInKeyWindow |
		NSTrackingEnabledDuringMouseDrag |
		NSTrackingCursorUpdate |
		NSTrackingInVisibleRect |
		NSTrackingAssumeInside;
	self.trackingArea = [[NSTrackingArea alloc] initWithRect:self.bounds
	                                                 options:options
	                                                   owner:self
	                                                userInfo:nil];
	[self addTrackingArea:self.trackingArea];
	[super updateTrackingAreas];
}

- (void)cursorUpdate:(NSEvent *)event {
	(void)event;
	VMultiwindowAppKitWindowState *state = self.state;
	if (state != nil) {
		[state applyCursorShape];
	}
}

- (void)queueMouseEvent:(NSEvent *)event kind:(int)kind button:(int)button clearDelta:(BOOL)clearDelta {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	[state updateMousePositionFromEvent:event clearDelta:clearDelta];
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = kind;
	queued.modifiers = v_multiwindow_appkit_modifiers(event);
	queued.mouse_button = button;
	queued.mouse_x = state.mouseX;
	queued.mouse_y = state.mouseY;
	queued.mouse_dx = state.mouseDx;
	queued.mouse_dy = state.mouseDy;
	[state queueInputEvent:queued];
}

- (NSDragOperation)draggingEntered:(id<NSDraggingInfo>)sender {
	NSPasteboard *pasteboard = sender.draggingPasteboard;
	if ([pasteboard canReadObjectForClasses:@[ NSURL.class ]
	                                options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }]) {
		return NSDragOperationCopy;
	}
	return NSDragOperationNone;
}

- (NSDragOperation)draggingUpdated:(id<NSDraggingInfo>)sender {
	return [self draggingEntered:sender];
}

- (BOOL)performDragOperation:(id<NSDraggingInfo>)sender {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return NO;
	}
	NSPasteboard *pasteboard = sender.draggingPasteboard;
	NSArray<NSURL *> *urls = [pasteboard readObjectsForClasses:@[ NSURL.class ]
	                                                   options:@{ NSPasteboardURLReadingFileURLsOnlyKey: @YES }];
	if (urls.count == 0) {
		return NO;
	}
	[state updateMousePositionFromWindowPoint:sender.draggingLocation clearDelta:YES];
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = V_MULTIWINDOW_APPKIT_INPUT_FILES_DROPPED;
	queued.modifiers = v_multiwindow_appkit_modifiers(nil);
	queued.mouse_x = state.mouseX;
	queued.mouse_y = state.mouseY;
	queued.dropped_files = (char **)calloc(urls.count, sizeof(char *));
	if (queued.dropped_files == NULL) {
		return NO;
	}
	for (NSURL *url in urls) {
		if (!url.fileURL) {
			continue;
		}
		const char *path = url.standardizedURL.path.UTF8String;
		char *copy = v_multiwindow_appkit_copy_cstring(path);
		if (copy == NULL) {
			continue;
		}
		queued.dropped_files[queued.dropped_file_count] = copy;
		queued.dropped_file_count++;
	}
	if (queued.dropped_file_count == 0) {
		v_multiwindow_appkit_release_queued_event_resources(&queued);
		return NO;
	}
	[state queueInputEvent:queued];
	return YES;
}

- (void)mouseEntered:(NSEvent *)event {
	if (self.state.mouseButtons == 0) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_ENTER button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:YES];
	}
}

- (void)mouseExited:(NSEvent *)event {
	if (self.state.mouseButtons == 0) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_LEAVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:YES];
	}
}

- (void)mouseDown:(NSEvent *)event {
	[self.window makeFirstResponder:self];
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT clearDelta:NO];
	self.state.mouseButtons |= (1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT);
}

- (void)mouseUp:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT clearDelta:NO];
	self.state.mouseButtons &= ~(1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_LEFT);
}

- (void)rightMouseDown:(NSEvent *)event {
	[self.window makeFirstResponder:self];
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT clearDelta:NO];
	self.state.mouseButtons |= (1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT);
}

- (void)rightMouseUp:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT clearDelta:NO];
	self.state.mouseButtons &= ~(1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_RIGHT);
}

- (void)otherMouseDown:(NSEvent *)event {
	if (event.buttonNumber == 2) {
		[self.window makeFirstResponder:self];
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_DOWN button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE clearDelta:NO];
		self.state.mouseButtons |= (1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE);
	}
}

- (void)otherMouseUp:(NSEvent *)event {
	if (event.buttonNumber == 2) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_UP button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE clearDelta:NO];
		self.state.mouseButtons &= ~(1u << V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_MIDDLE);
	}
}

- (void)mouseMoved:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
}

- (void)mouseDragged:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
}

- (void)rightMouseDragged:(NSEvent *)event {
	[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
}

- (void)otherMouseDragged:(NSEvent *)event {
	if (event.buttonNumber == 2) {
		[self queueMouseEvent:event kind:V_MULTIWINDOW_APPKIT_INPUT_MOUSE_MOVE button:V_MULTIWINDOW_APPKIT_MOUSE_BUTTON_INVALID clearDelta:NO];
	}
}

- (void)scrollWheel:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	[state updateMousePositionFromEvent:event clearDelta:YES];
	float dx = (float)event.scrollingDeltaX;
	float dy = (float)event.scrollingDeltaY;
	if (event.hasPreciseScrollingDeltas) {
		dx *= 0.1f;
		dy *= 0.1f;
	}
	if (dx == 0.0f && dy == 0.0f) {
		return;
	}
	VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
	queued.input_kind = V_MULTIWINDOW_APPKIT_INPUT_MOUSE_SCROLL;
	queued.modifiers = v_multiwindow_appkit_modifiers(event);
	queued.mouse_x = state.mouseX;
	queued.mouse_y = state.mouseY;
	queued.scroll_x = dx;
	queued.scroll_y = dy;
	[state queueInputEvent:queued];
}

- (void)touchesBeganWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_BEGAN
	               changedPhase:NSTouchPhaseBegan
	                      event:event
	                       view:self];
}

- (void)touchesMovedWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_MOVED
	               changedPhase:NSTouchPhaseMoved
	                      event:event
	                       view:self];
}

- (void)touchesEndedWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_ENDED
	               changedPhase:NSTouchPhaseEnded
	                      event:event
	                       view:self];
}

- (void)touchesCancelledWithEvent:(NSEvent *)event {
	[self.state queueTouchEvent:V_MULTIWINDOW_APPKIT_INPUT_TOUCHES_CANCELLED
	               changedPhase:NSTouchPhaseCancelled
	                      event:event
	                       view:self];
}

- (void)keyDown:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	uint32_t modifiers = v_multiwindow_appkit_modifiers(event);
	int keyCode = v_multiwindow_appkit_key_code(event.keyCode);
	[state queueKeyEvent:V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN keyCode:keyCode repeat:event.isARepeat modifiers:modifiers];
	if ((modifiers & 8u) == 0) {
		[state queueCharEventsFromString:event.characters repeat:event.isARepeat modifiers:modifiers];
	}
	if (modifiers == 8 && keyCode == 86) {
		VMultiwindowAppKitQueuedEvent queued = v_multiwindow_appkit_zero_event();
		queued.input_kind = V_MULTIWINDOW_APPKIT_INPUT_CLIPBOARD_PASTED;
		queued.modifiers = modifiers;
		[state queueInputEvent:queued];
	}
}

- (BOOL)performKeyEquivalent:(NSEvent *)event {
	if (v_multiwindow_appkit_key_code(event.keyCode) == 86 &&
	    v_multiwindow_appkit_modifiers(event) == 8) {
		[self keyDown:event];
		return YES;
	}
	if (v_multiwindow_appkit_key_code(event.keyCode) == 258) {
		[self keyDown:event];
		return YES;
	}
	return NO;
}

- (void)keyUp:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	int keyCode = v_multiwindow_appkit_key_code(event.keyCode);
	[state queueKeyEvent:V_MULTIWINDOW_APPKIT_INPUT_KEY_UP keyCode:keyCode repeat:event.isARepeat modifiers:v_multiwindow_appkit_modifiers(event)];
}

- (void)flagsChanged:(NSEvent *)event {
	VMultiwindowAppKitWindowState *state = self.state;
	if (state == nil) {
		return;
	}
	uint32_t oldFlags = state.flagsChangedStore;
	uint32_t newFlags = (uint32_t)event.modifierFlags;
	state.flagsChangedStore = newFlags;
	int keyCode = v_multiwindow_appkit_key_code(event.keyCode);
	BOOL wasPressed = NO;
	BOOL isPressed = NO;
	if (keyCode == 0 ||
	    !v_multiwindow_appkit_modifier_key_is_pressed((NSEventModifierFlags)oldFlags, keyCode, &wasPressed) ||
	    !v_multiwindow_appkit_modifier_key_is_pressed((NSEventModifierFlags)newFlags, keyCode, &isPressed)) {
		return;
	}

	if (wasPressed == isPressed) {
		[state setKeyDown:isPressed forKeyCode:keyCode];
		return;
	}
	[state queueKeyEvent:isPressed ? V_MULTIWINDOW_APPKIT_INPUT_KEY_DOWN : V_MULTIWINDOW_APPKIT_INPUT_KEY_UP
	             keyCode:keyCode
	              repeat:NO
	           modifiers:v_multiwindow_appkit_modifiers(event)];
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
		VMultiwindowAppKitView *view = [[VMultiwindowAppKitView alloc] initWithFrame:rect];
		if (view == nil) {
			return 0;
		}
		view.acceptsTouchEvents = YES;
		VMultiwindowAppKitWindowState *state = [[VMultiwindowAppKitWindowState alloc] init];
		state.window = window;
		state.view = view;
		view.state = state;
		[view registerForDraggedTypes:@[ NSPasteboardTypeFileURL ]];
		state.highDpi = high_dpi ? YES : NO;
		state.flagsChangedStore = (uint32_t)NSEvent.modifierFlags;
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
		[window makeFirstResponder:view];
		[window center];
		[state updateDimensions];
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
		[state clearQueuedEvents];
		if (state.window != nil) {
			state.window.delegate = nil;
			[state.window orderOut:nil];
			[state.window close];
		}
		state.view.state = nil;
		state.window = nil;
		state.view = nil;
		state.layer = nil;
	}
}

void v_multiwindow_appkit_release_window(void *state_ptr) {
	if (state_ptr != NULL) {
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		[state clearQueuedEvents];
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

int v_multiwindow_appkit_set_cursor_shape(void *state_ptr, int shape) {
	@autoreleasepool {
		if (state_ptr == NULL || ![NSThread isMainThread] ||
		    shape < V_MULTIWINDOW_CURSOR_SHAPE_DEFAULT ||
		    shape > V_MULTIWINDOW_CURSOR_SHAPE_GRABBING) {
			return 0;
		}
		VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
		if (state.window == nil) {
			return 0;
		}
		state.cursorShape = shape;
		[state applyCursorShape];
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
		state.suppressResizeEvent = YES;
		[state.window setContentSize:NSMakeSize((CGFloat)width, (CGFloat)height)];
		[state updateDimensions];
		state.suppressResizeEvent = NO;
		state.depthTexture = nil;
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

int v_multiwindow_appkit_take_queued_event(void *state_ptr, VMultiwindowAppKitQueuedEvent *out_event) {
	if (state_ptr == NULL || out_event == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	VMultiwindowAppKitWindowState *state = (__bridge VMultiwindowAppKitWindowState *)state_ptr;
	if (state.queuedEvents.count == 0) {
		return 0;
	}
	NSValue *value = state.queuedEvents[0];
	VMultiwindowAppKitQueuedEvent *event = (VMultiwindowAppKitQueuedEvent *)value.pointerValue;
	[state.queuedEvents removeObjectAtIndex:0];
	if (event == NULL) {
		return 0;
	}
	*out_event = *event;
	free(event);
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
