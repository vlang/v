#ifndef V_MULTIWINDOW_APPKIT_READBACK_METAL_PROBE_H
#define V_MULTIWINDOW_APPKIT_READBACK_METAL_PROBE_H

#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef int (*VMultiwindowAppKitStageWindowReadbackFn)(void *, uint64_t, int, int,
	int, int, uint64_t);
typedef int (*VMultiwindowAppKitStageImageReadbackFn)(void *, void *, uint64_t, int,
	int, int, int, uint64_t);
typedef int (*VMultiwindowAppKitResolveReadbacksFn)(void *, uint64_t, int);
typedef int (*VMultiwindowAppKitTakeReadbackFn)(void *, uint64_t *, int *, int *,
	int *, int *, uint64_t *, size_t *);
typedef int (*VMultiwindowAppKitCopyReadbackFn)(void *, uint64_t, uint8_t *, size_t);
typedef int (*VMultiwindowAppKitReleaseReadbackFn)(void *, uint64_t);
typedef int (*VMultiwindowAppKitCancelReadbackFn)(void *, uint64_t);
typedef int (*VMultiwindowAppKitCancelAllReadbacksFn)(void *);
typedef int (*VMultiwindowAppKitArmOffscreenReadbackFn)(void *, void *, uint64_t,
	uint64_t);

static void *v_multiwindow_appkit_readback_probe_symbol(const char *name) {
	return dlsym(RTLD_DEFAULT, name);
}

static uint32_t v_multiwindow_appkit_readback_probe_symbol_mask(void) {
	const char *names[] = {
		"v_multiwindow_appkit_service_stage_window_readback",
		"v_multiwindow_appkit_service_stage_image_readback",
		"v_multiwindow_appkit_service_resolve_readbacks_after_submit",
		"v_multiwindow_appkit_service_take_readback_result",
		"v_multiwindow_appkit_service_copy_readback_pixels",
		"v_multiwindow_appkit_service_release_readback_result",
		"v_multiwindow_appkit_service_cancel_readback",
		"v_multiwindow_appkit_service_cancel_all_readbacks",
	};
	uint32_t mask = 0;
	for (uint32_t index = 0; index < 8; index++) {
		if (v_multiwindow_appkit_readback_probe_symbol(names[index]) != NULL) {
			mask |= UINT32_C(1) << index;
		}
	}
	return mask;
}

static void *v_multiwindow_appkit_readback_probe_state(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread]) {
		return NULL;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	return (__bridge void *)window.delegate;
}

// Returns -1 when the window has no Metal layer, 0 when capture is enabled,
// and 1 when the drawable is framebuffer-only.
static int v_multiwindow_appkit_readback_probe_framebuffer_only(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread]) {
		return -2;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	CAMetalLayer *layer = (CAMetalLayer *)window.contentView.layer;
	if (![layer isKindOfClass:[CAMetalLayer class]]) {
		return -1;
	}
	return layer.framebufferOnly ? 1 : 0;
}

static void *v_multiwindow_appkit_readback_probe_make_pattern_texture_size(
		void *window_ptr, NSUInteger width, NSUInteger height) {
	if (window_ptr == NULL || ![NSThread isMainThread] || width == 0 || height == 0
			|| width > SIZE_MAX / 4 || height > SIZE_MAX / (width * 4)) {
		return NULL;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	CAMetalLayer *layer = (CAMetalLayer *)window.contentView.layer;
	id<MTLDevice> device = [layer isKindOfClass:[CAMetalLayer class]] ? layer.device : nil;
	if (device == nil) {
		return NULL;
	}
	MTLTextureDescriptor *descriptor = [MTLTextureDescriptor
		texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm
		width:width height:height mipmapped:NO];
	descriptor.storageMode = MTLStorageModeShared;
	descriptor.usage = MTLTextureUsageShaderRead | MTLTextureUsageRenderTarget;
	id<MTLTexture> texture = [device newTextureWithDescriptor:descriptor];
	if (texture == nil) {
		return NULL;
	}
	NSUInteger bytes_per_row = width * 4;
	NSUInteger byte_length = bytes_per_row * height;
	uint8_t *pixels = (uint8_t *)malloc(byte_length);
	if (pixels == NULL) {
		return NULL;
	}
	for (NSUInteger row = 0; row < height; row++) {
		for (NSUInteger column = 0; column < width; column++) {
			NSUInteger offset = row * bytes_per_row + column * 4;
			uint8_t base = (uint8_t)(row * 64 + column * 16);
			pixels[offset + 0] = (uint8_t)(base + 3);
			pixels[offset + 1] = (uint8_t)(base + 2);
			pixels[offset + 2] = (uint8_t)(base + 1);
			pixels[offset + 3] = (uint8_t)(base + 4);
		}
	}
	[texture replaceRegion:MTLRegionMake2D(0, 0, width, height)
		mipmapLevel:0 withBytes:pixels bytesPerRow:bytes_per_row];
	free(pixels);
	return (void *)CFBridgingRetain(texture);
}

static void *v_multiwindow_appkit_readback_probe_make_pattern_texture(void *window_ptr) {
	return v_multiwindow_appkit_readback_probe_make_pattern_texture_size(window_ptr, 3, 2);
}

static void v_multiwindow_appkit_readback_probe_release_texture(void *texture_ptr) {
	if (texture_ptr != NULL) {
		CFBridgingRelease(texture_ptr);
	}
}

static int v_multiwindow_appkit_readback_probe_stage_window(void *state, uint64_t request,
		int x, int y, int width, int height, uint64_t producing_frame) {
	VMultiwindowAppKitStageWindowReadbackFn function =
		(VMultiwindowAppKitStageWindowReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_stage_window_readback");
	return function != NULL ? function(state, request, x, y, width, height,
		producing_frame) : -127;
}

static int v_multiwindow_appkit_readback_probe_stage_image(void *state, void *texture,
		uint64_t request, int x, int y, int width, int height, uint64_t producing_frame) {
	VMultiwindowAppKitStageImageReadbackFn function =
		(VMultiwindowAppKitStageImageReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_stage_image_readback");
	return function != NULL ? function(state, texture, request, x, y, width, height,
		producing_frame) : -127;
}

static int v_multiwindow_appkit_readback_probe_resolve(void *state, uint64_t submitted_frame,
		int submission_succeeded) {
	VMultiwindowAppKitResolveReadbacksFn function =
		(VMultiwindowAppKitResolveReadbacksFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_resolve_readbacks_after_submit");
	return function != NULL ? function(state, submitted_frame, submission_succeeded) : -127;
}

static int v_multiwindow_appkit_readback_probe_take(void *state, uint64_t *out_request,
		int *out_status, int *out_width, int *out_height, int *out_stride,
		uint64_t *out_submitted_frame, size_t *out_byte_length) {
	VMultiwindowAppKitTakeReadbackFn function =
		(VMultiwindowAppKitTakeReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_take_readback_result");
	return function != NULL ? function(state, out_request, out_status, out_width,
		out_height, out_stride, out_submitted_frame, out_byte_length) : -127;
}

static int v_multiwindow_appkit_readback_probe_copy(void *state, uint64_t request,
		uint8_t *out_pixels, size_t capacity) {
	VMultiwindowAppKitCopyReadbackFn function =
		(VMultiwindowAppKitCopyReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_copy_readback_pixels");
	return function != NULL ? function(state, request, out_pixels, capacity) : -127;
}

static int v_multiwindow_appkit_readback_probe_release(void *state, uint64_t request) {
	VMultiwindowAppKitReleaseReadbackFn function =
		(VMultiwindowAppKitReleaseReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_release_readback_result");
	return function != NULL ? function(state, request) : -127;
}

static int v_multiwindow_appkit_readback_probe_cancel(void *state, uint64_t request) {
	VMultiwindowAppKitCancelReadbackFn function =
		(VMultiwindowAppKitCancelReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_cancel_readback");
	return function != NULL ? function(state, request) : -127;
}

static int v_multiwindow_appkit_readback_probe_cancel_all(void *state) {
	VMultiwindowAppKitCancelAllReadbacksFn function =
		(VMultiwindowAppKitCancelAllReadbacksFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_cancel_all_readbacks");
	return function != NULL ? function(state) : -127;
}

static int v_multiwindow_appkit_readback_probe_arm_offscreen(void *state, void *texture,
		uint64_t pass_serial, uint64_t producing_frame) {
	VMultiwindowAppKitArmOffscreenReadbackFn function =
		(VMultiwindowAppKitArmOffscreenReadbackFn)v_multiwindow_appkit_readback_probe_symbol(
			"v_multiwindow_appkit_service_arm_offscreen_readback_pass");
	return function != NULL ? function(state, texture, pass_serial, producing_frame) : -127;
}

static void *v_multiwindow_appkit_readback_probe_make_command_buffer(void *window_ptr) {
	if (window_ptr == NULL || ![NSThread isMainThread]) {
		return NULL;
	}
	NSWindow *window = (__bridge NSWindow *)window_ptr;
	CAMetalLayer *layer = (CAMetalLayer *)window.contentView.layer;
	id<MTLDevice> device = [layer isKindOfClass:[CAMetalLayer class]] ? layer.device : nil;
	id<MTLCommandQueue> queue = device != nil ? [device newCommandQueue] : nil;
	id<MTLCommandBuffer> command_buffer = queue != nil ? [queue commandBuffer] : nil;
	return command_buffer != nil ? (void *)CFBridgingRetain(command_buffer) : NULL;
}

static int v_multiwindow_appkit_readback_probe_commit_command_buffer(
		void *command_buffer_ptr, int wait) {
	if (command_buffer_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	id<MTLCommandBuffer> command_buffer =
		(__bridge id<MTLCommandBuffer>)command_buffer_ptr;
	[command_buffer commit];
	if (wait != 0) {
		[command_buffer waitUntilCompleted];
		return command_buffer.status == MTLCommandBufferStatusCompleted ? 1 : 0;
	}
	return 1;
}

static int v_multiwindow_appkit_readback_probe_wait_command_buffer(
		void *command_buffer_ptr) {
	if (command_buffer_ptr == NULL || ![NSThread isMainThread]) {
		return 0;
	}
	id<MTLCommandBuffer> command_buffer =
		(__bridge id<MTLCommandBuffer>)command_buffer_ptr;
	[command_buffer waitUntilCompleted];
	return command_buffer.status == MTLCommandBufferStatusCompleted ? 1 : 0;
}

static void v_multiwindow_appkit_readback_probe_release_object(void *object_ptr) {
	if (object_ptr != NULL) {
		CFBridgingRelease(object_ptr);
	}
}

#endif
