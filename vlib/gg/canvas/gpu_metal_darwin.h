#import <Metal/Metal.h>

// Returns a retained system default device for a headless sokol.gfx setup.
static void* v_canvas_metal_device(void) {
	return (__bridge_retained void*)MTLCreateSystemDefaultDevice();
}
