#import <Metal/Metal.h>

// The test owns this device for the process lifetime.
static void* v_sokol_transfer_test_device(void) {
	return (__bridge_retained void*)MTLCreateSystemDefaultDevice();
}
