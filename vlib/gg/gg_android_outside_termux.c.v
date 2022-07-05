module gg

import os
import sokol.sapp

#include <android/configuration.h>

fn C.AConfiguration_new() voidptr
fn C.AConfiguration_fromAssetManager(voidptr, voidptr)
fn C.AConfiguration_getDensity(voidptr) u32
fn C.AConfiguration_delete(voidptr)

// android_dpi_scale returns the scale factor of the device.
pub fn android_dpi_scale() f32 {
	config := C.AConfiguration_new()
	activity := &os.NativeActivity(sapp.android_get_native_activity())
	C.AConfiguration_fromAssetManager(config, activity.assetManager)
	density := C.AConfiguration_getDensity(config)
	C.AConfiguration_delete(config)
	return f32(density) / 160
}
