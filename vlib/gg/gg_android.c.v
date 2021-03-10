module gg

import sokol.sapp

#include <android/configuration.h>
#include <android/native_activity.h>

fn C.AConfiguration_new() voidptr
fn C.AConfiguration_fromAssetManager(voidptr, voidptr)
fn C.AConfiguration_getDensity(voidptr) u32
fn C.AConfiguration_delete(voidptr)

struct C.AAssetManager {}

// See https://developer.android.com/ndk/reference/struct/a-native-activity for more info.
struct C.ANativeActivity {
pub:
	assetManager     voidptr // Pointer to the Asset Manager instance for the application. (AAssetManager *)
	callbacks        voidptr // Pointer to the callback function table of the native application. (struct ANativeActivityCallbacks *)
	clazz            voidptr // The NativeActivity object handle.
	env              voidptr // JNI context for the main thread of the app.
	externalDataPath charptr // Path to this application's external (removable/mountable) data directory.
	instance         voidptr // This is the native instance of the application.
	internalDataPath charptr // Path to this application's internal data directory.
	obbPath          charptr // Available starting with Honeycomb: path to the directory containing the application's OBB files (if any).
	sdkVersion       int     // The platform's SDK version code.
	vm               voidptr // The global handle on the process's Java VM
}

pub fn android_dpi_scale() f32 {
	config := C.AConfiguration_new()
	activity := &C.ANativeActivity(sapp.android_get_native_activity())
	C.AConfiguration_fromAssetManager(config, activity.assetManager)
	density := C.AConfiguration_getDensity(config)
	C.AConfiguration_delete(config)
	return f32(density) / 160
}
