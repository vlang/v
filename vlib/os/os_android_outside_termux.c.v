module os

#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#include <android/native_activity.h>

pub enum AssetMode {
	buffer = C.AASSET_MODE_BUFFER // Caller plans to ask for a read-only buffer with all data.
	random = C.AASSET_MODE_RANDOM // Read chunks, and seek forward and backward.
	streaming = C.AASSET_MODE_STREAMING // Read sequentially, with an occasional forward seek.
	unknown = C.AASSET_MODE_UNKNOWN // No specific information about how data will be accessed.
}

// See https://developer.android.com/ndk/reference/struct/a-native-activity for more info.
struct C.ANativeActivity {
pub:
	assetManager     &AssetManager // Pointer to the Asset Manager instance for the application.
	clazz            voidptr       // (jobject) The NativeActivity object handle.
	env              voidptr       // (JNIEnv *) JNI context for the main thread of the app.
	externalDataPath &char   // Path to this application's external (removable/mountable) data directory.
	instance         voidptr // This is the native instance of the application.
	internalDataPath &char   // Path to this application's internal data directory.
	obbPath          &char   // Available starting with Honeycomb: path to the directory containing the application's OBB files (if any).
	sdkVersion       int     // The platform's SDK version code.
	vm               voidptr // (JavaVM *) The global handle on the process's Java VM.
}

// NativeActivity defines the native side of an android.app.NativeActivity.
pub type NativeActivity = C.ANativeActivity

struct C.AAssetManager {
}

// AssetManager provides access to an application's raw assets by creating Asset objects.
pub type AssetManager = C.AAssetManager

fn C.AAssetManager_open(&C.AAssetManager, &char, int) &C.AAsset

// open opens an Android `Asset`
pub fn (am &AssetManager) open(filename string, mode AssetMode) !&Asset {
	asset := C.AAssetManager_open(am, filename.str, int(mode))
	if isnil(asset) {
		return error('file `$filename` not found')
	}
	return asset
}

struct C.AAsset {
}

pub type Asset = C.AAsset

fn C.AAsset_getBuffer(&C.AAsset) voidptr

// get_buffer returns a pointer to a buffer holding the entire contents of the asset.
pub fn (a &Asset) get_buffer() voidptr {
	return C.AAsset_getBuffer(a)
}

fn C.AAsset_getLength(&C.AAsset) int

// get_length returns the total size of the asset data.
pub fn (a &Asset) get_length() int {
	return C.AAsset_getLength(a)
}

fn C.AAsset_getLength64(&C.AAsset) i64

// get_length_64 returns the total size of the asset data using
// a 64-bit number insted of 32-bit as `get_length`.
pub fn (a &Asset) get_length_64() i64 {
	return C.AAsset_getLength64(a)
}

fn C.AAsset_read(&C.AAsset, voidptr, usize) int

// read attempts to read 'count' bytes of data from the current offset.
// read returns the number of bytes read, zero on EOF, or < 0 on error.
pub fn (a &Asset) read(buffer voidptr, count usize) int {
	return C.AAsset_read(a, buffer, count)
}

fn C.AAsset_close(&C.AAsset)

// close closes the asset, freeing all associated resources.
pub fn (a &Asset) close() {
	C.AAsset_close(a)
}

// read_apk_asset returns all the data located at `path`.
// `path` is expected to be relative to the APK/AAB `assets` directory.
pub fn read_apk_asset(path string) ![]u8 {
	$if apk {
		act := &NativeActivity(C.sapp_android_get_native_activity())
		if isnil(act) {
			return error('could not get reference to Android activity')
		}
		asset_manager := act.assetManager
		asset := asset_manager.open(path, .streaming)!
		len := asset.get_length()
		buf := []u8{len: len}
		for {
			if asset.read(buf.data, usize(len)) > 0 {
				break
			}
		}
		asset.close()
		return buf
	} $else {
		return error(@FN + ' can only be used with APK/AAB packaged Android apps')
	}
}
