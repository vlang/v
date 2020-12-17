module os

struct C.AAset {
}

struct C.AAssetManager {
}

struct C.ANativeActivity {
	assetManager voidptr
}

fn C.AAssetManager_open(&C.AAsetManager, charptr, int) &C.AAset

fn C.AAsset_getLength(&C.AAset) int

fn C.AAsset_read(&C.AAset, voidptr, int) int

fn C.AAsset_close(&C.AAsset)

pub fn read_apk_asset(file string) ?[]byte {
	act := &C.ANativeActivity(C.sapp_android_get_native_activity())
	if isnil(act) {
		return error('Could not get reference to Android activity')
	}
	asset := C.AAssetManager_open(&C.AAssetManager(act.assetManager), file.str, C.AASSET_MODE_STREAMING)
	if isnil(asset) {
		return error('File `$file` not found')
	}
	len := C.AAsset_getLength(asset)
	buf := []byte{len: len}
	for {
		if C.AAsset_read(asset, buf.data, len) > 0 {
			break
		}
	}
	C.AAsset_close(asset)
	return buf
}
