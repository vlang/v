module driver

#include <windows.h>

fn C.BeginUpdateResourceW(pfilename &u16, delete_existing_resources int) voidptr
fn C.UpdateResourceW(update_handle voidptr, type_ voidptr, name voidptr, language u16, data voidptr, data_size u32) int
fn C.EndUpdateResourceW(update_handle voidptr, discard int) int
fn C.GetLastError() u32

fn apply_windows_icon_to_executable(exe_path string, icon_path string) ! {
	images := parse_ico_file(icon_path)!
	if images.len == 0 {
		return error('icon file `${icon_path}` does not contain any icon images')
	}
	windows_exe_path := exe_path.replace('/', '\\')
	update_handle := C.BeginUpdateResourceW(windows_exe_path.to_wide(), 0)
	if isnil(update_handle) {
		return error('failed to open `${windows_exe_path}` for icon updates (Windows error ${C.GetLastError()})')
	}
	group_resource := build_group_icon_resource(images)
	if C.UpdateResourceW(update_handle, windows_resource_id(14),
		windows_resource_id(windows_icon_group_resource_id), 0, &u8(group_resource.data),
		u32(group_resource.len)) == 0 {
		C.EndUpdateResourceW(update_handle, 1)
		return error('failed to write the icon group resource to `${windows_exe_path}` (Windows error ${C.GetLastError()})')
	}
	for i, image in images {
		if C.UpdateResourceW(update_handle, windows_resource_id(3), windows_resource_id(i + 1), 0,
			&u8(image.image_data.data), image.bytes_in_res) == 0 {
			C.EndUpdateResourceW(update_handle, 1)
			return error('failed to write icon image ${i + 1} to `${windows_exe_path}` (Windows error ${C.GetLastError()})')
		}
	}
	if C.EndUpdateResourceW(update_handle, 0) == 0 {
		return error('failed to finalize icon updates for `${windows_exe_path}` (Windows error ${C.GetLastError()})')
	}
}

fn C.LoadLibraryExW(file_name &u16, file voidptr, flags u32) voidptr
fn C.FindResourceW(module_handle voidptr, name voidptr, type_ voidptr) voidptr
fn C.FreeLibrary(module_handle voidptr) bool

const windows_rt_manifest = 24
// LOAD_LIBRARY_AS_DATAFILE | LOAD_LIBRARY_AS_IMAGE_RESOURCE: map the resources only.
const windows_load_as_resources = u32(0x22)

// windows_long_path_manifest declares the executable longPathAware, so Windows lets it
// use paths past MAX_PATH (the setting applies only to executables that declare it).
const windows_long_path_manifest = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <application xmlns="urn:schemas-microsoft-com:asm.v3">
    <windowsSettings>
      <longPathAware xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">true</longPathAware>
    </windowsSettings>
  </application>
</assembly>
'

// windows_executable_has_manifest reports whether the executable at `exe_path` already
// carries an application manifest, with one of the ids 1 to 3 that Windows looks for.
fn windows_executable_has_manifest(exe_path string) !bool {
	windows_exe_path := exe_path.replace('/', '\\')
	module_handle := C.LoadLibraryExW(windows_exe_path.to_wide(), unsafe { nil },
		windows_load_as_resources)
	if isnil(module_handle) {
		return error('failed to read the resources of `${windows_exe_path}` (Windows error ${C.GetLastError()})')
	}
	defer {
		C.FreeLibrary(module_handle)
	}
	for id in 1 .. 4 {
		if !isnil(C.FindResourceW(module_handle, windows_resource_id(id),
			windows_resource_id(windows_rt_manifest))) {
			return true
		}
	}
	return false
}

// add_windows_long_path_manifest gives the executable at `exe_path` a manifest that
// declares it longPathAware, after it is linked, the way `-icon` adds its icon. An
// executable that already has a manifest, as MinGW GCC links one, keeps it unchanged.
// The result reports whether the manifest was added.
fn add_windows_long_path_manifest(exe_path string) !bool {
	if windows_executable_has_manifest(exe_path)! {
		return false
	}
	windows_exe_path := exe_path.replace('/', '\\')
	update_handle := C.BeginUpdateResourceW(windows_exe_path.to_wide(), 0)
	if isnil(update_handle) {
		return error('failed to open `${windows_exe_path}` for a manifest update (Windows error ${C.GetLastError()})')
	}
	manifest := windows_long_path_manifest
	if C.UpdateResourceW(update_handle, windows_resource_id(windows_rt_manifest),
		windows_resource_id(1), 0, manifest.str, u32(manifest.len)) == 0 {
		C.EndUpdateResourceW(update_handle, 1)
		return error('failed to write the manifest to `${windows_exe_path}` (Windows error ${C.GetLastError()})')
	}
	if C.EndUpdateResourceW(update_handle, 0) == 0 {
		return error('failed to finalize the manifest of `${windows_exe_path}` (Windows error ${C.GetLastError()})')
	}
	return true
}

fn windows_resource_id(id int) voidptr {
	return voidptr(uint(id))
}
