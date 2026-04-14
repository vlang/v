module builder

#include <windows.h>

fn C.BeginUpdateResourceW(pfilename &u16, delete_existing_resources int) voidptr
fn C.UpdateResourceW(update_handle voidptr, type_ voidptr, name voidptr, language u16, data voidptr, data_size u32) int
fn C.EndUpdateResourceW(update_handle voidptr, discard int) int
fn C.GetLastError() u32

fn (mut b Builder) apply_windows_icon_to_executable() ! {
	if b.pref.icon_path == '' {
		return
	}
	icon_path := b.prepare_windows_icon_ico_path()!
	images := parse_ico_file(icon_path)!
	if images.len == 0 {
		return error('icon file `${icon_path}` does not contain any icon images')
	}
	exe_path := b.pref.out_name.replace('/', '\\')
	update_handle := C.BeginUpdateResourceW(exe_path.to_wide(), 0)
	if isnil(update_handle) {
		return error('failed to open `${exe_path}` for icon updates (Windows error ${C.GetLastError()})')
	}
	group_resource := build_group_icon_resource(images)
	if C.UpdateResourceW(update_handle, windows_resource_id(14),
		windows_resource_id(windows_icon_group_resource_id), 0, &u8(group_resource.data),
		u32(group_resource.len)) == 0 {
		C.EndUpdateResourceW(update_handle, 1)
		return error('failed to write the icon group resource to `${exe_path}` (Windows error ${C.GetLastError()})')
	}
	for i, image in images {
		if C.UpdateResourceW(update_handle, windows_resource_id(3), windows_resource_id(i + 1), 0,
			&u8(image.image_data.data), image.bytes_in_res) == 0 {
			C.EndUpdateResourceW(update_handle, 1)
			return error('failed to write icon image ${i + 1} to `${exe_path}` (Windows error ${C.GetLastError()})')
		}
	}
	if C.EndUpdateResourceW(update_handle, 0) == 0 {
		return error('failed to finalize icon updates for `${exe_path}` (Windows error ${C.GetLastError()})')
	}
}

fn windows_resource_id(id int) voidptr {
	return voidptr(usize(id))
}
