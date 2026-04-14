module builder

import encoding.binary
import os

const windows_icon_group_resource_id = 1
const max_windows_icon_dimension = 256

struct WindowsIconImage {
	width        u8
	height       u8
	color_count  u8
	planes       u16
	bit_count    u16
	bytes_in_res u32
	image_data   []u8
}

struct WindowsIconSize {
	width  int
	height int
}

fn (b &Builder) ensure_windows_icon_flag_is_valid() {
	if b.pref.icon_path == '' {
		return
	}
	if b.pref.os != .windows || b.pref.build_mode == .build_module || b.pref.is_o
		|| b.pref.is_shared {
		verror('`-icon` is supported only when building Windows executables')
	}
	if b.pref.generate_c_project != '' || b.pref.out_name.ends_with('.c')
		|| b.pref.out_name.ends_with('.js') || b.pref.should_output_to_stdout() {
		verror('`-icon` cannot be used when emitting generated C/JS output instead of a Windows executable')
	}
}

fn (mut b Builder) prepare_cross_windows_icon_resource() !string {
	if b.pref.icon_path == '' {
		return ''
	}
	ico_path := b.prepare_windows_icon_ico_path()!
	rc_path := b.get_vtmp_filename(b.pref.out_name, '.icon.rc')
	obj_path := b.get_vtmp_filename(b.pref.out_name, '.icon.o')
	os.write_file(rc_path, '1 ICON ${rc_quoted_string(ico_path)}\n')!
	b.pref.cleanup_files << rc_path
	b.pref.cleanup_files << obj_path
	windres := b.find_windres()!
	cmd := '${os.quoted_path(windres)} -i ${os.quoted_path(rc_path)} -o ${os.quoted_path(obj_path)} -O coff'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error('failed to compile Windows icon resource with `${windres}`: ${res.output.trim_space()}')
	}
	return obj_path
}

fn (mut b Builder) prepare_windows_icon_ico_path() !string {
	if b.pref.icon_path == '' {
		return ''
	}
	icon_path := os.real_path(b.pref.icon_path)
	if !os.is_file(icon_path) {
		return error('icon file `${icon_path}` does not exist')
	}
	match os.file_ext(icon_path).to_lower_ascii() {
		'.ico' {
			return icon_path
		}
		'.png' {
			png_bytes := os.read_bytes(icon_path)!
			ico_bytes := png_to_ico_bytes(png_bytes)!
			ico_path := b.get_vtmp_filename(b.pref.out_name, '.icon.ico')
			os.write_file_array(ico_path, ico_bytes)!
			b.pref.cleanup_files << ico_path
			return ico_path
		}
		else {
			return error('`-icon` accepts only `.ico` or `.png` files')
		}
	}
}

fn (b &Builder) find_windres() !string {
	compiler_dir := if b.pref.ccompiler.contains('/') || b.pref.ccompiler.contains('\\') {
		os.dir(b.pref.ccompiler)
	} else {
		''
	}
	compiler_name := executable_stem(os.file_name(b.pref.ccompiler))
	mut candidates := []string{}
	for suffix in ['-gcc', '-clang', '-cc', '-g++', '-clang++'] {
		if compiler_name.ends_with(suffix) {
			candidates << '${compiler_name[..compiler_name.len - suffix.len]}-windres'
		}
	}
	candidates << 'windres'
	candidates << 'llvm-windres'
	for candidate in candidates {
		for name in [candidate, '${candidate}.exe'] {
			if compiler_dir != '' {
				full_path := os.join_path(compiler_dir, name)
				if os.is_file(full_path) {
					return full_path
				}
			}
			if resolved := os.find_abs_path_of_executable(name) {
				return resolved
			}
		}
	}
	return error('could not find `windres`, which is needed for `-icon` while cross-compiling to Windows')
}

fn executable_stem(name string) string {
	lower_name := name.to_lower_ascii()
	if lower_name.ends_with('.exe') {
		return name[..name.len - 4]
	}
	return name
}

fn rc_quoted_string(path string) string {
	return '"' + path.replace('\\', '\\\\').replace('"', '\\"') + '"'
}

fn parse_ico_file(path string) ![]WindowsIconImage {
	return parse_ico_bytes(os.read_bytes(path)!)
}

fn parse_ico_bytes(data []u8) ![]WindowsIconImage {
	if data.len < 6 {
		return error('invalid icon file: missing ICO header')
	}
	if binary.little_endian_u16(data[0..2]) != 0 || binary.little_endian_u16(data[2..4]) != 1 {
		return error('invalid icon file: expected an ICO header')
	}
	image_count := int(binary.little_endian_u16(data[4..6]))
	if image_count <= 0 {
		return error('invalid icon file: no icon images were found')
	}
	if data.len < 6 + (image_count * 16) {
		return error('invalid icon file: truncated icon directory')
	}
	mut images := []WindowsIconImage{cap: image_count}
	for i := 0; i < image_count; i++ {
		entry_offset := 6 + (i * 16)
		image_size := int(binary.little_endian_u32(data[entry_offset + 8..entry_offset + 12]))
		image_offset := int(binary.little_endian_u32(data[entry_offset + 12..entry_offset + 16]))
		image_end := image_offset + image_size
		if image_offset < 0 || image_size <= 0 || image_offset > data.len || image_end > data.len {
			return error('invalid icon file: icon image ${i + 1} points outside the file')
		}
		images << WindowsIconImage{
			width:        data[entry_offset]
			height:       data[entry_offset + 1]
			color_count:  data[entry_offset + 2]
			planes:       binary.little_endian_u16(data[entry_offset + 4..entry_offset + 6])
			bit_count:    binary.little_endian_u16(data[entry_offset + 6..entry_offset + 8])
			bytes_in_res: u32(image_size)
			image_data:   data[image_offset..image_end].clone()
		}
	}
	return images
}

fn png_to_ico_bytes(png []u8) ![]u8 {
	size := png_dimensions(png)!
	mut ico := []u8{cap: 22 + png.len}
	append_le_u16(mut ico, 0)
	append_le_u16(mut ico, 1)
	append_le_u16(mut ico, 1)
	ico << u8(if size.width == max_windows_icon_dimension { 0 } else { size.width })
	ico << u8(if size.height == max_windows_icon_dimension { 0 } else { size.height })
	ico << u8(0)
	ico << u8(0)
	append_le_u16(mut ico, 1)
	append_le_u16(mut ico, 32)
	append_le_u32(mut ico, u32(png.len))
	append_le_u32(mut ico, u32(22))
	ico << png
	return ico
}

fn png_dimensions(png []u8) !WindowsIconSize {
	if png.len < 24 {
		return error('invalid PNG icon file: missing PNG header')
	}
	if png[0] != 0x89 || png[1] != `P` || png[2] != `N` || png[3] != `G` || png[4] != 0x0d
		|| png[5] != 0x0a || png[6] != 0x1a || png[7] != 0x0a {
		return error('invalid PNG icon file: bad PNG signature')
	}
	if png[12] != `I` || png[13] != `H` || png[14] != `D` || png[15] != `R` {
		return error('invalid PNG icon file: missing IHDR chunk')
	}
	width := int(binary.big_endian_u32(png[16..20]))
	height := int(binary.big_endian_u32(png[20..24]))
	if width <= 0 || height <= 0 || width > max_windows_icon_dimension
		|| height > max_windows_icon_dimension {
		return error('PNG icons must be between 1x1 and 256x256 pixels')
	}
	return WindowsIconSize{
		width:  width
		height: height
	}
}

fn build_group_icon_resource(images []WindowsIconImage) []u8 {
	mut data := []u8{cap: 6 + (images.len * 14)}
	append_le_u16(mut data, 0)
	append_le_u16(mut data, 1)
	append_le_u16(mut data, u16(images.len))
	for i, image in images {
		data << image.width
		data << image.height
		data << image.color_count
		data << u8(0)
		append_le_u16(mut data, image.planes)
		append_le_u16(mut data, image.bit_count)
		append_le_u32(mut data, image.bytes_in_res)
		append_le_u16(mut data, u16(i + 1))
	}
	return data
}

fn append_le_u16(mut data []u8, value u16) {
	mut buf := []u8{len: 2}
	binary.little_endian_put_u16(mut buf, value)
	data << buf
}

fn append_le_u32(mut data []u8, value u32) {
	mut buf := []u8{len: 4}
	binary.little_endian_put_u32(mut buf, value)
	data << buf
}
