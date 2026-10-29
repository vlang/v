module driver

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

fn windows_icon_inline_option_value(arg string) ?string {
	for prefix in ['-icon=', '--icon=', '-seticon=', '--seticon='] {
		if arg.starts_with(prefix) {
			return arg[prefix.len..]
		}
	}
	return none
}

fn validate_windows_icon_option(icon_path string, target_os string, backend string, is_shared bool, is_o bool, c_only bool, generate_c_project string) ! {
	if icon_path == '' {
		return
	}
	if target_os != 'windows' || backend != 'c' || is_o || is_shared {
		return error('`-icon` is supported only when building Windows executables with the C backend')
	}
	if generate_c_project != '' || c_only {
		return error('`-icon` cannot be used when emitting generated C instead of a Windows executable')
	}
	if !os.is_file(icon_path) {
		return error('icon file `${icon_path}` does not exist')
	}
	if os.file_ext(icon_path).to_lower_ascii() !in ['.ico', '.png'] {
		return error('`-icon` accepts only `.ico` or `.png` files')
	}
}

fn prepare_windows_icon_resource(icon_path string, build_dir string, arch string) !string {
	ico_path := prepare_windows_icon_ico_path(icon_path, build_dir)!
	obj_path := os.join_path_single(build_dir, 'icon.o')
	images := parse_ico_file(ico_path)!
	os.write_file_array(obj_path, windows_icon_resource_object(images, arch)!)!
	return obj_path
}

fn windows_icon_resource_arch(target_arch string, c_compiler string, flags []string) string {
	// An explicit cross compiler can select its own architecture without `-arch`.
	// Match its object format, just as windres did for cross-compiled icons.
	mut triple := os.file_name(c_compiler).to_lower_ascii()
	mut machine_flag := ''
	for i, flag in flags {
		if flag.starts_with('--target=') {
			triple = flag.all_after('=')
		} else if flag in ['-target', '--target'] && i + 1 < flags.len {
			triple = flags[i + 1]
		} else if flag in ['-m32', '-m64'] {
			machine_flag = flag
		}
	}
	if machine_flag != '' {
		return if machine_flag == '-m32' { 'x86' } else { 'amd64' }
	}
	for prefix in ['x86_64-', 'i686-', 'i386-', 'aarch64-'] {
		if triple.starts_with(prefix) {
			return match prefix {
				'x86_64-' { 'amd64' }
				'aarch64-' { 'arm64' }
				else { 'x86' }
			}
		}
	}
	return target_arch
}

// windows_icon_resource_object emits a single-section COFF object, accepted by TinyCC,
// MinGW and MSVC. Let the linker add the resources: UpdateResourceW drops COFF symbols
// and sections whose long names refer to the COFF string table, including DWARF.
fn windows_icon_resource_object(images []WindowsIconImage, arch string) ![]u8 {
	machine, relocation_type := match arch {
		'amd64' { u16(0x8664), u16(3) }
		'x86' { u16(0x14c), u16(7) }
		'arm64' { u16(0xaa64), u16(2) }
		else { return error('Windows icon resources are not supported for architecture `${arch}`') }
	}
	n := images.len
	if n == 0 || n >= 0xffff {
		return error('Windows icon resources require between 1 and 65534 images')
	}
	// Three directory levels: resource type, resource id, then neutral language.
	icon_names := 32
	icon_languages := icon_names + 16 + 8 * n
	group_names := icon_languages + 24 * n
	group_language := group_names + 24
	data_entries := group_language + 24
	mut section := []u8{len: data_entries + 16 * (n + 1)}
	binary.little_endian_put_u16(mut section[14..16], 2)
	put_windows_resource_entry(mut section, 16, 3, u32(icon_names) | 0x80000000)
	put_windows_resource_entry(mut section, 24, 14, u32(group_names) | 0x80000000)
	binary.little_endian_put_u16(mut section[icon_names + 14..icon_names + 16], u16(n))
	for i, image in images {
		language := icon_languages + 24 * i
		entry := data_entries + 16 * i
		put_windows_resource_entry(mut section, icon_names + 16 + 8 * i, u32(i + 1),
			u32(language) | 0x80000000)
		binary.little_endian_put_u16(mut section[language + 14..language + 16], 1)
		put_windows_resource_entry(mut section, language + 16, 0, u32(entry))
		put_windows_resource_entry(mut section, entry, u32(section.len), u32(image.image_data.len))
		section << image.image_data
		for section.len % 4 != 0 {
			section << u8(0)
		}
	}
	binary.little_endian_put_u16(mut section[group_names + 14..group_names + 16], 1)
	put_windows_resource_entry(mut section, group_names + 16, windows_icon_group_resource_id,
		u32(group_language) | 0x80000000)
	binary.little_endian_put_u16(mut section[group_language + 14..group_language + 16], 1)
	put_windows_resource_entry(mut section, group_language + 16, 0, u32(data_entries + 16 * n))
	group := build_group_icon_resource(images)
	put_windows_resource_entry(mut section, data_entries + 16 * n, u32(section.len), u32(group.len))
	section << group
	for section.len % 4 != 0 {
		section << u8(0)
	}
	// COFF header (20 bytes), section header (40), data, relocations, one section
	// symbol (18), and the empty string table (4). Each data RVA refers to symbol 0.
	relocations := 60 + section.len
	symbols := relocations + 10 * (n + 1)
	mut obj := []u8{len: 60}
	binary.little_endian_put_u16(mut obj[0..2], machine)
	binary.little_endian_put_u16(mut obj[2..4], 1)
	binary.little_endian_put_u32(mut obj[8..12], u32(symbols))
	binary.little_endian_put_u32(mut obj[12..16], 1)
	for i, b in '.rsrc'.bytes() {
		obj[20 + i] = b
	}
	binary.little_endian_put_u32(mut obj[36..40], u32(section.len))
	binary.little_endian_put_u32(mut obj[40..44], 60)
	binary.little_endian_put_u32(mut obj[44..48], u32(relocations))
	binary.little_endian_put_u16(mut obj[52..54], u16(n + 1))
	binary.little_endian_put_u32(mut obj[56..60], 0x40300040)
	obj << section
	for i in 0 .. n + 1 {
		append_le_u32(mut obj, u32(data_entries + 16 * i))
		append_le_u32(mut obj, 0)
		append_le_u16(mut obj, relocation_type)
	}
	obj << '.rsrc'.bytes()
	obj << [u8(0), 0, 0]
	append_le_u32(mut obj, 0)
	append_le_u16(mut obj, 1)
	append_le_u16(mut obj, 0)
	obj << [u8(3), 0] // IMAGE_SYM_CLASS_STATIC, no auxiliary symbols.
	append_le_u32(mut obj, 4)
	return obj
}

fn put_windows_resource_entry(mut data []u8, offset int, name u32, value u32) {
	binary.little_endian_put_u32(mut data[offset..offset + 4], name)
	binary.little_endian_put_u32(mut data[offset + 4..offset + 8], value)
}

fn prepare_windows_icon_ico_path(icon_path string, build_dir string) !string {
	match os.file_ext(icon_path).to_lower_ascii() {
		'.ico' {
			return icon_path
		}
		'.png' {
			png_bytes := os.read_bytes(icon_path)!
			ico_bytes := png_to_ico_bytes(png_bytes)!
			ico_path := os.join_path_single(build_dir, 'icon.ico')
			os.write_file_array(ico_path, ico_bytes)!
			return ico_path
		}
		else {
			return error('`-icon` accepts only `.ico` or `.png` files')
		}
	}
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
