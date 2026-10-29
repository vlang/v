module driver

import encoding.binary
import os
import v.cmdexec

struct WindowsDebugImage {
	sections     map[string][]u8
	symbols      []u8
	resource_rva u32
}

fn read_windows_debug_image(path string) !WindowsDebugImage {
	data := os.read_bytes(path)!
	pe := int(binary.little_endian_u32(data[0x3c..0x40]))
	assert data[pe..pe + 4] == [u8(`P`), `E`, 0, 0]
	n := int(binary.little_endian_u16(data[pe + 6..pe + 8]))
	symbols := int(binary.little_endian_u32(data[pe + 12..pe + 16]))
	count := int(binary.little_endian_u32(data[pe + 16..pe + 20]))
	assert symbols > 0 && count > 0
	strings := symbols + 18 * count
	strings_size := int(binary.little_endian_u32(data[strings..strings + 4]))
	headers := pe + 24 + int(binary.little_endian_u16(data[pe + 20..pe + 22]))
	mut sections := map[string][]u8{}
	mut resource_rva := u32(0)
	for i in 0 .. n {
		header := headers + 40 * i
		mut name := data[header..header + 8].bytestr().all_before('\x00')
		if name.starts_with('/') {
			offset := strings + name[1..].int()
			name = data[offset..strings + strings_size].bytestr().all_before('\x00')
		}
		size := int(binary.little_endian_u32(data[header + 16..header + 20]))
		offset := int(binary.little_endian_u32(data[header + 20..header + 24]))
		sections[name] = data[offset..offset + size].clone()
		if name == '.rsrc' {
			resource_rva = binary.little_endian_u32(data[header + 12..header + 16])
		}
	}
	return WindowsDebugImage{
		sections:     sections
		symbols:      data[symbols..strings + strings_size].clone()
		resource_rva: resource_rva
	}
}

fn windows_debug_resource_payload(image WindowsDebugImage, typ u32, id u32) []u8 {
	data := image.sections['.rsrc']
	mut directory := 0
	for key in [typ, id, u32(0)] {
		count := int(binary.little_endian_u16(data[directory + 14..directory + 16]))
		mut found := false
		for i in 0 .. count {
			entry := directory + 16 + 8 * i
			if binary.little_endian_u32(data[entry..entry + 4]) == key {
				directory = int(binary.little_endian_u32(data[entry + 4..entry + 8]) & 0x7fffffff)
				found = true
				break
			}
		}
		assert found, 'resource ${typ}/${id}, key ${key}'
	}
	offset := int(binary.little_endian_u32(data[directory..directory + 4]) - image.resource_rva)
	size := int(binary.little_endian_u32(data[directory + 4..directory + 8]))
	return data[offset..offset + size]
}

fn test_linked_windows_icons_keep_dwarf_and_coff_symbols() ! {
	compiler := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }) or {
		eprintln('skipped: no MinGW gcc for Windows debug resource test')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v windows debug resources ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'debug.c')
	os.write_file(source, '__attribute__((noinline)) int level_three(int x) { return x + 1; }\n' +
		'__attribute__((noinline)) int level_two(int x) { return level_three(x) + 1; }\n' +
		'__attribute__((noinline)) int level_one(int x) { return level_two(x) + 1; }\n' +
		'int main(void) { return level_one(0) != 3; }\n')!
	icon := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vdoc', 'theme', 'favicons', 'favicon.ico')
	resource := prepare_windows_icon_resource(icon, root, 'amd64')!
	mut inputs := [source]
	if clang := os.find_abs_path_of_executable('clang') {
		clang_object := os.join_path(root, 'clang.o')
		built := cmdexec.run(clang, ['--target=x86_64-w64-mingw32', '-g', '-O0', '-c', '-o',
			clang_object, source])
		assert built.exit_code == 0, built.output
		inputs << clang_object
	}
	for index, input in inputs {
		plain := os.join_path(root, 'plain_${index}.exe')
		with_icon := os.join_path(root, 'icon_${index}.exe')
		for output in [plain, with_icon] {
			mut args := ['-g', '-O0', '-o', output, input]
			if output == with_icon {
				args << resource
			}
			built := cmdexec.run(compiler, args)
			assert built.exit_code == 0, built.output
		}
		before := read_windows_debug_image(plain)!
		after := read_windows_debug_image(with_icon)!
		assert '.debug_info' in before.sections
		assert '.debug_line' in before.sections
		assert '.rsrc' in after.sections
		images := parse_ico_file(icon)!
		for i, image in images {
			assert windows_debug_resource_payload(after, 3, u32(i + 1)) == image.image_data
		}
		group := windows_debug_resource_payload(after, 14, 1)
		assert binary.little_endian_u16(group[0..2]) == 0
		assert binary.little_endian_u16(group[2..4]) == 1
		assert binary.little_endian_u16(group[4..6]) == images.len
		for name, contents in before.sections {
			if name.starts_with('.debug_') {
				assert name in after.sections, name
				assert after.sections[name] == contents, name
			}
		}
		for name in ['level_one', 'level_two', 'level_three'] {
			assert before.symbols.bytestr().contains(name)
			assert after.symbols.bytestr().contains(name)
		}
		$if windows {
			// The manifest path must leave all executable bytes untouched as well.
			executable := os.read_bytes(with_icon)!
			add_windows_long_path_manifest(with_icon)!
			assert os.read_bytes(with_icon)! == executable
			assert cmdexec.run(with_icon, []).exit_code == 0
		}
	}
}
