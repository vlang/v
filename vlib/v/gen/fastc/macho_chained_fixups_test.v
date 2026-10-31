module fastc

import os

const fixture_fixups_cmd = 32
const fixture_fixups_data = 64
const fixture_fixups_image = fixture_fixups_data + 32

// chained_fixups_fixture writes an LC_DYLD_CHAINED_FIXUPS command and its data, with
// one dyld_chained_starts_in_segment per entry of `segment_pages`, an empty entry
// for none. It returns the offsets of the segment entries, 0 for none.
fn chained_fixups_fixture(mut file []u8, segment_pages [][]u16) []int {
	fastc_write_u32_le(mut file, fixture_fixups_cmd, macho_lc_dyld_chained_fixups)
	fastc_write_u32_le(mut file, fixture_fixups_cmd + 4, 16)
	fastc_write_u32_le(mut file, fixture_fixups_cmd + 8, fixture_fixups_data)
	fastc_write_u32_le(mut file, fixture_fixups_data + 4, fixture_fixups_image - fixture_fixups_data)
	fastc_write_u32_le(mut file, fixture_fixups_image, u32(segment_pages.len))
	mut segments := []int{}
	mut next := fixture_fixups_image + 4 + segment_pages.len * 4
	for i, pages in segment_pages {
		if pages.len == 0 {
			segments << 0
			continue
		}
		next = (next + 7) & ~7
		segments << next
		fastc_write_u32_le(mut file, fixture_fixups_image + 4 + i * 4, u32(next - fixture_fixups_image))
		fastc_write_u32_le(mut file, next, u32(22 + pages.len * 2))
		fastc_write_u16_le(mut file, next + 4, macho_page_size)
		fastc_write_u16_le(mut file, next + 20, u16(pages.len))
		for page, start in pages {
			fastc_write_u16_le(mut file, next + 22 + page * 2, start)
		}
		next += 22 + pages.len * 2
	}
	fastc_write_u32_le(mut file, fixture_fixups_cmd + 12, u32(next - fixture_fixups_data))
	return segments
}

fn test_trim_chained_fixup_pages_drops_only_zero_fill_pages_without_fixups() {
	no_start := dyld_chained_ptr_start_none
	mut file := []u8{len: 512}
	segments := chained_fixups_fixture(mut file, [[]u16{}, [u16(0x10), no_start, no_start], [no_start],
		[no_start, u16(8)]])
	page := u64(macho_page_size)
	start, end := fastc_trim_chained_fixup_pages(mut file, file.len, fixture_fixups_cmd, [
		u64(0),
		page,
		0,
		page,
	])
	// A segment whose file contents end on its first page keeps that page only.
	assert fastc_read_u16_le(file, segments[1] + 20) == 1
	assert fastc_read_u16_le(file, segments[1] + 22) == 0x10
	// One that the file does not back at all has no fixups.
	assert fastc_read_u32_le(file, fixture_fixups_image + 4 + 2 * 4) == 0
	// A zero-fill page with a fixup stays listed.
	assert fastc_read_u16_le(file, segments[3] + 20) == 2
	assert start == fixture_fixups_image + 4 + 2 * 4
	assert end == segments[1] + 22
	// Nothing changes the second time.
	again_start, again_end := fastc_trim_chained_fixup_pages(mut file, file.len,
		fixture_fixups_cmd, [u64(0), page, 0, page])
	assert again_start == again_end
}

fn test_trim_chained_fixup_pages_ignores_fixups_past_the_end_of_the_file() {
	mut file := []u8{len: 512}
	segments := chained_fixups_fixture(mut file, [
		[u16(0x10), dyld_chained_ptr_start_none],
	])
	original := file.clone()
	start, end := fastc_trim_chained_fixup_pages(mut file, segments[0] + 8, fixture_fixups_cmd,
		[u64(macho_page_size)])
	assert start == end
	assert file == original
}

// TinyCC lists the zero-fill `__bss` pages of `__DATA` among its chained fixups.
// macOS 27 maps such pages from the file, so without the trimming in the signer
// `big_bss` starts with the bytes of `__LINKEDIT`
// (https://github.com/vlang/v/issues/29744). The many functions make the file
// long enough for that to happen.
fn test_signed_tcc_executable_starts_with_zeroed_bss() {
	if os.user_os() != 'macos' {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'fastc_macho_bss_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path_single(test_dir, 'bss.c')
	exe_path := os.join_path_single(test_dir, 'bss')
	mut source := []string{}
	source << 'int printf(const char *, ...);'
	source << 'char data_bytes[100] = {1};'
	source << 'char *data_ptr = data_bytes;'
	source << 'static char big_bss[32 * 1024];'
	for i in 0 .. 6000 {
		source << 'int padding_function_with_a_long_name_${i}(void) { return ${i}; }'
	}
	source << 'int main(void) {'
	source << '	long bad = 0;'
	source << '	for (long i = 0; i < (long)sizeof(big_bss); i++) bad += big_bss[i] != 0;'
	source << '	printf("non-zero bss bytes: %ld\\n", bad);'
	source << '	return bad != 0;'
	source << '}'
	os.write_lines(source_path, source) or { panic(err) }
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	mut args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}']
	mut sdk_root := os.getenv('SDKROOT')
	if !os.is_dir(sdk_root) {
		result := os.exec(['xcrun', '--show-sdk-path'])
		if result.exit_code == 0 {
			sdk_root = result.output.trim_space()
		}
	}
	if os.is_dir(sdk_root) {
		args << '-L${os.join_path(sdk_root, 'usr', 'lib')}'
	}
	args << ['-w', '-o', exe_path, source_path]
	// Without `codesign` on PATH TinyCC leaves the executable unsigned.
	old_path := os.getenv('PATH')
	os.setenv('PATH', '/nonexistent', true)
	os.exec([tcc, ...args])
	os.setenv('PATH', old_path, true)
	assert os.is_file(exe_path)
	fastc_sign_macho_adhoc(exe_path) or { panic(err) }
	verify := os.exec(['codesign', '--verify', '--strict', exe_path])
	assert verify.exit_code == 0, verify.output
	run := os.exec([exe_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'non-zero bss bytes: 0'
}
