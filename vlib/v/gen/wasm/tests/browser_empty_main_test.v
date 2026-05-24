import os

struct WasmVarUint {
	value    u32
	next_idx int
}

struct WasmModuleSummary {
mut:
	exports           []string
	has_start_section bool
}

fn test_wasm_browser_target_allows_empty_main() {
	vexe := os.quoted_path(@VEXE)
	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_browser_tests')
	os.mkdir_all(wrkdir)!
	defer {
		os.rmdir_all(wrkdir) or {}
	}

	source_path := os.join_path(wrkdir, 'empty_main.wasm.v')
	os.write_file(source_path, 'pub fn main() {}\n')!

	flags_sets := [
		'-no-bounds-checking -b wasm -os browser',
		'-no-bounds-checking -enable-globals -b wasm -os browser',
	]

	for idx, flags in flags_sets {
		output_path := os.join_path(wrkdir, 'empty_main_${idx}.wasm')
		res :=
			os.execute('${vexe} ${flags} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
		assert res.exit_code == 0, 'compilation failed for `${flags}`: ${res.output}'
		assert os.exists(output_path), 'missing output for `${flags}`'
	}
}

fn test_wasm_shared_library_exports_custom_names_without_main() {
	vexe := os.quoted_path(@VEXE)
	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_shared_library_tests')
	os.mkdir_all(wrkdir)!
	defer {
		os.rmdir_all(wrkdir) or {}
	}

	source_path := os.join_path(wrkdir, 'my_wasm_lib.v')
	output_path := os.join_path(wrkdir, 'my_wasm_lib.wasm')
	source := [
		'module my_wasm_lib',
		'',
		"@[export: 'myFunction']",
		'fn my_function(a int, b int) int {',
		'\treturn a + b',
		'}',
	].join_lines()
	os.write_file(source_path, source)!

	res :=
		os.execute('${vexe} -b wasm -shared -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, 'compilation failed: ${res.output}'
	assert os.exists(output_path), 'missing output for shared wasm library'

	wasm_bytes := os.read_bytes(output_path)!
	summary := inspect_wasm_module(wasm_bytes) or { panic(err) }

	assert 'myFunction' in summary.exports
	assert '_start' !in summary.exports
	assert summary.has_start_section
}

fn inspect_wasm_module(wasm_bytes []u8) !WasmModuleSummary {
	if wasm_bytes.len < 8 {
		return error('wasm module is too short')
	}
	if wasm_bytes[0] != 0x00 || wasm_bytes[1] != 0x61 || wasm_bytes[2] != 0x73
		|| wasm_bytes[3] != 0x6d || wasm_bytes[4] != 0x01 || wasm_bytes[5] != 0x00
		|| wasm_bytes[6] != 0x00 || wasm_bytes[7] != 0x00 {
		return error('invalid wasm header')
	}

	mut summary := WasmModuleSummary{}
	mut idx := 8
	for idx < wasm_bytes.len {
		section_id := wasm_bytes[idx]
		idx++
		section_len := read_wasm_u32(wasm_bytes, idx)!
		idx = section_len.next_idx
		section_end := idx + int(section_len.value)
		if section_end > wasm_bytes.len {
			return error('wasm section exceeds module bounds')
		}
		match section_id {
			u8(7) {
				mut export_idx := idx
				exports_len := read_wasm_u32(wasm_bytes, export_idx)!
				export_idx = exports_len.next_idx
				for _ in 0 .. int(exports_len.value) {
					name_len := read_wasm_u32(wasm_bytes, export_idx)!
					export_idx = name_len.next_idx
					name_end := export_idx + int(name_len.value)
					if name_end > section_end {
						return error('wasm export name exceeds section bounds')
					}
					summary.exports << wasm_bytes[export_idx..name_end].bytestr()
					export_idx = name_end
					if export_idx >= section_end {
						return error('wasm export kind is missing')
					}
					export_idx++
					export_ref := read_wasm_u32(wasm_bytes, export_idx)!
					export_idx = export_ref.next_idx
				}
			}
			u8(8) {
				summary.has_start_section = true
			}
			else {}
		}

		idx = section_end
	}
	return summary
}

fn read_wasm_u32(wasm_bytes []u8, start int) !WasmVarUint {
	mut value := u32(0)
	mut shift := u32(0)
	mut idx := start
	for idx < wasm_bytes.len {
		b := wasm_bytes[idx]
		value |= u32(b & 0x7f) << shift
		idx++
		if b & 0x80 == 0 {
			return WasmVarUint{
				value:    value
				next_idx: idx
			}
		}
		shift += 7
		if shift >= 35 {
			break
		}
	}
	return error('invalid wasm varuint32 encoding')
}
