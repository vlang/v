// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import maps
import os
import encoding.binary

const pe_dword_size = 4
const pe_export_data_dir_index = 0
const pe_export_directory_size = 0x28

struct SystemDll {
	name      string
	full_path string
}

fn C.SearchPathA(lp_path &char, lp_file_name &char, lp_extension &char, n_buffer_length u32, lp_buffer &char,
	lp_file_part &&char) u32
fn C.GetLastError() u32

fn (mut g Gen) lookup_system_dll(dll string) !SystemDll {
	for path in g.linker_include_paths {
		full_path := os.join_path(path, dll)
		if os.exists(full_path) {
			return SystemDll{
				name:      dll
				full_path: full_path
			}
		}
	}

	$if windows {
		unsafe {
			buffer := malloc(1024)
			len := C.SearchPathA(nil, dll.str, c'.dll', 1024, buffer, nil)
			if len == 0 {
				err_code := C.GetLastError()
				err_msg := cstring_to_vstring(C.strerror(err_code))
				return error('could not find dll: ${err_msg}')
			}

			full_path := cstring_to_vstring(buffer)
			free(buffer)
			return SystemDll{
				name:      dll
				full_path: full_path
			}
		}
	} $else {
		// TODO: look into library's dirs
		return SystemDll{
			name: dll
		}
	}
}

struct DllIndex {
	dllname string
mut:
	exports map[string]bool
}

fn (di DllIndex) to_import() PeDllImport {
	return PeDllImport{
		name:      di.dllname
		functions: maps.filter(di.exports, fn (_ string, val bool) bool {
			return val
		}).keys()
	}
}

fn index_dll(dll SystemDll) !DllIndex {
	mut file := os.open(dll.full_path)!
	exports := get_dllexports(mut file)!
	index := DllIndex{
		dllname: dll.name
		exports: exports
	}
	file.close()
	return index
}

fn get_dllexports(mut file os.File) !map[string]bool {
	dos_header := read_dos_header(mut file)!
	if dos_header.magic != u16(PeMagic.mz) {
		return error('wrong magic bytes: `${dos_header.magic.hex()}`, want: `${u16(PeMagic.mz).hex()}`')
	}

	pe_header := read_pe_header(mut file, dos_header.lfanew)!
	if pe_header.magic != u16(PeMagic.pe) {
		return error('wrong magic bytes: `${pe_header.magic.hex()}`, want: `${u16(PeMagic.pe).hex()}`')
	}
	opt_hdroffset := dos_header.lfanew + pe_coff_hdr_size

	mut sec_hdroffset := u32(0)
	export_data_dir := match pe_header.machine {
		u16(PeMachine.amd64), u16(PeMachine.arm64) {
			optional_header := read_pe32plus_optional_header(mut file, opt_hdroffset)!
			if optional_header.magic != u16(PeMagic.pe32plus) {
				return error('wrong magic bytes: `${optional_header.magic.hex()}`, want: `${u16(PeMagic.pe32plus).hex()}`')
			}
			if optional_header.number_of_rva_and_sizes <= pe_export_data_dir_index {
				return map[string]bool{} // no exports in this file
			}

			sec_hdroffset = opt_hdroffset + u32(pe_opt_hdr_size)
			read_pe_data_dir(mut file, opt_hdroffset + pe32_plus_opt_hdr_size, pe_export_data_dir_index)!
		}
		u16(PeMachine.i386) {
			return error('32-bit (i386) dlls not supported yet')
		}
		else {
			return error('unknown machine `${pe_header.machine.hex()}`')
		}
	}

	for i in 0 .. pe_header.number_of_sections {
		section_header := read_pe_section_header(mut file, sec_hdroffset +
			i * pe_section_header_size)!
		if export_data_dir.rva >= section_header.virtual_address
			&& export_data_dir.rva < section_header.virtual_address + section_header.size_of_raw_data {
			// found the right section
			return parse_export_section(mut file, export_data_dir, section_header)
		}
	}

	return map[string]bool{}
}

fn parse_export_section(mut file os.File, export_data_dir PeDataDir, section_header PeSectionHeaderRead) !map[string]bool {
	ref := section_header.virtual_address - section_header.pointer_to_raw_data
	export_directory := read_pe_export_directory(mut file, u64(export_data_dir.rva) - ref)!

	mut exports := map[string]bool{}
	exports.reserve(u32(export_data_dir.size))

	mut name_ptr := export_directory.name_ptr_rva - ref
	mut buf := []u8{}
	for _ in 0 .. export_directory.number_of_name_ptrs {
		ptr := binary.little_endian_u32(file.read_bytes_at(pe_dword_size, name_ptr))
		name_ptr += pe_dword_size

		mut j := u32(0)
		buf.clear()
		for {
			buf << file.read_bytes_at(1, ptr - ref + j)[0]
			if buf[j] == 0 {
				buf.delete_last()
				exports[buf.bytestr()] = false
				break
			}
			j++
		}
	}
	return exports
}

struct DosHeaderRead {
	magic  u16
	lfanew u32
	// address of the new exe header
}

fn read_dos_header(mut file os.File) !DosHeaderRead {
	buf := file.read_bytes(dos_header_size)
	if buf.len != dos_header_size {
		return error('error reading dos header (${dos_header_size} bytes)')
	}

	return DosHeaderRead{
		magic:  binary.little_endian_u16(buf)
		lfanew: binary.little_endian_u32(buf[dos_header_lfanew_offset..])
	}
}

struct PeHeaderRead {
	magic              u16
	machine            u16
	number_of_sections u16
}

fn read_pe_header(mut file os.File, offset u64) !PeHeaderRead {
	buf := file.read_bytes_at(pe_header_size, offset)
	if buf.len != pe_header_size {
		return error('error reading pe header (${pe_header_size} bytes)')
	}

	return PeHeaderRead{
		magic:              binary.little_endian_u16(buf)
		machine:            binary.little_endian_u16(buf[pe_header_machine_offset..])
		number_of_sections: binary.little_endian_u16(buf[pe_number_of_sections_offset..])
	}
}

struct Pe32PlusOptionalHeaderRead {
	magic                   u16
	number_of_rva_and_sizes u32
}

fn read_pe32plus_optional_header(mut file os.File, offset u64) !Pe32PlusOptionalHeaderRead {
	buf := file.read_bytes_at(pe_opt_hdr_size, offset)
	if buf.len != pe_opt_hdr_size {
		return error('error reading pe32+ optional header (${pe_opt_hdr_size} bytes)')
	}

	return Pe32PlusOptionalHeaderRead{
		magic:                   binary.little_endian_u16(buf)
		number_of_rva_and_sizes: binary.little_endian_u32(buf[pe32_plus_optional_header_offsetof(.number_of_rva_and_sizes)..])
	}
}

fn read_pe_data_dir(mut file os.File, offset u64, index u64) !PeDataDir {
	mut data_dir := PeDataDir{}
	file.read_struct_at(mut data_dir, offset + index * sizeof(PeDataDir))!
	return data_dir
}

struct PeSectionHeaderRead {
	virtual_address     u32
	size_of_raw_data    u32
	pointer_to_raw_data u32
}

fn read_pe_section_header(mut file os.File, offset u64) !PeSectionHeaderRead {
	buf := file.read_bytes_at(pe_section_header_size, offset)
	if buf.len != pe_section_header_size {
		return error('error reading section header (${pe_section_header_size} bytes)')
	}

	return PeSectionHeaderRead{
		virtual_address:     binary.little_endian_u32(buf[pe_section_header_offsetof(.virtual_address)..])
		size_of_raw_data:    binary.little_endian_u32(buf[pe_section_header_offsetof(.size_of_raw_data)..20])
		pointer_to_raw_data: binary.little_endian_u32(buf[pe_section_header_offsetof(.pointer_to_raw_data)..24])
	}
}

struct PeExportDirectoryRead {
	number_of_name_ptrs u32
	name_ptr_rva        u32
}

fn read_pe_export_directory(mut file os.File, offset u64) !PeExportDirectoryRead {
	buf := file.read_bytes_at(pe_export_directory_size, offset)
	if buf.len != pe_export_directory_size {
		return error('error reading export directory (${pe_export_directory_size} bytes)')
	}

	return PeExportDirectoryRead{
		number_of_name_ptrs: binary.little_endian_u32(buf[24..28])
		name_ptr_rva:        binary.little_endian_u32(buf[32..36])
	}
}
