module pe

import encoding.binary

// File represents a parsed PE (Portable Executable) file.
pub struct File {
pub:
	machine  u16
	sections []Section
}

// Section represents a PE section.
pub struct Section {
pub:
	name                string
	virtual_size        u32
	virtual_address     u32
	size_of_raw_data    u32
	pointer_to_raw_data u32
	characteristics     u32
}

// Machine types.
pub const image_file_machine_amd64 = u16(0x8664)
pub const image_file_machine_i386 = u16(0x014c)
pub const image_file_machine_arm64 = u16(0xaa64)

// new_file parses a PE file from `data`.
pub fn new_file(data []u8) !File {
	if data.len < 64 {
		return error('pe: file too small')
	}
	// DOS header: e_lfanew at offset 0x3C
	e_lfanew := binary.little_endian_u32(data[0x3C..0x40])
	if int(e_lfanew) + 24 > data.len {
		return error('pe: invalid e_lfanew')
	}
	// PE signature
	if data[e_lfanew] != 0x50 || data[e_lfanew + 1] != 0x45 || data[e_lfanew + 2] != 0 || data[e_lfanew + 3] != 0 {
		return error('pe: invalid PE signature')
	}
	off := int(e_lfanew) + 4
	// COFF header
	machine := binary.little_endian_u16(data[off..off + 2])
	num_sections := binary.little_endian_u16(data[off + 2..off + 4])
	// skip TimeDateStamp(4), PointerToSymbolTable(4), NumberOfSymbols(4)
	size_opt := binary.little_endian_u16(data[off + 16..off + 18])
	// characteristics at off+18
	// section headers start after optional header
	section_off := off + 20 + int(size_opt)
	mut sections := []Section{}
	for i in 0 .. int(num_sections) {
		so := section_off + i * 40
		if so + 40 > data.len {
			break
		}
		// name: 8 bytes, null-terminated
		mut name_bytes := []u8{}
		for k in 0 .. 8 {
			b := data[so + k]
			if b == 0 {
				break
			}
			name_bytes << b
		}
		name := name_bytes.bytestr()
		virtual_size := binary.little_endian_u32(data[so + 8..so + 12])
		virtual_address := binary.little_endian_u32(data[so + 12..so + 16])
		size_raw := binary.little_endian_u32(data[so + 16..so + 20])
		ptr_raw := binary.little_endian_u32(data[so + 20..so + 24])
		characteristics := binary.little_endian_u32(data[so + 36..so + 40])
		sections << Section{
			name:                name
			virtual_size:        virtual_size
			virtual_address:     virtual_address
			size_of_raw_data:    size_raw
			pointer_to_raw_data: ptr_raw
			characteristics:     characteristics
		}
	}
	return File{
		machine:  machine
		sections: sections
	}
}

// section_by_name returns the section with the given name, if any.
pub fn (f File) section_by_name(name string) ?Section {
	for s in f.sections {
		if s.name == name {
			return s
		}
	}
	return none
}
