module macho

import encoding.binary

// Magic numbers.
pub const magic_64 = u32(0xFEEDFACF)
pub const magic_32 = u32(0xFEEDFACE)
pub const magic_fat = u32(0xCAFEBABE)

// Header contains the fields of a 64-bit Mach-O header, in host byte order.
pub struct Header {
pub:
	magic       u32
	cpu_type    i32
	cpu_subtype i32
	file_type   u32
	num_cmds    u32
	size_cmds   u32
	flags       u32
}

// Section describes a section from an LC_SEGMENT_64 command.
pub struct Section {
pub:
	name    string
	segname string
	addr    u64
	size    u64
	offset  u32
}

// File contains the header and section metadata of a 64-bit Mach-O file.
pub struct File {
pub:
	header   Header
	sections []Section
}

// new_file parses a 64-bit Mach-O file in either byte order from `data`.
// Invalid or truncated load commands return an error. Section contents are not
// decoded or validated. Fat binaries and 32-bit Mach-O files are not supported.
pub fn new_file(data []u8) !File {
	if data.len < 32 {
		return error('macho: file too small')
	}
	magic := binary.big_endian_u32(data[0..4])
	little := binary.little_endian_u32(data[0..4]) == magic_64
	if magic != magic_64 && !little {
		return error('macho: not a 64-bit Mach-O file (magic 0x${magic.hex()})')
	}
	hdr := Header{
		magic:       magic_64
		cpu_type:    i32(read_u32(data, 4, little))
		cpu_subtype: i32(read_u32(data, 8, little))
		file_type:   read_u32(data, 12, little)
		num_cmds:    read_u32(data, 16, little)
		size_cmds:   read_u32(data, 20, little)
		flags:       read_u32(data, 24, little)
	}
	if u64(hdr.size_cmds) > u64(data.len - 32) {
		return error('macho: truncated load command area')
	}
	if hdr.num_cmds > hdr.size_cmds / 8 {
		return error('macho: invalid load command count')
	}
	// load commands start at offset 32
	mut off := 32
	commands_end := off + int(hdr.size_cmds)
	mut sections := []Section{}
	for _ in 0 .. int(hdr.num_cmds) {
		if commands_end - off < 8 {
			return error('macho: truncated load command')
		}
		cmd := read_u32(data, off, little)
		cmdsize := read_u32(data, off + 4, little)
		if cmdsize < 8 || cmdsize % 8 != 0 || u64(cmdsize) > u64(commands_end - off) {
			return error('macho: invalid load command size')
		}
		// LC_SEGMENT_64 = 0x19
		if cmd == 0x19 {
			if cmdsize < 72 {
				return error('macho: truncated segment command')
			}
			numsects := read_u32(data, off + 64, little)
			if numsects > (cmdsize - 72) / 80 {
				return error('macho: truncated section table')
			}
			// sections start at off + 72
			mut soff := off + 72
			for _ in 0 .. int(numsects) {
				sections << Section{
					name:    fixed_name(data[soff..soff + 16])
					segname: fixed_name(data[soff + 16..soff + 32])
					addr:    read_u64(data, soff + 32, little)
					size:    read_u64(data, soff + 40, little)
					offset:  read_u32(data, soff + 48, little)
				}
				soff += 80
			}
		}
		off += int(cmdsize)
	}
	if off != commands_end {
		return error('macho: inconsistent load command area size')
	}
	return File{
		header:   hdr
		sections: sections
	}
}

fn read_u32(data []u8, offset int, little bool) u32 {
	return if little {
		binary.little_endian_u32_at(data, offset)
	} else {
		binary.big_endian_u32_at(data, offset)
	}
}

fn read_u64(data []u8, offset int, little bool) u64 {
	return if little {
		binary.little_endian_u64_at(data, offset)
	} else {
		binary.big_endian_u64_at(data, offset)
	}
}

fn fixed_name(data []u8) string {
	mut end := 0
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[..end].bytestr()
}
