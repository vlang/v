module macho

import encoding.binary

// Magic numbers.
pub const magic_64 = u32(0xFEEDFACF)
pub const magic_32 = u32(0xFEEDFACE)
pub const magic_fat = u32(0xCAFEBABE)

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

pub struct Section {
pub:
	name    string
	segname string
	addr    u64
	size    u64
	offset  u32
}

pub struct File {
pub:
	header   Header
	sections []Section
}

// new_file parses a 64-bit Mach-O file from `data`.
pub fn new_file(data []u8) !File {
	if data.len < 32 {
		return error('macho: file too small')
	}
	magic := binary.big_endian_u32(data[0..4])
	if magic != magic_64 {
		return error('macho: not a 64-bit Mach-O file (magic 0x${magic.hex()})')
	}
	hdr := Header{
		magic:       magic
		cpu_type:    i32(binary.big_endian_u32(data[4..8]))
		cpu_subtype: i32(binary.big_endian_u32(data[8..12]))
		file_type:   binary.big_endian_u32(data[12..16])
		num_cmds:    binary.big_endian_u32(data[16..20])
		size_cmds:   binary.big_endian_u32(data[20..24])
		flags:       binary.big_endian_u32(data[24..28])
	}
	// load commands start at offset 32
	mut off := 32
	mut sections := []Section{}
	for _ in 0 .. int(hdr.num_cmds) {
		if off + 8 > data.len {
			break
		}
		cmd := binary.big_endian_u32(data[off..off + 4])
		cmdsize := binary.big_endian_u32(data[off + 4..off + 8])
		if cmdsize == 0 || off + int(cmdsize) > data.len {
			break
		}
		// LC_SEGMENT_64 = 0x19
		if cmd == 0x19 {
			// segment name at off+8, 16 bytes
			mut segname_bytes := []u8{}
			for k in 0 .. 16 {
				b := data[off + 8 + k]
				if b == 0 {
					break
				}
				segname_bytes << b
			}
			segname := segname_bytes.bytestr()
			numsects := binary.big_endian_u32(data[off + 64..off + 68])
			// sections start at off + 72
			mut soff := off + 72
			for _ in 0 .. int(numsects) {
				if soff + 80 > data.len {
					break
				}
				mut name_bytes := []u8{}
				for k in 0 .. 16 {
					b := data[soff + k]
					if b == 0 {
						break
					}
					name_bytes << b
				}
				name := name_bytes.bytestr()
				addr := binary.big_endian_u64(data[soff + 32..soff + 40])
				size := binary.big_endian_u64(data[soff + 40..soff + 48])
				offset := binary.big_endian_u32(data[soff + 48..soff + 52])
				sections << Section{
					name:    name
					segname: segname
					addr:    addr
					size:    size
					offset:  offset
				}
				soff += 80
			}
		}
		off += int(cmdsize)
	}
	return File{
		header:   hdr
		sections: sections
	}
}
