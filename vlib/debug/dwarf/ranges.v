module dwarf

import encoding.binary

// Range is one address range [begin, end).
pub struct Range {
pub:
	begin u64
	end   u64
}

// parse_ranges decodes a .debug_ranges list: (begin, end) address
// pairs terminated by a (0, 0) pair. A begin marker of all-ones bits
// is a base-address selection entry and is returned as-is; callers
// that need absolute ranges add the selected base themselves.
pub fn parse_ranges(data []u8, addr_size u8) ![]Range {
	if addr_size != 4 && addr_size != 8 {
		return error('dwarf: unsupported address size ${addr_size}')
	}
	step := int(addr_size) * 2
	mut out := []Range{}
	mut pos := 0
	for {
		if pos + step > data.len {
			return error('dwarf: truncated range list')
		}
		begin := read_addr(data[pos..], addr_size)
		end := read_addr(data[pos + int(addr_size)..], addr_size)
		pos += step
		if begin == 0 && end == 0 {
			break
		}
		out << Range{ begin: begin, end: end }
	}
	return out
}

fn read_addr(b []u8, addr_size u8) u64 {
	if addr_size == 8 {
		return binary.little_endian_u64(b[..8])
	}
	return u64(binary.little_endian_u32(b[..4]))
}
