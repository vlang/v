module tzdata

import time

const embedded_zoneinfo = $embed_file('zoneinfo.zip')

fn init() {
	time.register_zoneinfo_loader(load_from_embedded_zoneinfo)
}

fn load_from_embedded_zoneinfo(name string) ![]u8 {
	return read_uncompressed_zip_entry(embedded_zoneinfo.to_bytes(), name)
}

fn read_uncompressed_zip_entry(data []u8, name string) ![]u8 {
	eocd := find_zip_end_of_central_directory(data)!
	entries := read_zip_u16(data, eocd + 10)
	mut pos := int(read_zip_u32(data, eocd + 16))
	for _ in 0 .. entries {
		if pos + 46 > data.len || read_zip_u32(data, pos) != 0x0201_4b50 {
			return error('invalid embedded zoneinfo.zip')
		}
		method := read_zip_u16(data, pos + 10)
		size := int(read_zip_u32(data, pos + 24))
		name_len := int(read_zip_u16(data, pos + 28))
		extra_len := int(read_zip_u16(data, pos + 30))
		comment_len := int(read_zip_u16(data, pos + 32))
		local_header := int(read_zip_u32(data, pos + 42))
		if pos + 46 + name_len > data.len {
			return error('invalid embedded zoneinfo.zip')
		}
		entry_name := data[pos + 46..pos + 46 + name_len].bytestr()
		pos += 46 + name_len + extra_len + comment_len
		if entry_name != name {
			continue
		}
		if method != 0 {
			return error('unsupported compressed time zone entry "${name}"')
		}
		return read_zip_file_data(data, local_header, name, size)
	}
	return error('unknown time zone location "${name}"')
}

fn read_zip_file_data(data []u8, pos int, name string, size int) ![]u8 {
	if pos + 30 > data.len || read_zip_u32(data, pos) != 0x0403_4b50 {
		return error('invalid embedded time zone entry "${name}"')
	}
	name_len := int(read_zip_u16(data, pos + 26))
	extra_len := int(read_zip_u16(data, pos + 28))
	data_start := pos + 30 + name_len + extra_len
	data_end := data_start + size
	if data_end > data.len {
		return error('truncated embedded time zone entry "${name}"')
	}
	return data[data_start..data_end].clone()
}

fn find_zip_end_of_central_directory(data []u8) !int {
	max_comment_len := 65_535
	min_pos := if data.len > max_comment_len + 22 { data.len - max_comment_len - 22 } else { 0 }
	for pos := data.len - 22; pos >= min_pos; pos-- {
		if read_zip_u32(data, pos) == 0x0605_4b50 {
			return pos
		}
	}
	return error('invalid embedded zoneinfo.zip')
}

fn read_zip_u16(data []u8, offset int) int {
	if offset + 2 > data.len {
		return 0
	}
	return int(u16(data[offset]) | (u16(data[offset + 1]) << 8))
}

fn read_zip_u32(data []u8, offset int) u32 {
	if offset + 4 > data.len {
		return 0
	}
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[
		offset + 3]) << 24)
}
