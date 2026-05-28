module main

fn read_u32_le_like(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24)
}

fn main() {
	instr := u32(0x910000a5)
	result := (instr & u32(0xffc003ff)) | (u32(0xa0) << 10)
	assert result == u32(0x910280a5)

	sym_addr := u64(0x1000240e0)
	page_off := sym_addr & 0xfff
	result_from_sym := (instr & u32(0xffc003ff)) | (u32(page_off) << 10)
	assert result_from_sym == u32(0x910380a5)

	bytes := [u8(0xa5), 0, 0, 0x91]
	instr_from_bytes := read_u32_le_like(bytes, 0)
	assert instr_from_bytes == u32(0x910000a5)
	result_from_bytes := (instr_from_bytes & u32(0xffc003ff)) | (u32(page_off) << 10)
	assert result_from_bytes == u32(0x910380a5)
}
