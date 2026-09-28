struct DumpOffsetPacket {
	lead  u8
	value int
}

fn test_dump_offsetof_uses_usize_storage() {
	offset := dump(__offsetof(DumpOffsetPacket, value))
	assert offset > 0
}
