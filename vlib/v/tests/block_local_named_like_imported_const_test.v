import hash.crc32

// crc32 declares `pub const q = u32(...)`. A local `q` declared inside the block
// that gives `p` its value used to type `p` as that u32 (through the table of
// unique const name suffixes), which truncated the pointer.
fn pointer_through_block(x &u64) &u64 {
	p := unsafe {
		q := x
		q
	}
	return p
}

fn test_block_local_named_like_an_imported_const() {
	value := u64(0x1234567890)
	assert *pointer_through_block(&value) == u64(0x1234567890)
	assert crc32.sum('a'.bytes()) == u32(0xe8b7be43)
}
