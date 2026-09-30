// Like V1, a computed address may be cast to a struct pointer outside `unsafe`;
// casting a literal or a plain number still needs `unsafe`.
struct ComputedCastEntry {
	inode u32
	len   u16
}

fn test_computed_address_casts_to_a_struct_pointer() {
	entries := [ComputedCastEntry{
		inode: 7
		len:   3
	}, ComputedCastEntry{
		inode: 9
		len:   4
	}]
	base := u64(entries.data)
	second := &ComputedCastEntry(base + sizeof(ComputedCastEntry))
	assert second.inode == 9
	assert second.len == 4
}
