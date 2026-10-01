@[aligned: 512]
struct LocalAlignedCell {
	value int
}

type LocalAlignedCells = [2]LocalAlignedCell

fn test_local_aligned_fixed_array_alias_allocation_matches_free() {
	ptr := &LocalAlignedCells{}
	assert u64(voidptr(ptr)) % 512 == 0
	unsafe { free(ptr) }
}
