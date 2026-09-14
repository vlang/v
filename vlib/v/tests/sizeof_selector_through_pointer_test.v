struct Inode {
mut:
	sector_cnt u32
	size32l    u32
	blocks     [15]u32
}

// `sizeof(inode.blocks)` on a pointer receiver used to be emitted as
// `sizeof(inode.blocks)` in C, where `inode` is a pointer, which the C compiler
// rejects: "member reference type 'Inode *' is a pointer; did you mean to use
// '->'". The selector has to follow the step through the pointer.
fn (inode &Inode) block_bytes() u32 {
	return u32(sizeof(inode.blocks))
}

fn (inode &Inode) fits_inline() bool {
	return inode.size32l <= u32(sizeof(inode.blocks))
}

fn value_receiver_block_bytes(inode Inode) u32 {
	return u32(sizeof(inode.blocks))
}

fn test_sizeof_of_a_field_reached_through_a_pointer_receiver() {
	inode := Inode{}
	assert inode.block_bytes() == u32(60)
}

fn test_sizeof_in_a_comparison_through_a_pointer_receiver() {
	mut inode := Inode{}
	inode.size32l = 12
	assert inode.fits_inline()
	inode.size32l = 61
	assert !inode.fits_inline()
}

fn test_sizeof_through_a_value_receiver_still_uses_a_dot() {
	assert value_receiver_block_bytes(Inode{}) == u32(60)
}

// An alias can stand for the pointer. `type Ref = &Inode` records a types.Alias,
// whose C storage is still a pointer, so the alias has to be erased before asking
// -- otherwise a dot is emitted and the C compiler rejects it.
type Ref = &Inode

fn alias_block_bytes(inode Ref) u32 {
	return u32(sizeof(inode.blocks))
}

struct Holder {
mut:
	inode Inode
}

type HolderRef = &Holder

fn alias_chain_bytes(h HolderRef) u32 {
	return u32(sizeof(h.inode))
}

fn test_sizeof_through_a_pointer_alias() {
	mut inode := Inode{}
	assert alias_block_bytes(Ref(&inode)) == u32(60)
}

fn test_sizeof_of_a_struct_field_through_a_pointer_alias() {
	mut h := Holder{}
	assert alias_chain_bytes(HolderRef(&h)) == u32(sizeof(Inode))
}
