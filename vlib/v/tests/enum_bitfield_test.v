[flag]
enum BfPermission {
	read
	write
	execute
	other
}

struct BfFile {
mut:
	perm BfPermission
}

fn test_enum_bitfield() {
	mut a := BfFile{}
	assert 1 == int(BfPermission.read)
	assert 2 == int(BfPermission.write)
	assert 4 == int(BfPermission.execute)
	assert 8 == int(BfPermission.other)
	a.perm.set(.read)
	a.perm.set(.write)
	a.perm.toggle(.execute)
	a.perm.clear(.write)
	// a.perm.set(.other)
	assert a.perm.has(.read)
	assert a.perm.has(.execute)
	assert !a.perm.has(.write)
	assert !a.perm.has(.other)
	mut b := BfPermission(int(BfPermission.read) | int(BfPermission.execute))
	assert int(b) == 1 + 0 + 4 + 0
	assert b.has(.read)
	assert b.has(.execute)
	b.set(.write)
	assert int(b) == 1 + 2 + 4 + 0
	b.set(.other)
	assert int(b) == 1 + 2 + 4 + 8
	assert b.has(.write)
	assert b.has(.other)
	b.toggle(.read)
	assert int(b) == 0 + 2 + 4 + 8
	b.toggle(.execute)
	assert int(b) == 0 + 2 + 0 + 8
	assert !b.has(.read)
	assert !b.has(.execute)
}
