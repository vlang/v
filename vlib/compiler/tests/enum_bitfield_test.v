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

fn test_bitfield() {
	mut a := BfFile{}
	a.perm.set(.read)
	a.perm.set(.write)
	a.perm.toggle(.execute)
	a.perm.clear(.write)
	//a.perm.set(.other)

	assert a.perm.has(.read)
	assert a.perm.has(.execute)
	assert !a.perm.has(.write)
	assert !a.perm.has(.other)
}
