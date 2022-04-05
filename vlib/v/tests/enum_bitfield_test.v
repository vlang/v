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
}

fn test_enum_bitfield_operators() {
	mut b := BfPermission.read | BfPermission.execute
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

fn test_enum_bitfield_has_vs_all_methods_with_combined_flags() {
	mut c := BfPermission.read
	c.set(.write | .execute)
	assert c.has(.read | .write | .execute)
	assert !c.has(.other)
	assert c.has(.read | .write | .execute | .other)
	// .all() tests if *ALL* of the given flags are set, i.e. not just any one of them.
	// .has() tests if *ANY* of the given flags is set, even though some of the others may not be.
	assert c.all(.read | .write | .execute)
	assert !c.all(.read | .write | .execute | .other)
}

fn test_enum_bitfield_has_vs_all_methods_with_combined_flags_2() {
	mut c := BfPermission.read | .execute | .other
	//
	assert c.has(.read | .execute | .other | .write)
	assert c.has(.read | .write)
	assert !c.all(.read | .execute | .other | .write)
	assert !c.all(.read | .write)
	//
	assert c.all(.read | .execute | .other)
	assert c.all(.read | .execute)
	assert c.all(.execute | .other)
	assert c.all(.read | .other)
	//
	assert c.has(.read | .execute | .other)
	assert c.has(.read | .execute)
	assert c.has(.execute | .other)
	assert c.has(.read | .other)
}

fn test_enum_bitfield_clear_all() {
	mut a := BfFile{}

	a.perm.set(.read)
	assert a.perm.has(.read)
	a.perm.set(.write)
	assert a.perm.has(.write)
	a.perm.set(.execute)
	assert a.perm.has(.execute)
	a.perm.set(.other)
	assert a.perm.has(.other)

	a.perm.clear_all()

	assert !a.perm.has(.read)
	assert !a.perm.has(.execute)
	assert !a.perm.has(.write)
	assert !a.perm.has(.other)
}
