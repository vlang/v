struct Z0 {}

struct Z1 {
	padding1 char
}

struct Z2 {
	padding1 char
	padding2 char
}

struct Z3 {
	padding1 char
	padding2 char
	padding3 char
}

struct Z4 {
	padding1 char
	padding2 char
	padding3 char
	padding4 char
}

fn test_struct_sizes() {
	assert dump(sizeof(Z0)) < 2
	$if tinyc {
		assert dump(sizeof(Z0)) == 1
	} $else $if msvc {
		assert dump(sizeof(Z0)) == 1
	} $else {
		assert dump(sizeof(Z0)) == 0
	}
	assert dump(sizeof(Z1)) < sizeof(Z2)
	assert dump(sizeof(Z2)) < sizeof(Z3)
	assert dump(sizeof(Z3)) < sizeof(Z4)
	assert dump(sizeof(Z4)) == 4
}
