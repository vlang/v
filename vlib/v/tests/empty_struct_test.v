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
	assert dump(sizeof(Z0)) <= 1 // valid for all
	$if tinyc {
		// TCC has no problems with 0 sized structs in almost cases,
		// except when they are used in fixed arrays, or their address is taken,
		// in which case, it produces a compilation error. To avoid it, for it
		// empty structs are 1 byte in size.
		assert dump(sizeof(Z0)) == 1
	}
	$if msvc {
		// MSVC seems to have no way at all to have empty structs in C mode. It produces the following error:
		// `error c2016: C requires that a struct or union have at least one member`.
		// Note that MSVC allows empty structs in C++ mode, but that has other restrictions,
		// and is not suitable for the generated code of most V programs. Besides, even in C++  mode, the size of
		// an empty struct is still 1, not 0.
		// For that reason, empty structs are 1 byte in size for MSVC too.
		assert dump(sizeof(Z0)) == 1
	}
	$if clang {
		assert dump(sizeof(Z0)) == 0
	}
	$if gcc {
		assert dump(sizeof(Z0)) == 0
	}
	assert dump(sizeof(Z1)) < sizeof(Z2)
	assert dump(sizeof(Z2)) < sizeof(Z3)
	assert dump(sizeof(Z3)) < sizeof(Z4)
	assert dump(sizeof(Z4)) == 4
}
