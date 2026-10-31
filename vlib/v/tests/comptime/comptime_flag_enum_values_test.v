// `$for member in T.values` must report the bit values of a `@[flag]` enum
// (1, 2, 4, ...), exactly like casting its fields to an integer does.
// See https://github.com/vlang/v/issues/28825
@[flag]
enum FlagPerm {
	read
	write
	exec
}

@[flag]
enum FlagByte as u8 {
	a
	b
	c
	d
	e
	f
	g
	h
}

type FlagPermAlias = FlagPerm

fn flag_values[T]() []i64 {
	mut out := []i64{}
	$for member in T.values {
		out << i64(member.value)
	}
	return out
}

fn flag_members[T]() []T {
	mut out := []T{}
	$for member in T.values {
		out << T(member.value)
	}
	return out
}

fn test_generic_flag_enum_values() {
	assert flag_values[FlagPerm]() == [i64(1), 2, 4]
	assert flag_values[FlagPerm]() == [i64(FlagPerm.read), i64(FlagPerm.write), i64(FlagPerm.exec)]
	assert flag_values[FlagByte]() == [i64(1), 2, 4, 8, 16, 32, 64, 128]
	assert flag_values[FlagPermAlias]() == [i64(1), 2, 4]
}

fn test_generic_flag_enum_values_round_trip() {
	assert flag_members[FlagPerm]() == [FlagPerm.read, .write, .exec]
	assert flag_members[FlagByte]().last() == FlagByte.h
}

fn test_direct_flag_enum_values() {
	mut names := []string{}
	mut values := []int{}
	$for member in FlagPerm.values {
		names << member.name
		values << int(member.value)
	}
	assert names == ['read', 'write', 'exec']
	assert values == [1, 2, 4]
}
