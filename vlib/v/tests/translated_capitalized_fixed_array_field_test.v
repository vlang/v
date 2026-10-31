@[translated]
module main

const guid_tail_len = 8

// A capitalized field name followed by a fixed array type is a regular field,
// not the legacy `Embed [attr]` form (issue #29113).
struct Guid {
	Data1 u32
	Data2 u16
	Data3 u16
	Data4 [8]u8
}

struct FixedArrayFields {
	Tail   [guid_tail_len]u8
	Matrix [2][3]int
	Refs   [2]&Guid
	Named  [2]u8 = [u8(1), 2]!
}

fn test_capitalized_field_with_fixed_array_type() {
	mut g := Guid{
		Data1: 1
	}
	g.Data4[7] = 42
	assert g.Data4.len == 8
	assert g.Data4[7] == 42
	assert g.Data1 == 1

	mut f := FixedArrayFields{}
	f.Matrix[1][2] = 5
	f.Refs[0] = &g
	assert f.Tail.len == guid_tail_len
	assert f.Matrix[1][2] == 5
	assert f.Refs[0].Data4[7] == 42
	assert f.Named == [u8(1), 2]!
}
