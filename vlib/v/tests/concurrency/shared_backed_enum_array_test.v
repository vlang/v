// vtest vflags: -enable-globals
// A `shared` value is stored in a wrapper struct. For an enum with a backing type, and for
// fixed arrays of one, the wrapper has to store the backing type that reads and writes use,
// not a 4 byte `int` per element. Values are copied out of the locks before the asserts, so
// a failing assert does not leave a lock held for the next test.
enum Small as u8 {
	one = 1
	two
}

enum Half as u16 {
	x = 7
	y = 300
}

enum Neg as i8 {
	m = -3
	n = 4
}

enum Wide as u64 {
	a = 5
	b = 0x1_0000_0000
}

enum Plain {
	p = 2
	q
}

struct Holder {
mut:
	smalls shared [2]Small
	wides  shared []Wide
	one    shared Wide
}

__global g_smalls shared [2]Small
__global g_halves shared [3]Half
__global g_negs shared [2]Neg
__global g_wides shared [2]Wide
__global g_nested shared [2][2]Small
__global g_plains shared [2]Plain
__global g_wide shared Wide

fn test_shared_backed_enum_fixed_array_globals_default_to_the_first_member() {
	mut smalls := [Small.two, .two]!
	rlock g_smalls {
		smalls = g_smalls
	}
	mut halves := [Half.y, .y, .y]!
	rlock g_halves {
		halves = g_halves
	}
	mut negs := [Neg.n, .n]!
	rlock g_negs {
		negs = g_negs
	}
	mut wides := [Wide.b, .b]!
	rlock g_wides {
		wides = g_wides
	}
	mut nested := [[Small.two, .two]!, [Small.two, .two]!]!
	rlock g_nested {
		nested = g_nested
	}
	mut plains := [Plain.q, .q]!
	rlock g_plains {
		plains = g_plains
	}
	assert smalls == [Small.one, .one]!
	assert halves == [Half.x, .x, .x]!
	assert negs == [Neg.m, .m]!
	assert wides == [Wide.a, .a]!
	assert nested == [[Small.one, .one]!, [Small.one, .one]!]!
	assert plains == [Plain.p, .p]!
}

fn test_shared_backed_enum_fixed_array_global_writes() {
	lock g_smalls, g_halves, g_negs, g_wides, g_nested {
		g_smalls[1] = .two
		g_halves[2] = .y
		g_negs[0] = .n
		g_wides[1] = .b
		g_nested[1][0] = .two
	}
	mut smalls := [Small.two, .two]!
	rlock g_smalls {
		smalls = g_smalls
	}
	mut halves := [Half.y, .y, .y]!
	rlock g_halves {
		halves = g_halves
	}
	mut negs := [Neg.n, .n]!
	rlock g_negs {
		negs = g_negs
	}
	mut wides := [Wide.b, .b]!
	rlock g_wides {
		wides = g_wides
	}
	mut nested := [[Small.two, .two]!, [Small.two, .two]!]!
	rlock g_nested {
		nested = g_nested
	}
	assert smalls == [Small.one, .two]!
	assert halves == [Half.x, .x, .y]!
	assert negs == [Neg.n, .m]!
	assert wides == [Wide.a, .b]!
	assert u64(wides[1]) == 0x1_0000_0000
	assert nested == [[Small.one, .one]!, [Small.two, .one]!]!
}

fn test_shared_wide_enum_global_keeps_all_64_bits() {
	before := rlock g_wide {
		g_wide
	}
	lock g_wide {
		g_wide = .b
	}
	after := rlock g_wide {
		g_wide
	}
	assert before == .a
	assert after == .b
	assert u64(after) == 0x1_0000_0000
}

fn test_shared_backed_enum_fixed_array_locals() {
	shared smalls := [2]Small{}
	mut smalls_before := [Small.two, .two]!
	rlock smalls {
		smalls_before = smalls
	}
	lock smalls {
		smalls[0] = .two
	}
	mut smalls_after := [Small.two, .two]!
	rlock smalls {
		smalls_after = smalls
	}
	assert smalls_before == [Small.one, .one]!
	assert smalls_after == [Small.two, .one]!
	shared wides := [Wide.b, .a]!
	lock wides {
		wides[1] = .b
	}
	mut wides_after := [Wide.a, .a]!
	rlock wides {
		wides_after = wides
	}
	assert wides_after == [Wide.b, .b]!
	assert u64(wides_after[1]) == 0x1_0000_0000
}

fn test_shared_backed_enum_struct_fields() {
	mut h := Holder{}
	mut smalls_before := [Small.two, .two]!
	rlock h.smalls {
		smalls_before = h.smalls
	}
	wides_len_before := rlock h.wides {
		h.wides.len
	}
	one_before := rlock h.one {
		h.one
	}
	assert smalls_before == [Small.one, .one]!
	assert wides_len_before == 0
	assert one_before == .a
	lock h.smalls, h.wides, h.one {
		h.smalls[1] = .two
		h.wides << .b
		h.wides << .a
		h.one = .b
	}
	mut smalls_after := [Small.two, .two]!
	rlock h.smalls {
		smalls_after = h.smalls
	}
	wides_after := rlock h.wides {
		h.wides.clone()
	}
	one_after := rlock h.one {
		h.one
	}
	assert smalls_after == [Small.one, .two]!
	assert wides_after == [Wide.b, .a]
	assert u64(wides_after[0]) == 0x1_0000_0000
	assert u64(one_after) == 0x1_0000_0000
	h2 := Holder{
		smalls: [Small.two, .one]!
	}
	mut smalls2 := [Small.two, .two]!
	rlock h2.smalls {
		smalls2 = h2.smalls
	}
	assert smalls2 == [Small.two, .one]!
}
