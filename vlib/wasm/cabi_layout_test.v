module wasm

fn test_cabi_primitive_layouts() {
	sz, al := cabi_primitive_size_align('u8')
	assert sz == 1 && al == 1
	sz2, al2 := cabi_primitive_size_align('u16')
	assert sz2 == 2 && al2 == 2
	sz4, al4 := cabi_primitive_size_align('u32')
	assert sz4 == 4 && al4 == 4
	sz8, al8 := cabi_primitive_size_align('u64')
	assert sz8 == 8 && al8 == 8
	szf, alf := cabi_primitive_size_align('f32')
	assert szf == 4 && alf == 4
	szb, alb := cabi_primitive_size_align('bool')
	assert szb == 1 && alb == 1
	bad, bada := cabi_primitive_size_align('string')
	assert bad == -1 && bada == -1
}

fn test_cabi_string_and_list_layouts() {
	ssz, sal := cabi_string_layout()
	assert ssz == 8 && sal == 4
	lsz, lal := cabi_list_layout()
	assert lsz == 8 && lal == 4
}

fn test_cabi_enum_and_flags_layouts() {
	esz, eal := cabi_enum_layout(3)
	assert esz == 1 && eal == 1
	esz2, _ := cabi_enum_layout(300)
	assert esz2 == 2
	esz3, _ := cabi_enum_layout(70000)
	assert esz3 == 4
	fsz, fal := cabi_flags_layout(3)
	assert fsz == 1 && fal == 1
	fsz2, _ := cabi_flags_layout(9)
	assert fsz2 == 2
	fsz3, _ := cabi_flags_layout(17)
	assert fsz3 == 4
}

fn test_cabi_record_layout_flat() {
	// record { x: u8, y: u16 } -> y at offset 2, size 4, align 2.
	offs, size, align := cabi_record_layout([CabiField{'x', 1, 1}, CabiField{'y', 2, 2}])
	assert offs == [0, 2]
	assert size == 4
	assert align == 2
	// record { a: u32, b: u8 } -> b at offset 4, size 8, align 4.
	offs2, size2, align2 := cabi_record_layout([CabiField{'a', 4, 4}, CabiField{'b', 1, 1}])
	assert offs2 == [0, 4]
	assert size2 == 8
	assert align2 == 4
}

fn test_cabi_tuple_layout() {
	offs, size, align := cabi_tuple_layout([CabiField{'0', 4, 4}, CabiField{'1', 8, 8}])
	assert offs == [0, 8]
	assert size == 16
	assert align == 8
}

fn test_cabi_option_result_layouts() {
	// option<u16>: discriminant + u16 payload at offset 2, size 4, align 2.
	osz, oal := cabi_option_layout(2, 2)
	assert osz == 4 && oal == 2
	// option<u32>: payload at offset 4, size 8, align 4.
	osz2, oal2 := cabi_option_layout(4, 4)
	assert osz2 == 8 && oal2 == 4
	// result<u8, u32>: union payload 4, size 8, align 4.
	rsz, ral := cabi_result_layout(1, 1, 4, 4)
	assert rsz == 8 && ral == 4
	// result<_, _> with no payloads: just the discriminant.
	rsz2, ral2 := cabi_result_layout(0, 1, 0, 1)
	assert rsz2 == 1 && ral2 == 1
}

fn test_cabi_variant_layout() {
	// variant { a: u8, b: u32 } with 2 cases: discriminant 1 byte,
	// payload union of (1,1) and (4,4) starts at offset 4, total size 8.
	poff, size, align := cabi_variant_layout(2, [CabiField{'a', 1, 1}, CabiField{'b', 4, 4}])
	assert poff == 4
	assert size == 8
	assert align == 4
	// variant with 300 cases: discriminant 2 bytes.
	poff2, _, _ := cabi_variant_layout(300, [CabiField{'a', 1, 1}])
	assert poff2 == 2
}
