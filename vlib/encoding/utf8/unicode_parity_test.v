module utf8

// Parity with Go's unicode package over every code point, 0 to 0x10FFFF.
//
// The checksums are Go 1.26.1's own answers. A per-code-point golden file would
// be 1,114,112 lines, so instead each predicate is checksummed: any difference
// anywhere in the range changes the checksum. The counts are asserted too, so a
// mismatch also says how many code points are involved.
//
// When one of these fails, the failing predicate is the one named in the
// message; locate the differing code points by bisecting the range.

const fnv_offset = u64(0xcbf29ce484222325)
const fnv_prime = u64(0x100000001b3)

const go_is_letter_hash = u64(13616155193125678021)
const go_is_number_hash = u64(3955269138510666186)
const go_is_rune_punct_hash = u64(4940863109027226707)
const go_is_space_hash = u64(420366091043129260)
const go_is_control_hash = u64(15556758279732414322)
const go_not_letter_hash = u64(646384382061300257)
const go_not_space_hash = u64(3036783156973015008)
const go_not_punct_hash = u64(13612170359890156487)

const go_letter_count = 136104
const go_number_count = 1831
const go_punct_count = 842
const go_space_count = 25
const go_control_count = 65

struct ParityFnv {
mut:
	h u64
}

fn (mut f ParityFnv) b(v u8) {
	f.h ^= u64(v)
	f.h *= fnv_prime
}

fn parity_bit(v bool) u8 {
	return if v { 1 } else { 0 }
}

fn test_is_letter_matches_go() {
	mut f := ParityFnv{}
	f.h = fnv_offset
	mut fn_ := ParityFnv{}
	fn_.h = fnv_offset
	mut count := 0
	for i in 0 .. 0x10FFFF + 1 {
		r := rune(i)
		yes := is_letter(r)
		f.b(parity_bit(yes))
		fn_.b(parity_bit(!yes))
		if yes {
			count++
		}
	}
	assert count == go_letter_count, 'is_letter count ${count} != ${go_letter_count}'
	assert f.h == go_is_letter_hash, 'is_letter checksum ${f.h} != ${go_is_letter_hash}'
	assert fn_.h == go_not_letter_hash, 'is_letter negative checksum mismatch'
}

fn test_is_number_matches_go() {
	mut f := ParityFnv{}
	f.h = fnv_offset
	mut count := 0
	for i in 0 .. 0x10FFFF + 1 {
		yes := is_number(rune(i))
		f.b(parity_bit(yes))
		if yes {
			count++
		}
	}
	assert count == go_number_count, 'is_number count ${count} != ${go_number_count}'
	assert f.h == go_is_number_hash, 'is_number checksum ${f.h} != ${go_is_number_hash}'
}

fn test_is_rune_punct_matches_go() {
	mut f := ParityFnv{}
	f.h = fnv_offset
	mut fn_ := ParityFnv{}
	fn_.h = fnv_offset
	mut count := 0
	for i in 0 .. 0x10FFFF + 1 {
		r := rune(i)
		yes := is_rune_punct(r)
		f.b(parity_bit(yes))
		fn_.b(parity_bit(!yes))
		if yes {
			count++
		}
	}
	assert count == go_punct_count, 'is_rune_punct count ${count} != ${go_punct_count}'
	assert f.h == go_is_rune_punct_hash, 'is_rune_punct checksum ${f.h} != ${go_is_rune_punct_hash}'
	assert fn_.h == go_not_punct_hash, 'is_rune_punct negative checksum mismatch'
}

fn test_is_space_matches_go() {
	mut f := ParityFnv{}
	f.h = fnv_offset
	mut fn_ := ParityFnv{}
	fn_.h = fnv_offset
	mut count := 0
	for i in 0 .. 0x10FFFF + 1 {
		r := rune(i)
		yes := is_space(r)
		f.b(parity_bit(yes))
		fn_.b(parity_bit(!yes))
		if yes {
			count++
		}
	}
	assert count == go_space_count, 'is_space count ${count} != ${go_space_count}'
	assert f.h == go_is_space_hash, 'is_space checksum ${f.h} != ${go_is_space_hash}'
	assert fn_.h == go_not_space_hash, 'is_space negative checksum mismatch'
}

fn test_is_control_matches_go() {
	mut f := ParityFnv{}
	f.h = fnv_offset
	mut count := 0
	for i in 0 .. 0x10FFFF + 1 {
		yes := is_control(rune(i))
		f.b(parity_bit(yes))
		if yes {
			count++
		}
	}
	assert count == go_control_count, 'is_control count ${count} != ${go_control_count}'
	assert f.h == go_is_control_hash, 'is_control checksum ${f.h} != ${go_is_control_hash}'
}
