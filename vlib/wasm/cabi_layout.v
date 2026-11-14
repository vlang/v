module wasm

// cabi_primitive_size_align maps a canonical-ABI primitive name to
// (size, align) in bytes, following the Component Model Canonical ABI
// element-size table. Returns (-1, -1) for non-primitive names
// ('string' is not a primitive here; use cabi_string_layout).
pub fn cabi_primitive_size_align(prim string) (int, int) {
	match prim {
		'bool', 'u8', 's8' { return 1, 1 }
		'u16', 's16' { return 2, 2 }
		'u32', 's32', 'char', 'f32' { return 4, 4 }
		'u64', 's64', 'f64' { return 8, 8 }
		else { return -1, -1 }
	}
}

// cabi_align_up rounds off up to a multiple of align.
pub fn cabi_align_up(off int, align int) int {
	if align <= 1 {
		return off
	}
	return (off + align - 1) / align * align
}

// cabi_string_layout is the canonical string layout: (ptr, len), 8 bytes,
// align 4. Lists use the same shape; see cabi_list_layout.
pub fn cabi_string_layout() (int, int) {
	return 8, 4
}

// cabi_list_layout is the canonical list layout: (ptr, len), 8 bytes,
// align 4, regardless of element type. Element bytes live in linear memory
// allocated via cabi_realloc; the pair itself travels inline.
pub fn cabi_list_layout() (int, int) {
	return 8, 4
}

// cabi_discriminant_size_align sizes a case discriminant for ncases cases:
// 1 byte up to 256 cases, 2 bytes up to 65536, else 4.
pub fn cabi_discriminant_size_align(ncases int) (int, int) {
	if ncases <= 256 {
		return 1, 1
	} else if ncases <= 65536 {
		return 2, 2
	}
	return 4, 4
}

// cabi_enum_layout sizes a WIT enum by case count (same rule as a bare
// discriminant, no payload).
pub fn cabi_enum_layout(ncases int) (int, int) {
	return cabi_discriminant_size_align(ncases)
}

// cabi_flags_layout sizes a WIT flags set with nflags bits: the bitfield is
// rounded up to a whole number of bytes, then to 1, 2, or 4 (align equals
// size). A zero-flag set still occupies one byte.
pub fn cabi_flags_layout(nflags int) (int, int) {
	nbytes := if nflags <= 0 { 1 } else { (nflags + 7) / 8 }
	if nbytes <= 1 {
		return 1, 1
	} else if nbytes == 2 {
		return 2, 2
	}
	return 4, 4
}

// CabiField is one record/tuple field or variant case payload layout.
pub struct CabiField {
pub:
	name  string
	size  int
	align int
}

// cabi_record_layout lays out fields in order with canonical alignment:
// each field starts at align_up(offset, field.align); the record size is
// align_up(end, max_align) and its align is max field align (min 1).
// Returns the per-field offsets plus (size, align).
pub fn cabi_record_layout(fields []CabiField) ([]int, int, int) {
	mut offsets := []int{cap: fields.len}
	mut off := 0
	mut max_align := 1
	for f in fields {
		a := if f.align < 1 { 1 } else { f.align }
		if a > max_align {
			max_align = a
		}
		off = cabi_align_up(off, a)
		offsets << off
		off += f.size
	}
	return offsets, cabi_align_up(off, max_align), max_align
}

// cabi_tuple_layout is identical to a record layout without field names.
pub fn cabi_tuple_layout(items []CabiField) ([]int, int, int) {
	return cabi_record_layout(items)
}

// cabi_option_layout lays out option<T>: a 1-byte discriminant (two cases)
// followed by the payload at its own alignment. Size is
// align_up(1, payload_align) + payload_size, aligned up to payload_align;
// align is max(1, payload_align). A payload of size 0 still reserves the
// discriminant byte.
pub fn cabi_option_layout(payload_size int, payload_align int) (int, int) {
	a := if payload_align < 1 { 1 } else { payload_align }
	off := cabi_align_up(1, a) + payload_size
	return cabi_align_up(off, a), a
}

// cabi_result_layout lays out result<T, E>: a 1-byte discriminant plus the
// larger of the ok/err payloads (union). Align is the max of the payload
// aligns (min 1); size is the discriminant slot plus the max payload size,
// aligned up to that align. Either side may be absent (size 0, align 1).
pub fn cabi_result_layout(ok_size int, ok_align int, err_size int, err_align int) (int, int) {
	mut a := ok_align
	if err_align > a {
		a = err_align
	}
	if a < 1 {
		a = 1
	}
	mut payload := ok_size
	if err_size > payload {
		payload = err_size
	}
	off := cabi_align_up(1, a) + payload
	return cabi_align_up(off, a), a
}

// cabi_variant_layout lays out a variant with ncases cases: the discriminant
// is sized by case count, then the largest case payload follows at its own
// alignment. Cases carry their payload layout in order; a case with no
// payload contributes size 0, align 1. Returns (payload_offset, size, align)
// where payload_offset is the discriminant slot rounded up to the max case
// align (i.e. where any payload starts).
pub fn cabi_variant_layout(ncases int, cases []CabiField) (int, int, int) {
	dsize, _ := cabi_discriminant_size_align(ncases)
	mut max_align := 1
	mut max_size := 0
	for c in cases {
		a := if c.align < 1 { 1 } else { c.align }
		if a > max_align {
			max_align = a
		}
		if c.size > max_size {
			max_size = c.size
		}
	}
	payload_off := cabi_align_up(dsize, max_align)
	return payload_off, cabi_align_up(payload_off + max_size, max_align), max_align
}
