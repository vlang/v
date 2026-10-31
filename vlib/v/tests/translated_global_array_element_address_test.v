@[has_globals; translated]
module main

__global name_bytes = [i8(66), 73, 0]

// Like V1, a translated file takes the address of a global array's element outside
// `unsafe` (C translated by c2v: `z ? z : &sqlite3_str_binary[0]`).
fn pick(z &i8) &i8 {
	return if !isnil(z) { z } else { &name_bytes[0] }
}

fn test_address_of_a_global_array_element_in_a_translated_file() {
	p := pick(unsafe { nil })
	assert unsafe { cstring_to_vstring(&char(p)) } == 'BI'
	assert voidptr(p) == voidptr(name_bytes.data)
}
