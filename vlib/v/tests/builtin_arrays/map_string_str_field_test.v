// `it.str` is the `str` field of a string (its C data pointer), not the `str()`
// method, so `.map(it.str)` maps to the field values.
fn test_map_reads_the_str_field_of_strings() {
	varyings := ['abc', 'de']
	names := varyings.map(voidptr(it.str))
	assert names.len == 2
	for i, name in names {
		assert name == voidptr(varyings[i].str)
		assert unsafe { cstring_to_vstring(&char(name)) } == varyings[i]
	}
	pointers := varyings.map(it.str)
	assert unsafe { cstring_to_vstring(&char(pointers[1])) } == 'de'
	assert varyings.map(int(it.len)) == [3, 2]
}
