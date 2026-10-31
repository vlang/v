module fontstash

fn test_iterator_preserves_c_field_names_when_formatted() {
	iter := C.FONStextIter{}
	assert iter.isize == 0
	assert iter.str().contains('isize: 0')
}
