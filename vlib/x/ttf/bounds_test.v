module ttf

fn test_map_code_rejects_values_outside_supported_character_maps() {
	mut font := TTF_File{
		cmaps: [TrueTypeCmap{ format: 4, cache: []int{len: 65536, init: -1} }]
	}
	assert font.map_code(-1) == 0
	assert font.map_code(65536) == 0
	assert font.map_code(0x10ffff) == 0
}

fn test_filler_methods_initialize_missing_rows() {
	mut bitmap := BitMap{ width: 2, height: 2 }
	bitmap.exec_filler()
	assert bitmap.filler.len == 2
	bitmap.clear_filler()
	for row in bitmap.filler {
		assert row.len == 0
	}
	bitmap.height = 4
	bitmap.clear_filler()
	assert bitmap.filler.len == 4
	bitmap.exec_filler()
}
