const const_file = $embed_file('v.png')

fn test_const_embed_file() {
	mut file := const_file
	eprintln('file: $file')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: $file')
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [byte(0x89), `P`, `N`, `G`]
	}
}

fn test_embed_file() {
	mut file := $embed_file('v.png')
	eprintln('file: $file')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: $file')
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [byte(0x89), `P`, `N`, `G`]
	}
}
