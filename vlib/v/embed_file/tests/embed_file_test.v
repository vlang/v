const const_file = $embed_file('v.png')

const src = $embed_file('embed_file_test.v').to_string()

fn test_const_embed_file_to_string() {
	assert src.len > 0
	assert src.split_into_lines()[0].starts_with('const const_file')
	assert src.split_into_lines().last() == '}'
}

fn test_const_embed_file() {
	mut file := const_file
	eprintln('file: $file')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: $file')
	assert file.path == 'v.png'
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [u8(0x89), `P`, `N`, `G`]
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
		assert fdata.vbytes(4) == [u8(0x89), `P`, `N`, `G`]
	}
}
