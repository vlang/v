import v.embed_file

fn test_embed_file_with_import() {
	mut file := $embed_file('v.png')
	eprintln('file: ${file}')
	assert file.len == 603
	fdata := file.data()
	eprintln('file after .data() call: ${file}')
	assert file.len == 603
	unsafe {
		assert fdata.vbytes(4) == [u8(0x89), `P`, `N`, `G`]
	}
	assert check_file(file)
}

fn check_file(file embed_file.EmbedFileData) bool {
	return file.len == 603
}
