fn test_logo_can_be_embedded_using_a_path_with_vexeroot() {
	logo := $embed_file('@VEXEROOT/examples/assets/logo.png')
	assert unsafe { logo.data().vbytes(4) } == [u8(0x89), `P`, `N`, `G`]
	assert logo.len > 1000
}
