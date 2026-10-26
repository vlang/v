module modulecache

import os

fn test_file_signature_preserves_content_hash_across_partial_chunks() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_file_signature_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.bin')
	assert file_signature(path) == ''
	os.write_file(path, '')!
	assert file_signature(path) == header_signature('')
	mut content := []u8{len: 2 * 64 * 1024 + 17, init: u8(index % 251)}
	os.write_file_array(path, content)!
	before := file_signature(path)
	assert before == header_signature(content.bytestr())
	content[content.len - 1] ^= 0xff
	os.write_file_array(path, content)!
	assert file_signature(path) == header_signature(content.bytestr())
	assert file_signature(path) != before
}
