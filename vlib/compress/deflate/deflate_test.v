module deflate

const gzip_magic_numbers = [u8(0x1f), 0x8b]

fn test_gzip() {
	uncompressed := 'Hello world!'
	compressed := pack(uncompressed.bytes())!
	first2 := compressed[0..2]
	assert first2 != deflate.gzip_magic_numbers
	decompressed := unpack(compressed)!
	assert decompressed == uncompressed.bytes()
}
