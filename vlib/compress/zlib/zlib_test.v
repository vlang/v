module zlib

fn test_zlib() {
	uncompressed := 'Hello world!'
	compressed := pack(uncompressed.bytes())!
	decompressed := unpack(compressed)!
	assert decompressed == uncompressed.bytes()
}
