module zlib

fn test_zlib() {
	uncompressed := 'Hello world!'
	compressed := compress(uncompressed.bytes())!
	decompressed := decompress(compressed)!
	assert decompressed == uncompressed.bytes()
}
