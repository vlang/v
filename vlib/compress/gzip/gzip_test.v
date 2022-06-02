module gzip

fn test_gzip() ? {
	uncompressed := 'Hello world!'
	compressed := compress(uncompressed.bytes())?
	decompressed := decompress(compressed)?
	assert decompressed == uncompressed.bytes()
}
