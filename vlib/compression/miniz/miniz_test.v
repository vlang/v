module miniz


fn test_miniz() ? {
	uncompressed := "Hello world!"
	compressed := compress(uncompressed.bytes()) ?
	decompressed := decompress(compressed) ?
	assert decompressed == uncompressed.bytes()
}
