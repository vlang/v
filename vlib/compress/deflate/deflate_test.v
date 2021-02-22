import compress.deflate

fn test_compress() {
	mut test_data := 'Hellooo, wwwwwooorld!'.bytes()

	mut compressed := deflate.compress(test_data) or {
		assert false
		return
	}

	mut uncompressed := deflate.uncompress(compressed) or {
		assert false
		return
	}

	assert test_data == uncompressed
	compressed[3] = byte(1)

	uncompressed = deflate.uncompress(compressed) or {
		assert 'data error' == err
		return
	}

	assert test_data == uncompressed
}
