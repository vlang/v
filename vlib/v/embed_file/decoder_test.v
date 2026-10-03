module embed_file

struct XorDecoder {}

fn (decoder XorDecoder) decompress(data []u8) ![]u8 {
	return data.map(it ^ u8(0x5a))
}

fn test_registered_decoder_data_is_materialized_and_cached() {
	compression_type := 'embed_file_test_xor'
	register_decoder(compression_type, XorDecoder{})
	defer { g_embed_file_decoders.decoders.delete(compression_type) }
	compressed := [u8(0), 17, 128, 255]
	file := EmbedFileData{
		compression_type: compression_type
		compressed:       &compressed[0]
		compressed_len:   compressed.len
		len:              compressed.len
		path:             'test.bin'
	}
	first := file.data()
	assert unsafe { first.vbytes(file.len) } == [u8(0x5a), 0x4b, 0xda, 0xa5]
	assert file.data() == first
	assert file.to_bytes() == [u8(0x5a), 0x4b, 0xda, 0xa5]
}
