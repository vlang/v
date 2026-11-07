module embed_file

fn test_to_string_copies_nonterminated_data() {
	mut payload := [u8(`V`), `l`, `a`, `n`, `g`]!
	file := EmbedFileData{
		uncompressed: unsafe { &payload[0] }
		len:          payload.len
	}
	text := file.to_string()
	defer { unsafe { text.free() } }
	assert text == 'Vlang'
	assert text.len == payload.len
	assert unsafe { text.str[text.len] } == 0
	assert text.str != unsafe { &payload[0] }
	payload[0] = `X`
	assert text == 'Vlang'
}

fn test_to_string_terminates_empty_data() {
	payload := [u8(0xa5)]!
	file := EmbedFileData{
		uncompressed: unsafe { &payload[0] }
		len:          0
	}
	text := file.to_string()
	defer { unsafe { text.free() } }
	assert text == ''
	assert text.len == 0
	assert unsafe { text.str[0] } == 0
}

fn test_to_string_preserves_embedded_zero_bytes() {
	payload := [u8(`a`), 0, `b`, 0xff]!
	file := EmbedFileData{
		uncompressed: unsafe { &payload[0] }
		len:          payload.len
	}
	text := file.to_string()
	defer { unsafe { text.free() } }
	assert text.len == payload.len
	assert text.bytes() == [u8(`a`), 0, `b`, 0xff]
	assert unsafe { text.str[text.len] } == 0
}
