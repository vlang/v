module brotli

import encoding.hex

fn test_decode_dictionary_utf8_uppercase_first() {
	// Reference Brotli decodes this static dictionary transform as "За".
	data := hex.decode('6200000004400812e6eb01')!
	assert decompress(data)!.bytestr() == 'За'
}

fn test_decode_dictionary_utf8_uppercase_all() {
	data := hex.decode('6200000004400812eaeb0c')!
	assert decompress(data)!.bytestr() == 'ЗА'
}

fn test_decode_dictionary_three_byte_uppercase_first() {
	data := hex.decode('6200000004400812e68401')!
	assert decompress(data)!.bytestr() == '“s'
}

fn test_decode_dictionary_three_byte_uppercase_all() {
	data := hex.decode('6200000004400812ea840c')!
	assert decompress(data)!.bytestr() == '“S'
}

fn test_decode_dictionary_copy_shortened_below_base_word_length() {
	data := hex.decode('4200000004400812270100')!
	assert decompress(data)!.bytestr() == 'tim'
}

fn test_decode_dictionary_copy_omitting_entire_word_before_literal() {
	data := hex.decode('020000004450098248b0080001')!
	assert decompress(data)!.bytestr() == 'A'
}

fn test_reject_dictionary_transform_exceeding_metablock() {
	data := hex.decode('62000000044008122001')!
	if _ := decompress(data) {
		assert false, 'the transformed dictionary word must fit the remaining metablock'
	} else {
		assert err.msg() == 'brotli: transformed dictionary word exceeds metablock'
	}
}

fn test_reject_metadata_alignment_padding() {
	data := hex.decode('8c03')!
	if _ := decompress(data) {
		assert false, 'metadata alignment padding must be zero'
	} else {
		assert err.msg() == 'brotli: non-zero padding before metadata block'
	}
}

fn test_reject_uncompressed_alignment_padding() {
	data := hex.decode('0000304103')!
	if _ := decompress(data) {
		assert false, 'uncompressed alignment padding must be zero'
	} else {
		assert err.msg() == 'brotli: non-zero padding before uncompressed block'
	}
}

fn test_reject_noncanonical_metadata_length() {
	data := hex.decode('4c00004203')!
	if _ := decompress(data) {
		assert false, 'metadata length must not have a redundant high byte'
	} else {
		assert err.msg() == 'brotli: non-canonical metadata length'
	}
}

fn test_decode_metadata_with_zero_alignment_padding() {
	assert decompress(hex.decode('0c03')!)! == []u8{}
}

fn test_decode_uncompressed_with_zero_alignment_padding() {
	assert decompress(hex.decode('0000104103')!)!.bytestr() == 'A'
}

fn test_decode_metadata_with_canonical_length() {
	assert decompress(hex.decode('2c004203')!, comment: 'B'.bytes())! == []u8{}
}
