module bzip2

import encoding.hex

fn must_decode_hex(s string) []u8 {
	return hex.decode(s) or { panic(err) }
}

fn test_roundtrip_empty_input() {
	src := []u8{}
	compressed := compress(src) or { panic(err) }
	decompressed := decompress(compressed) or { panic(err) }
	assert decompressed == src
}

fn test_roundtrip_small_text() {
	src := 'hello world\n'.bytes()
	compressed := compress(src) or { panic(err) }
	decompressed := decompress(compressed) or { panic(err) }
	assert decompressed == src
}

fn test_roundtrip_binary_data() {
	mut src := []u8{len: 4096}
	for i in 0 .. src.len {
		src[i] = u8((i * 17 + 13) & 0xff)
	}
	compressed := compress(src, block_size: 1) or { panic(err) }
	decompressed := decompress(compressed) or { panic(err) }
	assert decompressed == src
}

fn test_roundtrip_long_runs() {
	src := 'a'.repeat(2000).bytes()
	compressed := compress(src) or { panic(err) }
	decompressed := decompress(compressed) or { panic(err) }
	assert decompressed == src
}

fn test_decompress_known_python_vector_empty() {
	bz2 := must_decode_hex('425a683917724538509000000000')
	plain := decompress(bz2) or { panic(err) }
	assert plain == []u8{}
}

fn test_decompress_known_python_vector_hello() {
	bz2 :=
		must_decode_hex('425a68393141592653594eece83600000251800010400006449080200031064c4101a7a9a580bb9431f8bb9229c28482776741b0')
	plain := decompress(bz2) or { panic(err) }
	assert plain.bytestr() == 'hello world\n'
}

fn test_decompress_known_python_vector_text() {
	bz2 :=
		must_decode_hex('425a6839314159265359dc01b0d8000002d9800010410120080a00cc20200021a4d3688cd0806800e28a3de49f0b16b10d177245385090dc01b0d8')
	plain := decompress(bz2) or { panic(err) }
	assert plain.bytestr() == '1.test\ncopy &copy;\n'
}

fn test_decompress_known_python_vector_repeated_a() {
	bz2 :=
		must_decode_hex('425a6839314159265359ca3d8dfb000000010420000400200021008283177245385090ca3d8dfb')
	plain := decompress(bz2) or { panic(err) }
	assert plain == 'a'.repeat(200).bytes()
}

fn test_decompress_rejects_invalid_header() {
	_ := decompress('not-bzip2'.bytes()) or {
		assert err.msg().contains('invalid header')
		return
	}
	assert false
}

fn test_decompress_rejects_crc_mismatch() {
	mut bz2 :=
		must_decode_hex('425a68393141592653594eece83600000251800010400006449080200031064c4101a7a9a580bb9431f8bb9229c28482776741b0')
	// Corrupt the stored block CRC field (bytes 10..13 in a single-block stream).
	bz2[10] ^= 0x01
	_ := decompress(bz2) or {
		assert err.msg().contains('crc mismatch')
		return
	}
	assert false
}

fn test_selector_count_limit_boundaries() {
	below_limit := selector_count_from_symbol_count(900050) or { panic(err) }
	at_limit := selector_count_from_symbol_count(900100) or { panic(err) }
	assert below_limit == 18001
	assert at_limit == 18002

	_ := selector_count_from_symbol_count(900101) or {
		assert err.msg().contains('invalid selector count')
		return
	}
	assert false
}

fn test_block_output_limit_guard() {
	ensure_block_output_limit(0, 100000, 100000) or { panic(err) }
	ensure_block_output_limit(99999, 1, 100000) or { panic(err) }

	ensure_block_output_limit(100000, 1, 100000) or {
		assert err.msg().contains('block output exceeds declared block size')
		return
	}
	assert false
}
