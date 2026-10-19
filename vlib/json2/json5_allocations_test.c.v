// vtest vflags: -gc none -d track_heap -nocache
module json2

import os

#include "@VMODROOT/vlib/json2/testdata/json5_heap_tracking.h"

fn C.json5_heap_live_bytes() u64

struct Json5PortOnly {
	port int
}

struct Json5KeyEncodingRecord {
	enabled bool
	unicode bool @[json: 'clé']
	quoted  bool @[json: 'with space']
}

@[manualfree]
fn assert_json5_unquoted_encoding_releases_temporaries[T](value T, expected string) {
	// Reuse caller-owned storage so the measurement isolates per-call scratch.
	mut output := []u8{cap: 4096}
	encode_append(value, mut output, json5: true, unquoted_keys: true)
	first := output.bytestr()
	assert first == expected
	unsafe { first.free() }
	before := C.json5_heap_live_bytes()
	for _ in 0 .. 100 {
		output.clear()
		encode_append(value, mut output, json5: true, unquoted_keys: true)
		result := output.bytestr()
		assert result == expected
		unsafe { result.free() }
	}
	assert C.json5_heap_live_bytes() == before
	unsafe { output.free() }
}

@[manualfree]
fn test_json5_unquoted_map_keys_release_encoding_temporaries() {
	value := {
		'$key0':      true
		'a\u0903':    true
		'a\u203f':    false
		'a\u00b2':    false
		'clé':        true
		'with space': false
		'\U00010341': true
	}
	assert_json5_unquoted_encoding_releases_temporaries(value,
		'{$key0:true,"a\u00b2":false,a\u0903:true,a\u203f:false,clé:true,"with space":false,\U00010341:true}')
}

@[manualfree]
fn test_json5_unquoted_struct_keys_release_encoding_temporaries() {
	value := Json5KeyEncodingRecord{
		enabled: true
		quoted:  true
	}
	assert_json5_unquoted_encoding_releases_temporaries(value,
		'{enabled:true,clé:false,"with space":true}')
}

@[manualfree]
fn test_json5_integer_decoding_releases_temporary_storage() {
	for input in ['123', "'123'", '0x7b', '+123', '-123'] {
		expected := if input == '-123' { -123 } else { 123 }
		assert decode[int](input, json5: true)! == expected
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 1000 {
			assert decode[int](input, json5: true)! == expected
		}
		assert C.json5_heap_live_bytes() == before
	}
}

@[manualfree]
fn test_json5_large_hexadecimal_float_decoding_releases_temporary_storage() {
	for input in ['0x10000000000000000', '-0x10000000000000000',
		'0x' + '0'.repeat(1024) + '10000000000000000'] {
		expected := if input.starts_with('-') {
			-18446744073709551616.0
		} else {
			18446744073709551616.0
		}
		assert decode[f64](input, json5: true)! == expected
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 100 {
			assert decode[f64](input, json5: true)! == expected
		}
		assert C.json5_heap_live_bytes() == before
	}
}

@[manualfree]
fn test_json5_reuse_releases_normalized_text_of_ignored_fields() {
	for prefix in ["{port:123,ignored:'", r"{\u0070ort:123,ig\u006eored:'"] {
		input := prefix + 'x'.repeat(8192) + "'}"
		mut buffer := DecodeBuffer{}
		assert decode_reuse[Json5PortOnly](input, mut buffer, json5: true)!.port == 123
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 100 {
			assert decode_reuse[Json5PortOnly](input, mut buffer, json5: true)!.port == 123
		}
		assert C.json5_heap_live_bytes() == before
		unsafe { buffer.values_info.free() }
	}
}

@[manualfree]
fn test_json5_string_results_own_only_their_returned_text() {
	input := "'" + 'x'.repeat(8192) + "'"
	first := decode[string](input, json5: true)!
	unsafe { first.free() }
	before := C.json5_heap_live_bytes()
	for _ in 0 .. 100 {
		decoded := decode[string](input, json5: true)!
		assert decoded.len == 8192
		unsafe { decoded.free() }
	}
	assert C.json5_heap_live_bytes() == before
}

@[manualfree]
fn test_decode_any_releases_serialization_scratch_for_numbers() {
	for json5 in [false, true] {
		for number in [0, 1, 123] {
			value := Any(number)
			input := number.str()
			assert decode_any[int](value, json5: json5)! == number
			// Plain JSON validation allocates EOF error sentinels even on success.
			// Measure direct decoding so the assertion isolates wrapper scratch storage.
			baseline := C.json5_heap_live_bytes()
			for _ in 0 .. 100 {
				assert decode[int](input, json5: json5)! == number
			}
			decoder_allocations := C.json5_heap_live_bytes() - baseline
			before := C.json5_heap_live_bytes()
			for _ in 0 .. 100 {
				assert decode_any[int](value, json5: json5)! == number
			}
			assert C.json5_heap_live_bytes() == before + decoder_allocations
		}
	}
}

@[manualfree]
fn test_decode_any_string_results_own_only_their_returned_text() {
	text := 'x'.repeat(8192)
	value := Any(text)
	for json5 in [false, true] {
		first := decode_any[string](value, json5: json5)!
		unsafe { first.free() }
		input := '"${text}"'
		baseline := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			decoded := decode[string](input, json5: json5)!
			unsafe { decoded.free() }
		}
		decoder_allocations := C.json5_heap_live_bytes() - baseline
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			decoded := decode_any[string](value, json5: json5)!
			assert decoded == text
			unsafe { decoded.free() }
		}
		assert C.json5_heap_live_bytes() == before + decoder_allocations
	}
}

@[manualfree]
fn test_decode_any_escaped_strings_release_encoding_temporaries() {
	text := '\x00\x01\n\r\t\b\f\\"' + rune(0x2028).str() + rune(0x2029).str() + 'é😀'
	value := Any(text)
	for json5 in [false, true] {
		input := encode(text, json5: json5)
		first := decode_any[string](value, json5: json5)!
		unsafe { first.free() }
		baseline := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			decoded := decode[string](input, json5: json5)!
			unsafe { decoded.free() }
		}
		decoder_allocations := C.json5_heap_live_bytes() - baseline
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			decoded := decode_any[string](value, json5: json5)!
			assert decoded == text
			unsafe { decoded.free() }
		}
		assert C.json5_heap_live_bytes() == before + decoder_allocations
	}
}

@[manualfree]
fn test_decode_file_releases_file_contents_for_numeric_results() {
	path := os.join_path(os.temp_dir(), 'json2_file_allocations_${os.getpid()}.json')
	defer { os.rm(path) or {} }
	input := ' '.repeat(8192) + '123'
	os.write_file(path, input)!
	for json5 in [false, true] {
		assert decode_file[int](path, json5: json5)! == 123
		baseline := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			assert decode[int](input, json5: json5)! == 123
		}
		decoder_allocations := C.json5_heap_live_bytes() - baseline
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			assert decode_file[int](path, json5: json5)! == 123
		}
		assert C.json5_heap_live_bytes() == before + decoder_allocations
	}
}

@[manualfree]
fn test_doc_decode_and_reflect_release_serialization_scratch() {
	for json5 in [false, true] {
		doc := new_doc(Any(123), json5: json5)
		assert doc.decode[int]()! == 123
		assert doc.reflect[int]() == 123
		baseline := C.json5_heap_live_bytes()
		for _ in 0 .. 200 {
			assert decode[int]('123', json5: json5)! == 123
		}
		decoder_allocations := C.json5_heap_live_bytes() - baseline
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 100 {
			assert doc.decode[int]()! == 123
			assert doc.reflect[int]() == 123
		}
		assert C.json5_heap_live_bytes() == before + decoder_allocations
	}
}

@[manualfree]
fn test_doc_decode_releases_scratch_for_large_ignored_fields() {
	value := Any({
		'port':    Any(123)
		'ignored': Any('x'.repeat(8192))
	})
	for json5 in [false, true] {
		doc := new_doc(value, json5: json5)
		input := encode(value, json5: json5)
		assert doc.decode[Json5PortOnly]()!.port == 123
		baseline := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			assert decode[Json5PortOnly](input, json5: json5)!.port == 123
		}
		decoder_allocations := C.json5_heap_live_bytes() - baseline
		before := C.json5_heap_live_bytes()
		for _ in 0 .. 10 {
			assert doc.decode[Json5PortOnly]()!.port == 123
		}
		assert C.json5_heap_live_bytes() == before + decoder_allocations
	}
}
