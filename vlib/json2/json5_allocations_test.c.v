// vtest vflags: -gc none -d track_heap -nocache
module json2

#include "@VMODROOT/vlib/json2/testdata/json5_heap_tracking.h"

fn C.json5_heap_live_bytes() u64

struct Json5PortOnly {
	port int
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
fn test_json5_reuse_releases_normalized_text_of_ignored_fields() {
	input := "{port:123,ignored:'" + 'x'.repeat(8192) + "'}"
	mut buffer := DecodeBuffer{}
	assert decode_reuse[Json5PortOnly](input, mut buffer, json5: true)!.port == 123
	before := C.json5_heap_live_bytes()
	for _ in 0 .. 100 {
		assert decode_reuse[Json5PortOnly](input, mut buffer, json5: true)!.port == 123
	}
	assert C.json5_heap_live_bytes() == before
	unsafe { buffer.values_info.free() }
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
