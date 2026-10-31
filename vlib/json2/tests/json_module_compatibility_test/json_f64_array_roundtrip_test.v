// vtest vflags: -w
import json2

struct F64ArrayPayload {
	arr []f64
}

fn test_encode_decode_struct_with_f64_array_roundtrips() ! {
	original := F64ArrayPayload{
		arr: [0.9716157205240175, 0.9336099585062241]
	}
	encoded := json2.encode(original, escape_unicode: true)
	assert encoded == '{"arr":[0.9716157205240175,0.9336099585062241]}'
	assert json2.decode[F64ArrayPayload](encoded)! == original
}
