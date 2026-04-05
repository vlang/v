import json

struct F64ArrayPayload {
	arr []f64
}

fn test_encode_decode_struct_with_f64_array_roundtrips() ! {
	original := F64ArrayPayload{
		arr: [0.9716157205240175, 0.9336099585062241]
	}
	encoded := json.encode(original)
	assert encoded == '{"arr":[0.9716157205240175,0.9336099585062241]}'
	assert json.decode(F64ArrayPayload, encoded)! == original
}
