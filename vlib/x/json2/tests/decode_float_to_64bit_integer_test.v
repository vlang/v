import json as old_json
import x.json2 as json

struct Legacy64BitNumbers {
	signed   i64
	unsigned u64
}

fn test_decode_float_numbers_into_64bit_integer_fields_matches_json_module() {
	payload := '{"signed": -1756811041916.0, "unsigned": 1756811041916.0}'
	expected := old_json.decode(Legacy64BitNumbers, payload)!
	actual := json.decode[Legacy64BitNumbers](payload)!
	assert actual == expected
}

fn test_decode_float_strings_into_64bit_integer_fields_in_default_mode() {
	payload := '{"signed": "-1756811041916.0", "unsigned": "1756811041916.0"}'
	actual := json.decode[Legacy64BitNumbers](payload)!
	assert actual.signed == i64(-1756811041916)
	assert actual.unsigned == u64(1756811041916)
}
