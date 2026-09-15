module main

import json

struct JsonDecodeOrPayload {
	value int
}

struct JsonDecodeOrSecondPayload {
	name string
}

fn json_decode_or_payload(source string) !int {
	payload := json.decode(JsonDecodeOrPayload, source) or { return error('decode failed') }
	return payload.value
}

fn json_decode_or_second_payload(source string) !string {
	payload := json.decode(JsonDecodeOrSecondPayload, source) or {
		return error('decode failed')
	}
	return payload.name
}

fn test_json_decode_or_payload_keeps_concrete_type() {
	assert json_decode_or_payload('{"value":42}')! == 42
	assert json_decode_or_second_payload('{"name":"ok"}')! == 'ok'
}
