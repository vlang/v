// main.v
module main

import json

fn test_main() {
	input := '{"methode":"test"}'
	decode_message(input) or {}
	assert true
}

type BaseMessage = struct {
	methode string @[json: methode]
}

pub fn decode_message(msg string) !string {
	decoded_json := json.decode(BaseMessage, msg.str())!
	return decoded_json.str()
}
