// vtest vflags: -w
import json

struct JsonMutReceiver {
	value string
}

fn (mut receiver JsonMutReceiver) encode() string {
	return json.encode(receiver)
}

fn test_json_encode_mut_receiver_uses_value_type() {
	mut receiver := JsonMutReceiver{
		value: 'encoded'
	}
	assert receiver.encode() == '{"value":"encoded"}'
}
