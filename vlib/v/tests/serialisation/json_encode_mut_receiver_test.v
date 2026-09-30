// vtest vflags: -w
import json2

struct JsonMutReceiver {
	value string
}

fn (mut receiver JsonMutReceiver) encode() string {
	return json2.encode(receiver, escape_unicode: true)
}

fn test_json_encode_mut_receiver_uses_value_type() {
	mut receiver := JsonMutReceiver{
		value: 'encoded'
	}
	assert receiver.encode() == '{"value":"encoded"}'
}
