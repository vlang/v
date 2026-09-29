module responses

import json2

pub struct Writer {}

// accept accepts a generic value for the selective-import specialization test.
pub fn accept[T](value T) {
	_ = value
}

struct Response[T] {
	result T
}

// write encodes a payload after passing it through a generic response.
pub fn (mut w Writer) write[T](payload T) string {
	response := Response[T]{ result: payload }
	return encode_response[T](response)
}

fn encode_response[T](response Response[T]) string {
	return json2.encode(response.result)
}
