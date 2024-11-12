import json

fn test_main() {
	json_text := '{ "a": "b" }'
	b := json.decode(struct {
		a string
	}, json_text)!.a
	assert dump(b) == 'b'
}
