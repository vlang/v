// vtest vflags: -w
import json2

fn test_main() {
	json_text := '{ "a": "b" }'
	b := json2.decode[struct {
		a string
	}](json_text)!.a
	assert dump(b) == 'b'
}
