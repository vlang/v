// vtest vflags: -w
import json2

struct SomeParams {
	name       string
	sub_struct struct {
		id string
	} @[omitempty]
}

fn some_fn(p SomeParams) string {
	return json2.encode(p, escape_unicode: true)
}

fn test_main() {
	some_fn(SomeParams{})
}
