import json

struct SomeParams {
	name       string
	sub_struct struct {
		id string
	} @[omitempty]
}

fn some_fn(p SomeParams) string {
	return json.encode(p)
}

fn test_main() {
	some_fn(SomeParams{})
}
