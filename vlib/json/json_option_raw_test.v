import json

pub struct Dto {
pub:
	key      string  [raw]
	key2     string  [raw]
	data     ?string [raw]
	optional ?string [raw]
}

fn test_main() {
	raw_json := '{
        "key": [1, 2, "test"],
        "key2": { "test": 1 },
        "data": { "test": 1 },
        "optional": "test"
    }'

	dto := json.decode(Dto, raw_json)!

	println(dto)
}

fn test_none() {
	raw_json := '{
        "key": [1, 2, "test"],
        "optional": "test"
    }'

	dto := json.decode(Dto, raw_json)!

	println(dto)
}

fn test_null() {
	raw_json := '{
        "key": [1, 2, "test"],
       	"key2": null,
        "data": null,
        "optional": "test"
    }'

	dto := json.decode(Dto, raw_json)!

	println(dto)
}

fn test_not_set() {
	raw_json := '{
    }'

	dto := json.decode(Dto, raw_json)!

	println(dto)
}
