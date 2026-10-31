// vtest vflags: -w
import json2

pub struct Dto {
pub:
	key      string  @[raw]
	key2     string  @[raw]
	data     ?string @[raw]
	optional ?string @[raw]
}

fn test_main() {
	raw_json := '{
        "key": [1, 2, "test"],
        "key2": { "test": 1 },
        "data": { "test": 1 },
        "optional": "test"
    }'

	dto := json2.decode[Dto](raw_json)!

	println(dto)
	// json2 keeps the raw JSON text as written.
	assert dto.data? == '{ "test": 1 }'
}

fn test_none() {
	raw_json := '{
        "key": [1, 2, "test"],
        "optional": "test"
    }'

	dto := json2.decode[Dto](raw_json)!

	println(dto)
	assert dto.data == none
	assert dto.optional? == '"test"'
}

fn test_null() {
	raw_json := '{
        "key": [1, 2, "test"],
       	"key2": null,
        "data": null,
        "optional": "test"
    }'

	dto := json2.decode[Dto](raw_json)!

	println(dto)
	assert dto.key2 == 'null'
	assert dto.data? == 'null'
}

fn test_not_set() {
	raw_json := '{
    }'

	dto := json2.decode[Dto](raw_json)!

	println(dto)
	assert dto.data == none
	assert dto.optional == none
}
