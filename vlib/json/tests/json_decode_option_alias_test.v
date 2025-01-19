import json

struct Empty {}

struct SomeStruct {
	random_field_a ?string
	random_field_b ?string
	empty_field    ?Empty
}

type Alias = SomeStruct

fn test_main() {
	data := json.decode(Alias, '{"empty_field":{}}')!
	assert data.str() == 'Alias(SomeStruct{
    random_field_a: Option(none)
    random_field_b: Option(none)
    empty_field: Option(none)
})'
}
