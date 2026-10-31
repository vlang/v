module json2

struct KeyPlain {
	name     string @[json: 'n']
	count    int
	unused   string @[skip]
	optional ?string
	blank    string @[omitempty]
}

struct KeyNested {
	item  KeyPlain
	items []KeyPlain
}

struct KeyBase {
	name string
	x    int
}

struct KeyMiddle {
	KeyBase
	name string
}

struct KeyOuter {
	KeyMiddle
	x    int
	name string
}

type KeyVariant = KeyPlain | KeyOuter

struct KeyDuplicate {
	a int @[json: 'same']
	b int @[json: 'same']
}

fn test_key_tracking_plain_nested_and_renamed_fields() {
	value := KeyPlain{ name: 'hello', count: 42, unused: 'hidden' }
	assert encode(value) == '{"n":"hello","count":42}'
	assert encode(KeyNested{ item: value, items: [value] }) == '{"item":{"n":"hello","count":42},"items":[{"n":"hello","count":42}]}'
	assert encode(value, prettify: true) == '{\n    "n": "hello",\n    "count": 42\n}'
	assert encode(value, prettify: true, legacy_layout: true) == '{\n\t"n":\t"hello",\n\t"count":\t42\n}'
	assert encode(KeyDuplicate{ a: 1, b: 2 }) == '{"same":1,"same":2}'
}

fn test_key_tracking_embedded_collision_and_sum_variants() {
	value := KeyOuter{
		KeyMiddle: KeyMiddle{ KeyBase: KeyBase{ name: 'base', x: 1 }, name: 'middle' }
		x:         2
		name:      'outer'
	}
	expected := '{"KeyMiddle.KeyBase.name":"base","KeyMiddle.KeyBase.x":1,"KeyMiddle.name":"middle","x":2,"name":"outer"}'
	assert encode(value) == expected
	assert decode[KeyOuter](expected)! == value
	variant := KeyVariant(value)
	assert encode(variant) == expected[..expected.len - 1] + ',"_type":"KeyOuter"}'
	plain := KeyVariant(KeyPlain{ name: 'v', count: 3 })
	assert encode(plain) == '{"n":"v","count":3,"_type":"KeyPlain"}'
}
