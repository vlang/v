import x.json2 as json

struct Baz {
	c bool = true
}

struct Foo {
	Baz
	a int = 1
	b int = 2
}

struct Bar {
	Foo
	a string = '1'
	c string = '3'
}

struct Environment {
	vflags string
}

struct HistoryEvent {
	Environment
}

struct History {
pub mut:
	history_events []HistoryEvent
}

struct Classifier {
	History
}

fn test_embed() {
	assert json.encode(Bar{}) == '{"a":"1","c":"3","Foo.a":1,"b":2,"Foo.Baz.c":true}'
	assert json.encode(Bar{}, prettify: true) == '{
    "a": "1",
    "c": "3",
    "Foo.a": 1,
    "b": 2,
    "Foo.Baz.c": true
}'
}

fn test_decode_embed_with_prefixed_keys() {
	decoded := json.decode[Bar]('{"a":"1","c":"3","Foo.a":4,"b":2,"Foo.Baz.c":false}')!
	assert decoded.a == '1'
	assert decoded.c == '3'
	assert decoded.Foo.a == 4
	assert decoded.Foo.b == 2
	assert decoded.Foo.Baz.c == false
}

fn test_decode_embed_roundtrip_with_flattened_keys() {
	mut classifier := Classifier{}
	classifier.history_events = [
		HistoryEvent{
			Environment: Environment{
				vflags: 'one'
			}
		},
		HistoryEvent{
			Environment: Environment{
				vflags: 'two'
			}
		},
	]
	encoded := json.encode(classifier)
	assert encoded == '{"history_events":[{"vflags":"one"},{"vflags":"two"}]}'
	decoded := json.decode[Classifier](encoded)!
	assert decoded.history_events.len == 2
	assert decoded.history_events[0].Environment.vflags == 'one'
	assert decoded.history_events[1].Environment.vflags == 'two'
}

fn test_decode_embed_with_nested_objects() {
	decoded := json.decode[Classifier]('{"History":{"history_events":[{"Environment":{"vflags":"legacy"}}]}}')!
	assert decoded.history_events.len == 1
	assert decoded.history_events[0].Environment.vflags == 'legacy'
}
