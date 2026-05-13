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

struct RoundtripInner {
	inner []f64
}

struct RoundtripOuter {
	RoundtripInner
	test f64
}

struct Article {
	title       string
	description string
}

struct ArticlePlus {
	Article
	perex string
}

fn test_embed() {
	assert json.encode(Bar{}) == '{"Foo.Baz.c":true,"Foo.a":1,"b":2,"a":"1","c":"3"}'
	assert json.encode(Bar{}, prettify: true) == '{
    "Foo.Baz.c": true,
    "Foo.a": 1,
    "b": 2,
    "a": "1",
    "c": "3"
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
	decoded :=
		json.decode[Classifier]('{"History":{"history_events":[{"Environment":{"vflags":"legacy"}}]}}')!
	assert decoded.history_events.len == 1
	assert decoded.history_events[0].Environment.vflags == 'legacy'
}

fn test_embed_roundtrip_preserves_declaration_order() {
	str := '{"inner":[1,2,3,4,5],"test":1.2}'
	data := json.decode[RoundtripOuter](str)!
	assert json.encode(data) == str
}

fn test_encode_array_of_embedded_structs_preserves_declaration_order() {
	list_of_object := [
		ArticlePlus{
			title:       'One good title'
			description: 'this is the first'
		},
		ArticlePlus{
			title:       'Other good title'
			description: 'more one'
		},
		ArticlePlus{
			title:       'Other good title'
			description: 'more one'
			perex:       'good perex'
		},
	]
	assert json.encode(list_of_object) == '[{"title":"One good title","description":"this is the first","perex":""},{"title":"Other good title","description":"more one","perex":""},{"title":"Other good title","description":"more one","perex":"good perex"}]'
}
