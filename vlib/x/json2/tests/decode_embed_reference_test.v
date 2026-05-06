import json as stdjson
import x.json2

struct Issue24950GateRecord {
	exit string
}

struct Issue24950RecordBase {
	gate &Issue24950GateRecord = unsafe { nil } @[omitempty]
}

struct Issue24950Record {
	Issue24950RecordBase
}

struct Issue24950Wrapper {
	gate &Issue24950GateRecord = unsafe { nil }
}

struct Issue25614Node {
mut:
	value    int
	children []&Issue25614Node
}

struct OptionalPointerRegressionMessage {
mut:
	id       int
	text     string
	reply_to ?&OptionalPointerRegressionMessage
}

fn test_decode_struct_with_reference_field() {
	decoded := json2.decode[Issue24950Wrapper]('{"gate":{"exit":"north"}}')!

	assert decoded.gate != unsafe { nil }
	assert decoded.gate.exit == 'north'
}

fn test_decode_embedded_struct_with_reference_field() {
	empty_std := stdjson.decode(Issue24950Record, '{}')!
	empty_json2 := json2.decode[Issue24950Record]('{}')!

	assert empty_std.Issue24950RecordBase.gate == unsafe { nil }
	assert empty_json2.Issue24950RecordBase.gate == unsafe { nil }

	populated_std := stdjson.decode(Issue24950Record, '{"gate":{"exit":"north"}}')!
	populated_json2 := json2.decode[Issue24950Record]('{"gate":{"exit":"north"}}')!

	assert populated_std.Issue24950RecordBase.gate != unsafe { nil }
	assert populated_json2.Issue24950RecordBase.gate != unsafe { nil }
	assert populated_std.Issue24950RecordBase.gate.exit == 'north'
	assert populated_json2.Issue24950RecordBase.gate.exit == 'north'
}

fn test_decode_top_level_reference_to_primitive() {
	decoded := json2.decode[&int]('1')!

	assert decoded != unsafe { nil }
	assert *decoded == 1
}

fn test_decode_top_level_reference_to_primitive_null() {
	decoded := json2.decode[&int]('null')!

	assert decoded == unsafe { nil }
}

fn test_decode_top_level_reference_to_struct() {
	decoded := json2.decode[&Issue25614Node]('{"value":1}')!

	assert decoded != unsafe { nil }
	assert decoded.value == 1
}

fn test_decode_struct_with_array_of_references() {
	decoded := json2.decode[Issue25614Node]('{"children":[{"value":1},{"value":2}]}')!

	assert decoded.children.len == 2
	assert decoded.children[0] != unsafe { nil }
	assert decoded.children[1] != unsafe { nil }
	assert decoded.children[0].value == 1
	assert decoded.children[1].value == 2
}

fn test_decode_optional_reference_field() {
	message :=
		json2.decode[OptionalPointerRegressionMessage]('{"id":1,"text":"Hello","reply_to":{"id":2,"text":"Hi","reply_to":null}}')!

	reply := message.reply_to or {
		assert false
		return
	}

	assert reply.id == 2
	assert reply.text == 'Hi'
	assert reply.reply_to == none
}
