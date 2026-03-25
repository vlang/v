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
