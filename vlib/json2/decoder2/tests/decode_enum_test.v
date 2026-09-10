import json2
import json2.decoder2 as json

enum DecoderEnum {
	a
	b  @[json: 'bee']
	c = 10
}

type DecoderEnumAlias = DecoderEnum

struct EnumHolder {
	value DecoderEnum
	name  string
}

fn test_decode_enum_strings_and_aliases() {
	assert json.decode[DecoderEnum]('"a"')! == .a
	assert json.decode[DecoderEnum]('"b"')! == .b
	assert json.decode[DecoderEnum]('"bee"')! == .b
	assert json.decode[DecoderEnum]('"c"')! == .c
	assert json.decode[DecoderEnumAlias]('"bee"')! == DecoderEnumAlias(DecoderEnum.b)

	encoded := json2.encode(DecoderEnum.b)
	assert encoded == '"bee"'
	assert json.decode[DecoderEnum](encoded)! == .b
}

fn test_decode_enum_numbers() {
	assert json.decode[DecoderEnum]('0')! == .a
	assert json.decode[DecoderEnum]('1')! == .b
	assert json.decode[DecoderEnum]('10')! == .c
}

fn test_decode_enum_advances_to_next_struct_field() {
	assert json.decode[EnumHolder]('{"value":"bee","name":"ok"}')! == EnumHolder{
		value: .b
		name:  'ok'
	}
}

fn test_decode_enum_rejects_unknown_values() {
	mut failed := false
	json.decode[DecoderEnum]('"unknown"') or { failed = true }
	assert failed

	failed = false
	json.decode[DecoderEnum]('2') or { failed = true }
	assert failed
}
