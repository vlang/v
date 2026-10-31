// vtest vflags: -w
import json2

struct TestTwin {
	id     int
	seed   string
	pubkey string
}

struct TestTwins {
mut:
	twins []TestTwin @[required]
}

fn test_json_decode_fails_to_decode_unrecognised_array_of_dicts() {
	data := '[{"twins":[{"id":123,"seed":"abcde","pubkey":"xyzasd"},{"id":456,"seed":"dfgdfgdfgd","pubkey":"skjldskljh45sdf"}]}]'
	json2.decode[TestTwins](data) or {
		assert err.msg().contains('Expected object, but got array')
		return
	}
	assert false
}

fn test_json_decode_works_with_a_dict_of_arrays() {
	data := '{"twins":[{"id":123,"seed":"abcde","pubkey":"xyzasd"},{"id":456,"seed":"dfgdfgdfgd","pubkey":"skjldskljh45sdf"}]}'
	res := json2.decode[TestTwins](data) or {
		assert false
		exit(1)
	}
	assert res.twins[0].id == 123
	assert res.twins[0].seed == 'abcde'
	assert res.twins[0].pubkey == 'xyzasd'
	assert res.twins[1].id == 456
	assert res.twins[1].seed == 'dfgdfgdfgd'
	assert res.twins[1].pubkey == 'skjldskljh45sdf'
}

struct Mount {
	size u64
}

fn test_decode_u64() {
	data := '{"size": 10737418240}'
	m := json2.decode[Mount](data)!
	assert m.size == 10737418240
	// println(m)
}

fn test_decode_large_u64_from_decimal_json() {
	cases := [
		u64(9007199254740991),
		u64(9007199254740992),
		u64(9007199254740993),
		u64(9223372036854775807),
		u64(9223372036854775808),
		u64(9223372036854775809),
		u64(18446744073709551614),
		u64(18446744073709551615),
	]
	for want in cases {
		got := json2.decode[[]u64]('[${want}]')!
		assert got.len == 1
		assert got[0] == want
	}
}

fn test_encode_decode_large_u64_roundtrip() {
	cases := [
		u64(9007199254740991),
		u64(9007199254740992),
		u64(9007199254740993),
		u64(9223372036854775807),
		u64(9223372036854775808),
		u64(9223372036854775809),
		u64(18446744073709551614),
		u64(18446744073709551615),
	]
	for want in cases {
		encoded := json2.encode([want], escape_unicode: true)
		assert encoded == '[${want}]'
		got := json2.decode[[]u64](encoded)!
		assert got.len == 1
		assert got[0] == want
	}
}

//

pub struct Comment {
pub mut:
	id      string
	comment string
}

pub struct Task {
mut:
	description    string
	id             int
	total_comments int
	file_name      string    @[skip]
	comments       []Comment @[skip]
	skip_field     string    @[json: '-']
}

fn test_skip_fields_should_be_initialised_by_json_decode() {
	data := '{"total_comments": 55, "id": 123}'
	mut task := json2.decode[Task](data)!
	assert task.id == 123
	assert task.total_comments == 55
	assert task.comments == []
}

fn test_skip_should_be_ignored() {
	data := '{"total_comments": 55, "id": 123, "skip_field": "foo"}'
	mut task := json2.decode[Task](data)!
	assert task.id == 123
	assert task.total_comments == 55
	assert task.comments == []
	assert task.skip_field == ''
}

//

struct DbConfig {
	host   string
	dbname string
	user   string
}

fn test_decode_error_message_should_have_enough_context_empty() {
	json2.decode[DbConfig]('') or {
		assert err.msg().contains('1:1: Invalid json: empty string')
		return
	}
	assert false
}

fn test_decode_error_message_should_have_enough_context_just_brace() {
	json2.decode[DbConfig]('{') or {
		assert err.msg().contains('1:1: Invalid json: Syntax: EOF: expected object end')
		return
	}
	assert false
}

fn test_decode_error_message_should_have_enough_context_trailing_comma_at_end() {
	txt := '{
    "host": "localhost",
    "dbname": "alex",
    "user": "alex",
}'
	json2.decode[DbConfig](txt) or {
		assert err.msg().contains('5:1: Invalid json: Syntax: Cannot use `,`, before `}`')
		return
	}
	assert false
}

fn test_decode_error_message_should_have_enough_context_in_the_middle() {
	txt := '{"host": "localhost", "dbname": "alex" "user": "alex", "port": "1234"}'
	json2.decode[DbConfig](txt) or {
		assert err.msg().contains('1:40: Invalid json: Syntax: invalid value. Unexpected character after string end')
		assert err.msg().contains('{"host": "localhost", "dbname": "alex" ')
		return
	}
	assert false
}
