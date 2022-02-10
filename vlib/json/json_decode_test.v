import json

struct TestTwin {
	id     int
	seed   string
	pubkey string
}

struct TestTwins {
mut:
	twins []TestTwin [required]
}

fn test_json_decode_fails_to_decode_unrecognised_array_of_dicts() {
	data := '[{"twins":[{"id":123,"seed":"abcde","pubkey":"xyzasd"},{"id":456,"seed":"dfgdfgdfgd","pubkey":"skjldskljh45sdf"}]}]'
	json.decode(TestTwins, data) or {
		assert err.msg() == "expected field 'twins' is missing"
		return
	}
	assert false
}

fn test_json_decode_works_with_a_dict_of_arrays() {
	data := '{"twins":[{"id":123,"seed":"abcde","pubkey":"xyzasd"},{"id":456,"seed":"dfgdfgdfgd","pubkey":"skjldskljh45sdf"}]}'
	res := json.decode(TestTwins, data) or {
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

fn test_decode_u64() ? {
	data := '{"size": 10737418240}'
	m := json.decode(Mount, data) ?
	assert m.size == 10737418240
	// println(m)
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
	file_name      string    [skip]
	comments       []Comment [skip]
}

fn test_skip_fields_should_be_initialised_by_json_decode() ? {
	data := '{"total_comments": 55, "id": 123}'
	mut task := json.decode(Task, data) ?
	assert task.id == 123
	assert task.total_comments == 55
	assert task.comments == []
}
