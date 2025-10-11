import x.json2 as json

struct Mount {
	size u64
}

fn test_decode_u64() {
	data := '{"size": 10737418240}'
	m := json.decode[Mount](data)!
	assert m.size == 10737418240
}

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
}

fn test_skip_fields_should_be_initialised_by_json_decode() {
	data := '{"total_comments": 55, "id": 123}'
	mut task := json.decode[Task](data)!
	assert task.id == 123
	assert task.total_comments == 55
	assert task.comments == []
}

//

struct DbConfig {
	host   string
	dbname string
	user   string
}

fn test_decode_error_message_should_have_enough_context_empty() {
	json.decode[DbConfig]('') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'empty string'
		}
		return
	}
	assert false
}

fn test_decode_error_message_should_have_enough_context_just_brace() {
	json.decode[DbConfig]('{') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Syntax: EOF: expected object end'
		}
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

	json.decode[DbConfig](txt) or {
		if err is json.JsonDecodeError {
			assert err.line == 5
			assert err.character == 1
			assert err.message == 'Syntax: Cannot use `,`, before `}`'
		}

		return
	}
	assert false
}

fn test_decode_error_message_should_have_enough_context_in_the_middle() {
	txt := '{"host": "localhost", "dbname": "alex" "user": "alex", "port": "1234"}'
	json.decode[DbConfig](txt) or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 40
			assert err.message == 'Syntax: invalid value. Unexpected character after string end'
		}
		return
	}
	assert false
}
