module redis

fn test_command_encoding_preserves_binary_arguments() {
	mut db := DB{}
	db.pipeline_start()
	payload := [u8(0), `\r`, `\n`, 0xff]
	assert db.set('key\r\n', payload)! == ''
	mut expected := '*3\r\n$3\r\nSET\r\n$5\r\nkey\r\n\r\n$4\r\n'.bytes()
	expected << payload
	expected << '\r\n'.bytes()
	assert db.pipeline_buffer == expected
	assert db.pipeline_cmd_count == 1
	assert db.get[[]u8]('key\r\n')! == []u8{}
	assert db.pipeline_cmd_count == 2
	assert db.pipeline_buffer[expected.len..] == '*2\r\n$3\r\nGET\r\n$5\r\nkey\r\n\r\n'.bytes()
}

fn test_existing_wrappers_queue_without_decoding_null_placeholder() {
	mut db := DB{}
	db.pipeline_start()
	assert db.del('key')! == 0
	assert db.incr('key')! == 0
	assert db.decr('key')! == 0
	assert db.hset('hash', {
		'field': 42
	})! == 0
	assert db.hget[int]('hash', 'field')! == 0
	assert db.hgetall[string]('hash')! == map[string]string{}
	assert !db.expire('key', 60)!
	assert db.pipeline_cmd_count == 7
}

fn test_unsupported_value_does_not_add_a_pipeline_command() {
	mut db := DB{}
	db.pipeline_start()
	db.set('key', true) or {
		assert err.msg().len > 0
		assert db.pipeline_cmd_count == 0
		assert db.pipeline_buffer.len == 0
		return
	}
	assert false, 'unsupported value was accepted'
}

fn test_unsupported_return_type_does_not_add_a_pipeline_command() {
	mut db := DB{}
	db.pipeline_start()
	db.get[bool]('key') or {
		assert err.msg().len > 0
		assert db.pipeline_cmd_count == 0
		assert db.pipeline_buffer.len == 0
		return
	}
	assert false, 'unsupported return type was accepted'
}

fn test_empty_hash_with_unsupported_value_type_does_not_queue() {
	mut db := DB{}
	db.pipeline_start()
	db.hset('hash', map[string]bool{}) or {
		assert err.msg().len > 0
		assert db.pipeline_cmd_count == 0
		assert db.pipeline_buffer.len == 0
		return
	}
	assert false, 'unsupported hash value type was accepted'
}
