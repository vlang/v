module redis

import net
import time

fn protocol_db_with_response(response string, version int) !DB {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	address := listener.addr()!
	mut client := net.dial_tcp(address.str())!
	client.set_read_timeout(time.second)
	mut server := listener.accept()!
	defer { server.close() or {} }
	server.write_string(response)!
	return DB{
		version: version
		conn:    client
	}
}

fn test_resp3_null_does_not_consume_next_response() {
	mut db := protocol_db_with_response('_\r\n+PONG\r\n', 3)!
	defer { db.close() or {} }
	assert db.read_response()! is RedisNull
	assert db.read_response()! as string == 'PONG'
}

fn test_resp3_array_preserves_null_and_empty_bulk_string() {
	mut db := protocol_db_with_response('*3\r\n$5\r\nfirst\r\n_\r\n$0\r\n\r\n+OK\r\n',
		3)!
	defer { db.close() or {} }
	values := db.read_response()! as []RedisValue
	assert values.len == 3
	assert values[0] as []u8 == 'first'.bytes()
	assert values[1] is RedisNull
	assert values[2] as []u8 == []u8{}
	assert db.read_response()! as string == 'OK'
}

fn test_resp3_null_requires_crlf_terminator() {
	for frame in ['_x\r\n', '_\r'] {
		mut db := protocol_db_with_response(frame, 3)!
		defer { db.close() or {} }
		db.read_response() or {
			continue
		}
		assert false, 'malformed null frame was accepted'
	}
}

fn test_resp2_null_bulk_string_and_resp3_null_rejection() {
	mut db := protocol_db_with_response('$-1\r\n', 2)!
	defer { db.close() or {} }
	assert db.read_response()! is RedisNull
	mut resp2_db := protocol_db_with_response('_\r\n', 2)!
	defer { resp2_db.close() or {} }
	resp2_db.read_response() or {
		assert err.msg().contains('unknown response prefix')
		return
	}
	assert false, 'RESP3 null frame was accepted in RESP2 mode'
}
