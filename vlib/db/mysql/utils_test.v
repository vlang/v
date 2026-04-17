module mysql

import json

struct MysqlErrorPayload {
	data string
}

fn test_clone_mysql_cstring_keeps_json_encodable_error_text() {
	mut backing :=
		"Incorrect string value: 'undefined' for function uuid_to_bin; code: 1411".bytes()
	backing << u8(0)
	msg := clone_mysql_cstring(backing.data)

	backing[0] = 0xff

	assert msg == "Incorrect string value: 'undefined' for function uuid_to_bin; code: 1411"
	assert json.encode(MysqlErrorPayload{
		data: msg
	}) == '{"data":"Incorrect string value: \'undefined\' for function uuid_to_bin; code: 1411"}'
}
