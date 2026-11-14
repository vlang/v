module httptest

import net.http

fn test_recorder() {
	mut rec := new_recorder()
	resp := http.Response{ status_code: 201, body: 'hi' }
	rec.record(resp)
	assert rec.code == 201
	assert rec.body == 'hi'
}
