module httptest

import net.http

// ResponseRecorder records an HTTP response for testing.
pub struct ResponseRecorder {
pub mut:
	code   int
	body   string
	header http.Header
}

pub fn new_recorder() ResponseRecorder {
	return ResponseRecorder{ code: 200 }
}

// record writes resp into recorder.
pub fn (mut r ResponseRecorder) record(resp http.Response) {
	r.code = resp.status_code
	r.body = resp.body
	r.header = resp.header
}
