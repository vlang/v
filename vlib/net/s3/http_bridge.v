// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import net.http

// init registers an `s3://` scheme handler with `net.http` so that
// `http.fetch(url: 's3://...')` and friends route here transparently. Only
// active when this module is imported.
fn init() {
	http.register_scheme('s3', http_fetch_handler)
}

// http_fetch_handler bridges `http.fetch` into `s3.fetch`. Credentials are
// pulled from the environment (`S3_*`, `AWS_*`, `CELLAR_ADDON_*`, …); for
// explicit credentials, callers should use `s3.fetch` directly.
fn http_fetch_handler(config http.FetchConfig) !http.Response {
	resp := fetch(config.url,
		method:       config.method
		body:         config.data.bytes()
		credentials:  Credentials.from_env()
		content_type: config.header.get(.content_type) or { '' }
	)!
	mut header := http.new_header()
	for k, v in resp.headers {
		header.add_custom(k, v) or {}
	}
	if resp.content_type != '' {
		header.set(.content_type, resp.content_type)
	}
	if resp.etag != '' {
		header.set(.etag, resp.etag)
	}
	return http.Response{
		status_code: resp.status_code
		body:        resp.body.bytestr()
		header:      header
	}
}

fn http_method_to_string(m http.Method) string {
	return match m {
		.get { 'GET' }
		.put { 'PUT' }
		.post { 'POST' }
		.delete { 'DELETE' }
		.head { 'HEAD' }
		else { 'GET' }
	}
}
