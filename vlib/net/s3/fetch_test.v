// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

fn test_parse_s3_url_full() {
	bucket, key := parse_s3_url('s3://my-bucket/path/to/key.txt') or { panic(err) }
	assert bucket == 'my-bucket'
	assert key == 'path/to/key.txt'
}

fn test_parse_s3_url_only_key() {
	bucket, key := parse_s3_url('s3://standalone-key') or { panic(err) }
	assert bucket == ''
	assert key == 'standalone-key'
}

fn test_parse_s3_url_rejects_other_schemes() {
	if _, _ := parse_s3_url('https://example.com/x') {
		assert false, 'should have errored'
	}
	if _, _ := parse_s3_url('http://x') {
		assert false, 'should have errored'
	}
	if _, _ := parse_s3_url('') {
		assert false, 'should have errored'
	}
}

fn test_parse_s3_url_rejects_empty_key() {
	if _, _ := parse_s3_url('s3://my-bucket/') {
		assert false, 'should have errored'
	}
	if _, _ := parse_s3_url('s3://') {
		assert false, 'should have errored'
	}
}

fn test_redact_url_strips_query() {
	assert redact_url('https://x.y/path?X-Amz-Signature=abc') == 'https://x.y/path?<redacted>'
	assert redact_url('https://x.y/path') == 'https://x.y/path'
}

fn test_fetch_rejects_non_s3_scheme() {
	if _ := fetch('https://example.com/x') {
		assert false
	}
	if _ := fetch('http://example.com/x') {
		assert false
	}
	if _ := fetch('file:///etc/passwd') {
		assert false
	}
}

fn test_fetch_rejects_post_method() {
	// POST is not part of the s3.fetch contract: object writes use PUT,
	// and POST in S3 is reserved for multipart-control endpoints which
	// take an XML body that fetch does not generate. Reject explicitly
	// rather than silently rewriting to PUT.
	if _ := fetch('s3://my-bucket/key.txt', method: .post, body: 'x'.bytes()) {
		assert false, 'POST should be rejected'
	}
}

fn test_build_object_path_preserves_leading_and_trailing_slashes() {
	creds := Credentials{
		access_key_id:     'K'
		secret_access_key: 'S'
		bucket:            'b'
	}
	// S3 keys are byte-exact: `folder/`, `/x` and `a//b` must round-trip
	// untouched (only percent-encoded). Stripping slashes would address a
	// different object than requested.
	assert build_object_path(creds, '', 'folder/')! == '/b/folder/'
	assert build_object_path(creds, '', '/x')! == '/b//x'
	assert build_object_path(creds, '', 'a//b')! == '/b/a//b'
	assert build_object_path(creds, '', 'plain.txt')! == '/b/plain.txt'
}
