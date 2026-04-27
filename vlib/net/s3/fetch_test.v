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
