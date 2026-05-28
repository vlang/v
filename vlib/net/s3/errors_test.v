// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

fn test_extract_xml_tag_basic() {
	body := '<?xml version="1.0"?><Error><Code>NoSuchKey</Code><Message>Key not found</Message><RequestId>R1</RequestId></Error>'
	assert extract_xml_tag(body, 'Code') == 'NoSuchKey'
	assert extract_xml_tag(body, 'Message') == 'Key not found'
	assert extract_xml_tag(body, 'RequestId') == 'R1'
	assert extract_xml_tag(body, 'Missing') == ''
}

fn test_extract_xml_tag_decodes_entities() {
	body := '<Message>a &amp; b &lt;tag&gt;</Message>'
	assert extract_xml_tag(body, 'Message') == 'a & b <tag>'
}

fn test_decode_xml_entities() {
	assert decode_xml_entities('&quot;hello&quot;') == '"hello"'
	assert decode_xml_entities('a &amp; b &lt; c &gt; d &apos;e&apos;') == "a & b < c > d 'e'"
	assert decode_xml_entities('no entities here') == 'no entities here'
}

fn test_s3error_msg_contains_code_and_path() {
	e := S3Error{
		code:    'NoSuchKey'
		message: 'object missing'
		status:  404
		path:    'foo/bar.txt'
	}
	rendered := e.msg()
	assert rendered.contains('NoSuchKey')
	assert rendered.contains('object missing')
	assert rendered.contains('404')
	assert rendered.contains('foo/bar.txt')
}

fn test_s3error_code_maps_known_strings() {
	assert (&S3Error{
		code: 'NoSuchKey'
	}).code() == 404
	assert (&S3Error{
		code: 'NoSuchBucket'
	}).code() == 404
	assert (&S3Error{
		code: 'AccessDenied'
	}).code() == 403
	assert (&S3Error{
		code: 'BucketAlreadyExists'
	}).code() == 409
	// Unknown code falls back to the wire status.
	assert (&S3Error{
		code:   'WeirdCode'
		status: 418
	}).code() == 418
}

fn test_new_http_error_parses_xml_body() {
	body := '<?xml version="1.0"?><Error><Code>NoSuchKey</Code><Message>The specified key does not exist.</Message><Resource>/bucket/missing.txt</Resource><RequestId>ABC123</RequestId></Error>'
	err := new_http_error(404, 'missing.txt', body)
	if err is S3Error {
		assert err.code == 'NoSuchKey'
		assert err.message == 'The specified key does not exist.'
		assert err.resource == '/bucket/missing.txt'
		assert err.request_id == 'ABC123'
		assert err.status == 404
		assert err.path == 'missing.txt'
	} else {
		assert false, 'expected S3Error'
	}
}

fn test_new_http_error_falls_back_when_body_empty() {
	err := new_http_error(503, 'foo', '')
	if err is S3Error {
		assert err.code == 'ServiceUnavailable'
		assert err.status == 503
	} else {
		assert false, 'expected S3Error'
	}
}
