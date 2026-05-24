// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import time

// Test vectors come from the AWS-published Signature V4 reference test
// suite ("Examples of the complete version 4 signing process").
// These secret/key pairs are the documented example values, not real
// credentials.
const aws_secret = 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'
const aws_key_id = 'AKIAIOSFODNN7EXAMPLE'

// SigV4 vector for `s3-get-object` (examplebucket / test.txt with a Range
// header). Source: AWS docs (Signature V4 examples).
//
//   GET /test.txt HTTP/1.1
//   Host: examplebucket.s3.amazonaws.com
//   Range: bytes=0-9
//   X-Amz-Date: 20130524T000000Z
//   X-Amz-Content-SHA256: <empty-sha>
fn test_aws_sigv4_get_object_vector() {
	t := time.parse_iso8601('2013-05-24T00:00:00.000Z') or { panic(err) }
	creds := Credentials{
		access_key_id:        aws_key_id
		secret_access_key:    aws_secret
		region:               'us-east-1'
		endpoint:             'examplebucket.s3.amazonaws.com'
		virtual_hosted_style: false // host already carries the bucket
	}
	req := SignRequest{
		method:        'GET'
		path:          '/test.txt'
		query:         ''
		payload_hash:  empty_sha256
		extra_headers: {
			'range': 'bytes=0-9'
		}
		sign_time:     t
	}
	signed := sign_request(creds, req) or { panic(err) }
	expected := 'AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-content-sha256;x-amz-date, Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41'
	assert signed.authorization == expected, 'Got:\n${signed.authorization}\nExpected:\n${expected}'
}

// SigV4 vector for `s3-put-object` (examplebucket / test$file.text with body
// "Welcome to Amazon S3.").
//
//   PUT /test%24file.text HTTP/1.1
//   Host: examplebucket.s3.amazonaws.com
//   Date: Fri, 24 May 2013 00:00:00 GMT
//   x-amz-date: 20130524T000000Z
//   x-amz-storage-class: REDUCED_REDUNDANCY
fn test_aws_sigv4_put_object_vector() {
	t := time.parse_iso8601('2013-05-24T00:00:00.000Z') or { panic(err) }
	creds := Credentials{
		access_key_id:        aws_key_id
		secret_access_key:    aws_secret
		region:               'us-east-1'
		endpoint:             'examplebucket.s3.amazonaws.com'
		virtual_hosted_style: false
	}
	body := 'Welcome to Amazon S3.'
	body_hash := sha256_hex(body.bytes())
	req := SignRequest{
		method:        'PUT'
		path:          '/test%24file.text'
		query:         ''
		payload_hash:  body_hash
		extra_headers: {
			'date':                'Fri, 24 May 2013 00:00:00 GMT'
			'x-amz-storage-class': 'REDUCED_REDUNDANCY'
		}
		sign_time:     t
	}
	signed := sign_request(creds, req) or { panic(err) }
	expected := 'AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class, Signature=98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd'
	assert signed.authorization == expected, 'Got:\n${signed.authorization}\nExpected:\n${expected}'
}

// Asserts the canonical request string for a ListObjectsV2 GET. The
// canonical request is the most stable interop point — Authorization line
// formatting has wobbled across AWS doc revisions, but the canonical hash
// is always the same.
fn test_aws_sigv4_list_objects_canonical_request() {
	headers := {
		'host':                 'examplebucket.s3.amazonaws.com'
		'x-amz-content-sha256': empty_sha256
		'x-amz-date':           '20130524T000000Z'
	}
	canonical := build_canonical_request('GET', '/', 'list-type=2&prefix=foo', headers,
		'host;x-amz-content-sha256;x-amz-date', empty_sha256)
	expected := 'GET\n/\nlist-type=2&prefix=foo\nhost:examplebucket.s3.amazonaws.com\nx-amz-content-sha256:${empty_sha256}\nx-amz-date:20130524T000000Z\n\nhost;x-amz-content-sha256;x-amz-date\n${empty_sha256}'
	assert canonical == expected, 'canonical mismatch:\n${canonical}'
}

// Cross-checked manually with openssl on the AWS docs example
// (date 20120215, region us-east-1, service iam):
//
//   echo -n 'aws4_request' | openssl dgst -sha256 -mac HMAC -macopt hexkey:<kService>
fn test_signing_key_chain() {
	key := derive_signing_key(aws_secret, '20120215', 'us-east-1', 'iam')
	assert to_hex_lower(key) == '004aa806e13dae88b9032d9261bcb04c67d023afadd221e6b0d206e1760e0b5e', 'Got: ${to_hex_lower(key)}'
}

fn test_normalize_header_value_collapses_whitespace() {
	assert normalize_header_value('  hello   world  ') == 'hello world'
	assert normalize_header_value('a\tb\tc') == 'a b c'
	assert normalize_header_value('') == ''
}

fn test_canonical_query_string_sorted_and_encoded() {
	q := canonical_query_string({
		'b': '2'
		'a': '1'
		'c': 'hello world'
	})
	assert q == 'a=1&b=2&c=hello%20world'
}

fn test_format_amz_date() {
	t := time.parse_iso8601('2024-01-15T12:34:56.000Z') or { panic(err) }
	assert format_amz_date(t) == '20240115T123456Z'
}

fn test_signer_rejects_unsupported_method() {
	creds := Credentials{
		access_key_id:     'KEY'
		secret_access_key: 'secret'
		region:            'us-east-1'
		endpoint:          'examplebucket.s3.amazonaws.com'
	}
	if _ := sign_request(creds, SignRequest{ method: 'PATCH', path: '/x', payload_hash: empty_sha256 }) {
		assert false
	}
}

fn test_signer_rejects_crlf_in_extra_headers() {
	creds := Credentials{
		access_key_id:     'KEY'
		secret_access_key: 'secret'
		region:            'us-east-1'
		endpoint:          'examplebucket.s3.amazonaws.com'
	}
	bad := SignRequest{
		method:        'GET'
		path:          '/foo'
		payload_hash:  empty_sha256
		extra_headers: {
			'x-evil': 'value\r\nInjected: yes'
		}
	}
	if _ := sign_request(creds, bad) {
		assert false, 'expected CRLF rejection'
	} else {
		if err is S3Error {
			assert err.code == 'InvalidHeader' || err.code == 'InvalidCredentials', 'unexpected code: ${err.code}'
		} else {
			assert false, 'wrong error type'
		}
	}
}

fn test_presign_rejects_crlf_in_extra_query() {
	creds := Credentials{
		access_key_id:     'KEY'
		secret_access_key: 'secret'
		region:            'us-east-1'
		endpoint:          'examplebucket.s3.amazonaws.com'
	}
	bad := PresignRequest{
		method:      'GET'
		path:        '/foo'
		extra_query: {
			'X-Evil': 'value\r\nInjected: yes'
		}
	}
	if _ := presign_url(creds, bad) {
		assert false, 'expected CRLF rejection'
	} else {
		assert err is S3Error
	}
}

fn test_presign_rejects_invalid_expiry() {
	creds := Credentials{
		access_key_id:     'KEY'
		secret_access_key: 'secret'
		region:            'us-east-1'
		endpoint:          'examplebucket.s3.amazonaws.com'
	}
	if _ := presign_url(creds, PresignRequest{ method: 'GET', path: '/x', expires_in: 0 }) {
		assert false, 'expires_in=0 must be rejected'
	}
	if _ := presign_url(creds, PresignRequest{
		method:     'GET'
		path:       '/x'
		expires_in: 700_000
	})
	{
		assert false, 'expires_in > 7 days must be rejected'
	}
}

fn test_presign_url_contains_required_query_params() {
	creds := Credentials{
		access_key_id:     'KEY'
		secret_access_key: 'secret'
		region:            'us-east-1'
		endpoint:          'examplebucket.s3.amazonaws.com'
	}
	url := presign_url(creds, PresignRequest{
		method:     'GET'
		path:       '/foo.txt'
		expires_in: 300
	}) or { panic(err) }
	assert url.contains('X-Amz-Algorithm=AWS4-HMAC-SHA256')
	assert url.contains('X-Amz-Credential=')
	assert url.contains('X-Amz-Date=')
	assert url.contains('X-Amz-Expires=300')
	assert url.contains('X-Amz-SignedHeaders=host')
	assert url.contains('X-Amz-Signature=')
}
