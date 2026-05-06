// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import net.http
import time

// Client is the entry point for S3 operations. It carries default credentials
// and tuneable HTTP behaviour; per-call overrides go through the option params
// of each method. Instantiate once, reuse for many objects.
@[heap]
pub struct Client {
pub:
	credentials Credentials
	// part_size is the multipart-upload chunk size in bytes (default 5 MiB,
	// the S3 minimum).
	part_size i64 = 5 * 1024 * 1024
	// queue_size is the intended parallel-upload concurrency for multipart
	// (currently sequential; reserved).
	queue_size int = 5
	// retry is the number of retry attempts for failed uploads.
	retry int = 3
	// read_timeout / write_timeout map onto V's net.http settings.
	// Defaults are generous because parts can be 5 MiB+ on slow links.
	read_timeout  i64 = 5 * 60 * time.second
	write_timeout i64 = 5 * 60 * time.second
}

// new_client builds a Client. Pass an empty Credentials to fall back to env vars.
pub fn new_client(creds Credentials) Client {
	resolved := if creds.access_key_id == '' && creds.secret_access_key == '' {
		Credentials.from_env().merge(creds)
	} else {
		creds
	}
	return Client{
		credentials: resolved
	}
}

// PutOptions configures a put / write call.
@[params]
pub struct PutOptions {
pub:
	bucket              string
	content_type        string
	content_disposition string
	content_encoding    string
	cache_control       string
	acl                 Acl
	storage_class       StorageClass
	request_payer       bool
	// hash_payload, when true, computes SHA-256 of the body before signing
	// instead of using `UNSIGNED-PAYLOAD`. Slightly stronger integrity guarantee
	// at the cost of one full body scan.
	hash_payload bool
}

// GetOptions configures a get / read call.
@[params]
pub struct GetOptions {
pub:
	bucket        string
	range         string // e.g. 'bytes=0-1023' for partial downloads
	version_id    string
	request_payer bool
}

// StatOptions configures stat / size / exists checks.
@[params]
pub struct StatOptions {
pub:
	bucket        string
	request_payer bool
}

// put uploads `data` to `key`. Use `upload_file` / `upload_bytes_multipart`
// for streaming or for files larger than ~100 MiB; `put` keeps everything in
// memory.
pub fn (c &Client) put(key string, data []u8, opts PutOptions) ! {
	creds := c.creds_for(opts.bucket)
	host := canonical_host(creds, opts.bucket)
	if host == '' {
		return new_error('InvalidEndpoint', 'cannot determine S3 host (set endpoint or bucket)')
	}
	path := build_object_path(creds, opts.bucket, key)!
	payload_hash := if opts.hash_payload { sha256_hex(data) } else { unsigned_payload }
	signed := sign_request(creds, SignRequest{
		method:        'PUT'
		path:          path
		payload_hash:  payload_hash
		extra_headers: put_object_headers(opts)
	})!
	resp := c.do_http(signed, data.bytestr())!
	if resp.status_code !in [200, 204] {
		return new_http_error(resp.status_code, key, resp.body)
	}
}

// put_object_headers maps `PutOptions` to the wire headers used by both the
// single-shot `put` and the multipart `initiate_multipart` paths. Keeping the
// mapping in one place ensures the two upload modes accept the exact same set
// of options without drift.
fn put_object_headers(opts PutOptions) map[string]string {
	mut headers := map[string]string{}
	if opts.content_type != '' {
		headers['content-type'] = opts.content_type
	}
	if opts.content_disposition != '' {
		headers['content-disposition'] = opts.content_disposition
	}
	if opts.content_encoding != '' {
		headers['content-encoding'] = opts.content_encoding
	}
	if opts.cache_control != '' {
		headers['cache-control'] = opts.cache_control
	}
	if opts.acl != .unset {
		headers['x-amz-acl'] = opts.acl.to_header_value()
	}
	if opts.storage_class != .unset {
		headers['x-amz-storage-class'] = opts.storage_class.to_header_value()
	}
	if opts.request_payer {
		headers['x-amz-request-payer'] = 'requester'
	}
	return headers
}

// get downloads the entire object body into memory. For large files set a
// `range` and assemble the result yourself, or use a presigned URL with an
// HTTP streaming client.
pub fn (c &Client) get(key string, opts GetOptions) ![]u8 {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	mut headers := map[string]string{}
	if opts.range != '' {
		headers['range'] = opts.range
	}
	if opts.request_payer {
		headers['x-amz-request-payer'] = 'requester'
	}
	mut query := ''
	if opts.version_id != '' {
		query = 'versionId=' + uri_encode_query(opts.version_id)
	}
	signed := sign_request(creds, SignRequest{
		method:        'GET'
		path:          path
		query:         query
		payload_hash:  empty_sha256
		extra_headers: headers
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code !in [200, 206] {
		return new_http_error(resp.status_code, key, resp.body)
	}
	return resp.body.bytes()
}

// get_string is a convenience wrapper around `get` that returns the body as a
// V `string`.
pub fn (c &Client) get_string(key string, opts GetOptions) !string {
	bytes := c.get(key, opts)!
	return bytes.bytestr()
}

// stat returns the object metadata (size / last_modified / etag / content_type).
// Returns an `S3Error` with code `NoSuchKey` when the object doesn't exist.
pub fn (c &Client) stat(key string, opts StatOptions) !Stat {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	mut headers := map[string]string{}
	if opts.request_payer {
		headers['x-amz-request-payer'] = 'requester'
	}
	signed := sign_request(creds, SignRequest{
		method:        'HEAD'
		path:          path
		payload_hash:  empty_sha256
		extra_headers: headers
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code == 404 {
		return new_error('NoSuchKey', 'No such key: ${key}')
	}
	if resp.status_code !in [200, 204] {
		return new_http_error(resp.status_code, key, resp.body)
	}
	return Stat{
		size:          resp.header.get(.content_length) or { '0' }.i64()
		last_modified: resp.header.get(.last_modified) or { '' }
		etag:          (resp.header.get(.etag) or { '' }).trim('"')
		content_type:  resp.header.get(.content_type) or { '' }
	}
}

// exists is `stat` + boolean: true on 200, false on 404, error otherwise.
pub fn (c &Client) exists(key string, opts StatOptions) !bool {
	c.stat(key, opts) or {
		if err is S3Error && err.code == 'NoSuchKey' {
			return false
		}
		return err
	}
	return true
}

// size returns just the Content-Length of the object.
pub fn (c &Client) size(key string, opts StatOptions) !i64 {
	st := c.stat(key, opts)!
	return st.size
}

// delete removes a single object. S3 returns 204 on success, 204 on
// already-absent (idempotent) — we surface success in both cases.
pub fn (c &Client) delete(key string, opts StatOptions) ! {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	mut headers := map[string]string{}
	if opts.request_payer {
		headers['x-amz-request-payer'] = 'requester'
	}
	signed := sign_request(creds, SignRequest{
		method:        'DELETE'
		path:          path
		payload_hash:  empty_sha256
		extra_headers: headers
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code !in [200, 204] {
		return new_http_error(resp.status_code, key, resp.body)
	}
}

// presign generates a presigned URL — see PresignOptions for tunables.
pub fn (c &Client) presign(key string, opts PresignOptions) !string {
	creds := c.creds_for(opts.bucket)
	path := build_object_path(creds, opts.bucket, key)!
	mut extra := map[string]string{}
	if opts.acl != .unset {
		extra['X-Amz-Acl'] = opts.acl.to_header_value()
	}
	if opts.storage_class != .unset {
		extra['x-amz-storage-class'] = opts.storage_class.to_header_value()
	}
	if opts.content_type != '' {
		extra['response-content-type'] = opts.content_type
	}
	if opts.content_disposition != '' {
		extra['response-content-disposition'] = opts.content_disposition
	}
	if opts.request_payer {
		extra['x-amz-request-payer'] = 'requester'
	}
	return presign_url(creds, PresignRequest{
		method:      http_method_to_string(opts.method)
		path:        path
		expires_in:  opts.expires_in
		extra_query: extra
	})
}

// file returns a File reference for the given key, bound to this client.
// `path` may be `<bucket>/<key>` if the client has no default bucket and
// `bucket` here is empty.
pub fn (c &Client) file(key string, opts FileOptions) File {
	return File{
		client: c
		key:    key
		bucket: opts.bucket
	}
}

// creds_for returns credentials with `bucket` overridden if the caller passed one.
fn (c &Client) creds_for(bucket string) Credentials {
	if bucket == '' {
		return c.credentials
	}
	mut copy := c.credentials
	copy = Credentials{
		...copy
		bucket: bucket
	}
	return copy
}

// build_object_path produces the canonical URI path for a key.
// Path style: `/<bucket>/<encoded-key>[+endpoint extra path]`
// Virtual hosted: `/<encoded-key>`
// Returns an error if neither bucket nor key is provided.
// The key is forwarded byte-exact (only percent-encoded): S3 treats keys as
// opaque identifiers, so `folder/`, `/x` and `a//b` all designate distinct
// objects and must reach the wire as written.
pub fn build_object_path(creds Credentials, bucket_override string, key string) !string {
	bucket := if bucket_override != '' { bucket_override } else { creds.bucket }
	if key == '' {
		return new_error('InvalidPath', 'Empty object key')
	}
	encoded_key := uri_encode_path(key)
	extra := creds.extra_path()
	if creds.virtual_hosted_style {
		return '${extra}/${encoded_key}'
	}
	if bucket == '' {
		return new_error('InvalidPath',
			'No bucket given (set Credentials.bucket or pass bucket via options)')
	}
	encoded_bucket := uri_encode_path(strip_slashes(bucket))
	return '${extra}/${encoded_bucket}/${encoded_key}'
}

// HttpResponse is the small subset of an HTTP response we surface to callers
// of the lower-level helpers. `body` is the raw response body for non-stream
// requests; `header` keeps V's typed Header for parsing.
pub struct HttpResponse {
pub:
	status_code int
	body        string
	header      http.Header
}

// do_http fires off the signed request via V's `net.http`. The HTTP method is
// taken from `signed.method` (set by `sign_request`) so callers cannot drift
// between what was signed and what is sent on the wire.
fn (c &Client) do_http(signed SignedRequest, body string) !HttpResponse {
	mut header := http.new_header()
	for k, v in signed.headers {
		// `Host` is set automatically by V's http client; skip to avoid duplicates.
		if k.to_lower() == 'host' {
			continue
		}
		header.add_custom(canonical_header_name(k), v)!
	}
	method_enum := parse_http_method(signed.method)!
	req := http.new_request(method_enum, signed.url, body)
	mut req_mut := http.Request{
		...req
		header:        header
		read_timeout:  c.read_timeout
		write_timeout: c.write_timeout
	}
	resp := req_mut.do() or {
		return new_error('NetworkError', 'HTTP request failed: ${err.msg()}')
	}
	$if s3_debug ? {
		eprintln('[s3] ${signed.method} ${signed.url}')
		for k, v in signed.headers {
			eprintln('[s3]   > ${k}: ${redacted_value(k, v)}')
		}
		eprintln('[s3] -> ${resp.status_code}')
		eprintln('[s3] body: ${resp.body}')
	}
	return HttpResponse{
		status_code: resp.status_code
		body:        resp.body
		header:      resp.header
	}
}

// redacted_value masks credentials so they cannot leak via s3_debug logs.
fn redacted_value(name string, value string) string {
	low := name.to_lower()
	if low == 'authorization' || low.starts_with('x-amz-security-token') {
		return '<redacted>'
	}
	return value
}

// canonical_header_name turns 'x-amz-content-sha256' into 'X-Amz-Content-Sha256'
// for the wire. HTTP/1.1 says headers are case-insensitive, but some
// intermediaries / mocks aren't, so we emit the canonical Title-Case form.
fn canonical_header_name(s string) string {
	mut out := []u8{cap: s.len}
	mut upper_next := true
	for b in s.bytes() {
		if b == `-` {
			out << b
			upper_next = true
			continue
		}
		if upper_next && b >= `a` && b <= `z` {
			out << b - 32
		} else {
			out << b
		}
		upper_next = false
	}
	return out.bytestr()
}

// parse_http_method maps a SigV4 canonical method string to V's `http.Method`.
// The signer enforces the supported set up-front, so anything unexpected here
// indicates a programming bug rather than user input.
fn parse_http_method(s string) !http.Method {
	return match s.to_upper() {
		'GET' { http.Method.get }
		'PUT' { http.Method.put }
		'POST' { http.Method.post }
		'DELETE' { http.Method.delete }
		'HEAD' { http.Method.head }
		else { new_error('InvalidMethod', 'Unsupported HTTP method: ${s}') }
	}
}
