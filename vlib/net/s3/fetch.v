// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import net.http

// FetchOptions overlays an S3 endpoint over the `fetch` call. All fields
// are optional; bucket and key are taken from the URL.
@[params]
pub struct FetchOptions {
pub:
	method      http.Method = .get
	body        []u8
	credentials Credentials
	// content_type, acl, etc. are forwarded as-is when method is PUT/POST.
	content_type        string
	content_disposition string
	content_encoding    string
	cache_control       string
	acl                 Acl
	storage_class       StorageClass
	request_payer       bool
	range               string
	hash_payload        bool
}

// FetchResponse is the simplified return type of `fetch`. It's intentionally
// flat (no streaming yet) — easier to consume than V's `http.Response` and
// surfaces the most useful fields.
pub struct FetchResponse {
pub:
	status_code    int
	body           []u8
	headers        map[string]string
	etag           string
	content_type   string
	content_length i64
}

// fetch is a `fetch('s3://bucket/key', { ... })`-style helper.
//
// Examples:
//
//   resp := s3.fetch('s3://my-bucket/path/to/file.txt')!
//   resp := s3.fetch('s3://my-bucket/key', method: .put, body: 'hello'.bytes())!
//   resp := s3.fetch('s3://key', method: .get, credentials: s3.Credentials{ bucket: 'b', ... })!
//
// The URL must use the `s3://` scheme. Anything else is rejected outright
// (avoids accidentally calling a real HTTP endpoint with S3 credentials).
pub fn fetch(url string, opts FetchOptions) !FetchResponse {
	if !url.starts_with('s3://') {
		return new_error('InvalidURL', 's3.fetch only accepts s3:// URLs (got: ${redact_url(url)})')
	}
	bucket, key := parse_s3_url(url)!
	mut creds := if opts.credentials.access_key_id == '' {
		Credentials.from_env()
	} else {
		opts.credentials
	}
	if bucket != '' {
		creds = Credentials{
			...creds
			bucket: bucket
		}
	}
	creds.validate()!

	c := Client{
		credentials: creds
	}
	match opts.method {
		.get {
			data := c.get(key,
				bucket:        bucket
				range:         opts.range
				request_payer: opts.request_payer
			)!
			return FetchResponse{
				status_code:    200
				body:           data
				content_length: i64(data.len)
			}
		}
		.head {
			st := c.stat(key,
				bucket:        bucket
				request_payer: opts.request_payer
			)!
			return FetchResponse{
				status_code:    200
				etag:           st.etag
				content_type:   st.content_type
				content_length: st.size
			}
		}
		.put, .post {
			c.put(key, opts.body,
				bucket:              bucket
				content_type:        opts.content_type
				content_disposition: opts.content_disposition
				content_encoding:    opts.content_encoding
				cache_control:       opts.cache_control
				acl:                 opts.acl
				storage_class:       opts.storage_class
				request_payer:       opts.request_payer
				hash_payload:        opts.hash_payload
			)!
			return FetchResponse{
				status_code: 200
			}
		}
		.delete {
			c.delete(key,
				bucket:        bucket
				request_payer: opts.request_payer
			)!
			return FetchResponse{
				status_code: 204
			}
		}
		else {
			return new_error('InvalidMethod', 's3.fetch: unsupported method ${opts.method}')
		}
	}

	return new_error('InvalidMethod', 's3.fetch: unreachable')
}

// parse_s3_url splits `s3://bucket/key/with/slashes` into (bucket, key).
//
// Special case: `s3://key` (no second path component) returns ('', 'key') —
// the caller is then expected to provide the bucket via credentials.
pub fn parse_s3_url(url string) !(string, string) {
	if !url.starts_with('s3://') {
		return new_error('InvalidURL', 'Not an s3:// URL')
	}
	rest := url[5..] // strip 's3://'
	if rest == '' {
		return new_error('InvalidURL', 'Empty s3:// URL')
	}
	if i := rest.index('/') {
		bucket := rest[..i]
		key := rest[i + 1..]
		if key == '' {
			return new_error('InvalidURL', 'Empty key in s3:// URL')
		}
		return bucket, key
	}
	// no '/' separator — treat the whole rest as the key
	return '', rest
}

// redact_url strips query strings before logging. Used in error messages so
// presigned-URL-style query params (which can contain credentials) aren't
// leaked into logs.
pub fn redact_url(url string) string {
	if i := url.index('?') {
		return url[..i] + '?<redacted>'
	}
	return url
}
