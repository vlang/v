// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// BucketOptions configures bucket-level operations. `region_constraint`
// lets `create_bucket` pin the bucket to a specific region (S3 sends a
// `<CreateBucketConfiguration>` body when this is non-empty).
@[params]
pub struct BucketOptions {
pub:
	bucket            string
	acl               Acl
	region_constraint string
}

// create_bucket creates a new bucket. Returns:
//   - nil on success (HTTP 200)
//   - S3Error("BucketAlreadyOwnedByYou") if you already own this bucket
//   - S3Error("BucketAlreadyExists") if someone else owns it
//   - S3Error("InvalidBucketName") for non-conformant names
//
// The S3 wire response for these states is HTTP 409, parsed from the
// returned XML body.
pub fn (c &Client) create_bucket(opts BucketOptions) ! {
	bucket := pick_bucket(c.credentials, opts.bucket)!
	validate_bucket_name(bucket)!
	creds := c.creds_for(bucket)
	path := bucket_path(creds, bucket)
	body := if opts.region_constraint != '' {
		'<CreateBucketConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><LocationConstraint>${escape_xml(opts.region_constraint)}</LocationConstraint></CreateBucketConfiguration>'
	} else {
		''
	}
	mut headers := map[string]string{}
	if opts.acl != .unset {
		headers['x-amz-acl'] = opts.acl.to_header_value()
	}
	if body != '' {
		headers['content-type'] = 'application/xml'
	}
	signed := sign_request(creds, SignRequest{
		method:        'PUT'
		path:          path
		payload_hash:  if body == '' { empty_sha256 } else { sha256_hex(body.bytes()) }
		extra_headers: headers
	})!
	resp := c.do_http(signed, body)!
	if resp.status_code !in [200, 204] {
		return new_http_error(resp.status_code, bucket, resp.body)
	}
}

// delete_bucket removes an empty bucket. S3 returns 409 BucketNotEmpty if it
// still has keys — caller is expected to clean up first or handle the error.
pub fn (c &Client) delete_bucket(opts BucketOptions) ! {
	bucket := pick_bucket(c.credentials, opts.bucket)!
	creds := c.creds_for(bucket)
	path := bucket_path(creds, bucket)
	signed := sign_request(creds, SignRequest{
		method:       'DELETE'
		path:         path
		payload_hash: empty_sha256
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code !in [200, 204] {
		return new_http_error(resp.status_code, bucket, resp.body)
	}
}

// bucket_exists checks bucket existence/access. Returns true if accessible (200),
// false on 404 / 403 (no such bucket or no read permission), error on other
// statuses. Uses HEAD under the hood — no body is fetched.
pub fn (c &Client) bucket_exists(opts BucketOptions) !bool {
	bucket := pick_bucket(c.credentials, opts.bucket)!
	creds := c.creds_for(bucket)
	path := bucket_path(creds, bucket)
	signed := sign_request(creds, SignRequest{
		method:       'HEAD'
		path:         path
		payload_hash: empty_sha256
	})!
	resp := c.do_http(signed, '')!
	if resp.status_code == 404 || resp.status_code == 403 {
		return false
	}
	if resp.status_code != 200 {
		return new_http_error(resp.status_code, bucket, resp.body)
	}
	return true
}

// validate_bucket_name applies the S3 bucket naming rules honoured by most
// providers:
//   - 3..63 chars
//   - lowercase letters, digits, dots, hyphens only
//   - must start/end with letter or digit
//   - no consecutive dots, no `.-` / `-.`
//   - cannot look like an IPv4 address
//
// Provider-specific reservations (e.g. `xn--`, `sthree-`) are intentionally
// not checked here — they vary, so the server-side error is authoritative.
pub fn validate_bucket_name(name string) ! {
	if name.len < 3 || name.len > 63 {
		return new_error('InvalidBucketName', 'Bucket name must be 3..63 characters: ${name}')
	}
	first := name[0]
	last := name[name.len - 1]
	if !is_lower_alnum(first) || !is_lower_alnum(last) {
		return new_error('InvalidBucketName',
			'Bucket name must start and end with a lowercase letter or digit: ${name}')
	}
	mut prev := u8(0)
	for b in name.bytes() {
		if !is_bucket_char(b) {
			return new_error('InvalidBucketName', 'Bucket name "${name}" contains invalid character: ${[
				b,
			].bytestr()}')
		}
		if prev == `.` && b == `.` {
			return new_error('InvalidBucketName', 'Bucket name "${name}" contains consecutive dots')
		}
		if (prev == `.` && b == `-`) || (prev == `-` && b == `.`) {
			return new_error('InvalidBucketName',
				'Bucket name "${name}" contains adjacent dot/hyphen')
		}
		prev = b
	}
	if looks_like_ipv4(name) {
		return new_error('InvalidBucketName', 'Bucket name "${name}" looks like an IP address')
	}
}

fn is_lower_alnum(b u8) bool {
	return (b >= `a` && b <= `z`) || (b >= `0` && b <= `9`)
}

fn is_bucket_char(b u8) bool {
	return is_lower_alnum(b) || b == `-` || b == `.`
}

fn looks_like_ipv4(s string) bool {
	parts := s.split('.')
	if parts.len != 4 {
		return false
	}
	for p in parts {
		if p.len == 0 || p.len > 3 {
			return false
		}
		for b in p.bytes() {
			if b < `0` || b > `9` {
				return false
			}
		}
	}
	return true
}

fn pick_bucket(c Credentials, override string) !string {
	b := if override != '' { override } else { c.bucket }
	if b == '' {
		return new_error('InvalidPath',
			'No bucket given (set Credentials.bucket or pass bucket via options)')
	}
	return b
}

fn bucket_path(creds Credentials, bucket string) string {
	extra := creds.extra_path()
	if creds.virtual_hosted_style {
		return if extra == '' { '/' } else { extra }
	}
	encoded := uri_encode_path(strip_slashes(bucket))
	return '${extra}/${encoded}'
}

// escape_xml does the minimal XML special-char escape we need for body content.
fn escape_xml(s string) string {
	mut out := []u8{cap: s.len}
	for b in s.bytes() {
		match b {
			`<` { out << '&lt;'.bytes() }
			`>` { out << '&gt;'.bytes() }
			`&` { out << '&amp;'.bytes() }
			`"` { out << '&quot;'.bytes() }
			`'` { out << '&apos;'.bytes() }
			else { out << b }
		}
	}
	return out.bytestr()
}
