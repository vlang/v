// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

import crypto.hmac
import crypto.sha256
import time

// service_name is the SigV4 service identifier baked into the signing key.
pub const service_name = 's3'

// algo is the SigV4 algorithm marker S3 expects.
pub const algo = 'AWS4-HMAC-SHA256'

// unsigned_payload is the magic string used in `x-amz-content-sha256` when
// the payload is not pre-hashed. Used by the single-shot put path to avoid
// buffering / re-scanning the entire body just to sign it.
pub const unsigned_payload = 'UNSIGNED-PAYLOAD'

// empty_sha256 is `sha256("")` precomputed. Used for HEAD/GET/DELETE where
// there is no body and we want a real hash for stricter S3 endpoints.
pub const empty_sha256 = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'

// SignRequest describes the request to sign. The signer is intentionally
// agnostic of HTTP option semantics (ACL, storage class, …) — the caller
// pre-fills `extra_headers` with whatever it intends to send. That keeps the
// signer pure and easy to test against published SigV4 reference vectors.
pub struct SignRequest {
pub:
	method        string @[required] // GET, PUT, POST, DELETE, HEAD
	path          string @[required] // canonical URI (already URI-encoded, kept as-is)
	query         string            // canonical query string (sorted, encoded; no leading '?')
	payload_hash  string            // hex SHA-256 of body or `unsigned_payload`
	extra_headers map[string]string // lowercase keys, raw values — will be added to canonical/signed set
	sign_time     time.Time         // when omitted (Time{}) we use time.utc()
}

// SignedRequest is the output of header signing.
pub struct SignedRequest {
pub:
	method        string            // canonical HTTP method that was signed (GET, PUT, …)
	url           string            // scheme://host<extra_path>/<bucket>/<key>?<query>
	host          string            // host[:port], suitable for the Host header
	amz_date      string            // YYYYMMDDTHHMMSSZ
	authorization string            // ready-to-send Authorization header value
	headers       map[string]string // ALL headers that MUST be sent for the signature to verify
}

// sign_request builds the SigV4 Authorization header for an HTTP request.
//
// The returned `SignedRequest.headers` is the full set the caller must send
// (excluding `Content-Length`, which the HTTP client adds itself). Adding,
// removing, or mutating a header after signing will break the signature.
pub fn sign_request(creds Credentials, req SignRequest) !SignedRequest {
	creds.validate()!
	method := normalize_method(req.method) or {
		return new_error('InvalidMethod',
			'Method must be GET, PUT, POST, DELETE or HEAD; got: ${req.method}')
	}
	host := canonical_host(creds, '')
	if host == '' {
		return new_error('InvalidEndpoint',
			'No endpoint and no bucket provided — cannot determine host')
	}
	region := creds.resolved_region()
	now := if req.sign_time.year == 0 { time.utc() } else { req.sign_time }
	amz_date := format_amz_date(now)
	amz_day := amz_date[..8]
	payload := if req.payload_hash == '' { unsigned_payload } else { req.payload_hash }

	// Build the headers set. We always sign host + amz date + content sha
	// because S3 requires them. Reject CRLF before they reach the wire.
	mut headers := map[string]string{}
	for k, v in req.extra_headers {
		if contains_crlf(v) || contains_crlf(k) {
			return new_error('InvalidHeader',
				'Header "${k}" contains CR/LF — refused (header injection guard)')
		}
		headers[k.to_lower()] = v
	}
	headers['host'] = host
	headers['x-amz-content-sha256'] = payload
	headers['x-amz-date'] = amz_date
	if creds.session_token != '' {
		headers['x-amz-security-token'] = creds.session_token
	}

	signed_header_names := sorted_keys(headers).join(';')
	canonical := build_canonical_request(method, req.path, req.query, headers, signed_header_names,
		payload)
	credential_scope := '${amz_day}/${region}/${service_name}/aws4_request'
	string_to_sign := '${algo}\n${amz_date}\n${credential_scope}\n${sha256_hex(canonical.bytes())}'
	signature := to_hex_lower(hmac_sha256(derive_signing_key(creds.secret_access_key, amz_day,
		region, service_name), string_to_sign.bytes()))
	authorization := '${algo} Credential=${creds.access_key_id}/${credential_scope}, SignedHeaders=${signed_header_names}, Signature=${signature}'
	headers['authorization'] = authorization

	scheme := creds.scheme()
	mut url := '${scheme}://${host}${req.path}'
	if req.query != '' {
		url += '?' + req.query
	}
	return SignedRequest{
		method:        method
		url:           url
		host:          host
		amz_date:      amz_date
		authorization: authorization
		headers:       headers
	}
}

// PresignRequest describes a presigned URL to generate. The output is a
// self-contained URL — no extra headers are required at request time.
pub struct PresignRequest {
pub:
	method      string @[required]
	path        string @[required] // canonical URI (already URI-encoded)
	expires_in  int = 86400 // 1..604800 seconds (SigV4 hard limit is 7 days)
	extra_query map[string]string // additional signed query params (e.g. response-content-type, x-amz-acl)
	sign_time   time.Time
}

// presign_url returns a fully-formed `https://...` URL signed via SigV4
// query-string parameters. The URL is valid until `now + expires_in`.
pub fn presign_url(creds Credentials, req PresignRequest) !string {
	creds.validate()!
	method := normalize_method(req.method) or {
		return new_error('InvalidMethod',
			'Method must be GET, PUT, POST, DELETE or HEAD; got: ${req.method}')
	}
	if req.expires_in < 1 || req.expires_in > 604800 {
		return new_error('InvalidExpiry', 'expires_in must be between 1 and 604800 seconds')
	}
	host := canonical_host(creds, '')
	if host == '' {
		return new_error('InvalidEndpoint',
			'No endpoint and no bucket provided — cannot determine host')
	}
	region := creds.resolved_region()
	now := if req.sign_time.year == 0 { time.utc() } else { req.sign_time }
	amz_date := format_amz_date(now)
	amz_day := amz_date[..8]
	credential := '${creds.access_key_id}/${amz_day}/${region}/${service_name}/aws4_request'

	// Required signed query parameters; we then merge in extras from the caller.
	mut params := map[string]string{}
	params['X-Amz-Algorithm'] = algo
	params['X-Amz-Credential'] = credential
	params['X-Amz-Date'] = amz_date
	params['X-Amz-Expires'] = req.expires_in.str()
	params['X-Amz-SignedHeaders'] = 'host'
	if creds.session_token != '' {
		params['X-Amz-Security-Token'] = creds.session_token
	}
	for k, v in req.extra_query {
		if contains_crlf(k) || contains_crlf(v) {
			return new_error('InvalidQueryParam', 'Query param "${k}" contains CR/LF — refused')
		}
		params[k] = v
	}

	canonical_query := canonical_query_string(params)
	headers := {
		'host': host
	}
	canonical := build_canonical_request(method, req.path, canonical_query, headers, 'host',
		unsigned_payload)
	credential_scope := '${amz_day}/${region}/${service_name}/aws4_request'
	string_to_sign := '${algo}\n${amz_date}\n${credential_scope}\n${sha256_hex(canonical.bytes())}'
	signature := to_hex_lower(hmac_sha256(derive_signing_key(creds.secret_access_key, amz_day,
		region, service_name), string_to_sign.bytes()))
	scheme := creds.scheme()
	return '${scheme}://${host}${req.path}?${canonical_query}&X-Amz-Signature=${signature}'
}

// build_canonical_request assembles the canonical request string per
// SigV4 §3.2. `path` and `query` MUST already be URI-encoded.
pub fn build_canonical_request(method string, path string, query string, headers map[string]string,
	signed_headers string, payload_hash string) string {
	sorted := sorted_keys(headers)
	mut lines := []string{cap: sorted.len}
	for k in sorted {
		lines << '${k}:${normalize_header_value(headers[k])}'
	}
	return '${method}\n${path}\n${query}\n${lines.join('\n')}\n\n${signed_headers}\n${payload_hash}'
}

// canonical_query_string sorts query params by key (then by value if the same
// key appears twice — we don't here) and joins them as `k=v&k=v` with values
// already URI-encoded.
pub fn canonical_query_string(params map[string]string) string {
	keys := sorted_keys(params)
	mut parts := []string{cap: keys.len}
	for k in keys {
		parts << '${uri_encode_query(k)}=${uri_encode_query(params[k])}'
	}
	return parts.join('&')
}

// derive_signing_key implements the four-step HMAC chain that produces the
// SigV4 signing key. The result is cacheable per (secret, date, region,
// service) tuple — left to the caller to memoize if needed.
pub fn derive_signing_key(secret string, date string, region string, service string) []u8 {
	k_date := hmac_sha256(('AWS4' + secret).bytes(), date.bytes())
	k_region := hmac_sha256(k_date, region.bytes())
	k_service := hmac_sha256(k_region, service.bytes())
	return hmac_sha256(k_service, 'aws4_request'.bytes())
}

// format_amz_date returns the basic ISO-8601 timestamp used by SigV4
// (`YYYYMMDDTHHMMSSZ`). `t` is assumed to be in UTC.
pub fn format_amz_date(t time.Time) string {
	return '${t.year:04d}${t.month:02d}${t.day:02d}T${t.hour:02d}${t.minute:02d}${t.second:02d}Z'
}

// canonical_host resolves the on-the-wire host. Path-style addressing is
// the default; virtual-hosted style uses `<bucket>.<endpoint-host>`. If no
// endpoint is configured we fall back to `s3.<region>.amazonaws.com` (the
// canonical default for clients pointed at AWS S3 itself).
pub fn canonical_host(creds Credentials, bucket_override string) string {
	bucket := if bucket_override != '' { bucket_override } else { creds.bucket }
	host := creds.host_only()
	if host == '' {
		region := creds.resolved_region()
		if creds.virtual_hosted_style {
			if bucket == '' {
				return ''
			}
			return '${bucket}.s3.${region}.amazonaws.com'
		}
		return 's3.${region}.amazonaws.com'
	}
	if creds.virtual_hosted_style && bucket != '' {
		return '${bucket}.${host}'
	}
	return host
}

// normalize_method uppercases and validates the HTTP method.
pub fn normalize_method(m string) ?string {
	up := m.to_upper()
	return match up {
		'GET', 'PUT', 'POST', 'DELETE', 'HEAD' { up }
		else { none }
	}
}

// normalize_header_value collapses runs of internal whitespace to a single
// space and trims leading/trailing whitespace, per SigV4 §3.2.2 step 3.
fn normalize_header_value(v string) string {
	if v == '' {
		return v
	}
	mut out := []u8{cap: v.len}
	mut prev_space := false
	mut started := false
	for b in v.bytes() {
		if b == ` ` || b == `\t` {
			if started {
				prev_space = true
			}
			continue
		}
		if prev_space {
			out << ` `
			prev_space = false
		}
		out << b
		started = true
	}
	return out.bytestr()
}

// sha256_hex returns the lowercase hex digest of `data`.
@[inline]
pub fn sha256_hex(data []u8) string {
	return to_hex_lower(sha256.sum256(data))
}

// hmac_sha256 wraps `crypto.hmac.new` for HMAC-SHA-256 with the V stdlib's
// type signature (a hash function that returns `[]u8`).
@[inline]
pub fn hmac_sha256(key []u8, data []u8) []u8 {
	return hmac.new(key, data, sha256.sum256, sha256.block_size)
}

// sorted_keys returns the keys of `m` in lexical order.
fn sorted_keys(m map[string]string) []string {
	mut keys := m.keys()
	keys.sort()
	return keys
}
