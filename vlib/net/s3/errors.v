// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

// S3Error is the structured error returned for every signing or service
// failure. `code` is stable (e.g. `NoSuchKey`, `MissingCredentials`),
// `message` is human-readable, `status` is the HTTP status (0 for
// client-side errors), and `path` is the offending object key when known.
//
// V's `IError` interface only requires `msg()` and `code()`, so this type can
// be returned via `!T` and inspected with type-assertion / `as`.
pub struct S3Error {
pub:
	code       string
	message    string
	status     int
	path       string
	resource   string // S3's <Resource> field, when present
	request_id string // S3 RequestId, useful for support tickets
}

// msg renders the error in a single line. Includes the S3 error code so users
// can switch on it without parsing the prose. Path is appended when known.
pub fn (e &S3Error) msg() string {
	mut buf := '[${e.code}] ${e.message}'
	if e.status != 0 {
		buf += ' (HTTP ${e.status})'
	}
	if e.path != '' {
		buf += ' — ${e.path}'
	}
	return buf
}

// code returns a stable numeric code so callers can use `if err.code() == ...`.
// We map a few well-known S3 codes; everything else returns 0 so callers fall
// back to string comparison on `e.code`.
pub fn (e &S3Error) code() int {
	return match e.code {
		'NoSuchKey' {
			404
		}
		'NoSuchBucket' {
			404
		}
		'BucketAlreadyExists', 'BucketAlreadyOwnedByYou' {
			409
		}
		'AccessDenied', 'SignatureDoesNotMatch' {
			403
		}
		'MissingCredentials', 'InvalidEndpoint', 'InvalidPath', 'InvalidMethod',
		'InvalidSessionToken' {
			400
		}
		else {
			e.status
		}
	}
}

// new_error builds an S3Error from a code + message. Use this for client-side
// validation failures (missing creds, invalid path, etc.).
pub fn new_error(code string, message string) IError {
	return &S3Error{
		code:    code
		message: message
	}
}

// new_http_error wraps an HTTP-level failure. Body is the raw response body
// — `parse_xml_error` is responsible for digging out the structured
// `<Error>` envelope when the server returns one.
pub fn new_http_error(status int, path string, body string) IError {
	parsed := parse_xml_error(body)
	if parsed.code != '' {
		return &S3Error{
			code:       parsed.code
			message:    parsed.message
			status:     status
			path:       path
			resource:   parsed.resource
			request_id: parsed.request_id
		}
	}
	// Body was empty or unparseable — fall back to a generic code keyed by status.
	return &S3Error{
		code:    fallback_code_for(status)
		message: if body.len > 0 { body } else { 'HTTP ${status}' }
		status:  status
		path:    path
	}
}

// XmlErrorFields is what we pull out of an `<Error>` XML envelope.
struct XmlErrorFields {
	code       string
	message    string
	resource   string
	request_id string
}

// parse_xml_error extracts the standard S3 error XML:
//
//   <Error>
//     <Code>...</Code>
//     <Message>...</Message>
//     <Resource>...</Resource>
//     <RequestId>...</RequestId>
//   </Error>
//
// We use a tiny tag scanner (no DOM) — the format is rigid enough that this
// stays correct, faster than spinning up the full XML parser, and there is no
// attribute parsing to worry about.
fn parse_xml_error(body string) XmlErrorFields {
	return XmlErrorFields{
		code:       extract_xml_tag(body, 'Code')
		message:    extract_xml_tag(body, 'Message')
		resource:   extract_xml_tag(body, 'Resource')
		request_id: extract_xml_tag(body, 'RequestId')
	}
}

// extract_xml_tag returns the inner text of `<tag>...</tag>` (first match,
// case-sensitive) with the five predefined XML entities decoded, or '' if
// the tag is absent.
pub fn extract_xml_tag(body string, tag string) string {
	open := '<' + tag + '>'
	close := '</' + tag + '>'
	start := body.index(open) or { return '' }
	rest_off := start + open.len
	end := body.index_after(close, rest_off) or { return '' }
	return decode_xml_entities(body[rest_off..end])
}

// decode_xml_entities decodes the five predefined XML entities. We don't try
// to handle arbitrary `&#nn;` sequences because S3 only ever uses these five.
pub fn decode_xml_entities(s string) string {
	if !s.contains('&') {
		return s
	}
	mut out := []u8{cap: s.len}
	mut i := 0
	for i < s.len {
		if s[i] != `&` {
			out << s[i]
			i++
			continue
		}
		matched := match true {
			s.len - i >= 5 && s[i..i + 5] == '&amp;' {
				out << `&`
				i += 5
				true
			}
			s.len - i >= 4 && s[i..i + 4] == '&lt;' {
				out << `<`
				i += 4
				true
			}
			s.len - i >= 4 && s[i..i + 4] == '&gt;' {
				out << `>`
				i += 4
				true
			}
			s.len - i >= 6 && s[i..i + 6] == '&quot;' {
				out << `"`
				i += 6
				true
			}
			s.len - i >= 6 && s[i..i + 6] == '&apos;' {
				out << `'`
				i += 6
				true
			}
			else {
				false
			}
		}

		if !matched {
			out << s[i]
			i++
		}
	}
	return out.bytestr()
}

fn fallback_code_for(status int) string {
	return match status {
		301, 307 { 'PermanentRedirect' }
		400 { 'BadRequest' }
		401, 403 { 'AccessDenied' }
		404 { 'NotFound' }
		405 { 'MethodNotAllowed' }
		409 { 'Conflict' }
		411 { 'LengthRequired' }
		412 { 'PreconditionFailed' }
		416 { 'InvalidRange' }
		429 { 'TooManyRequests' }
		500 { 'InternalError' }
		503 { 'ServiceUnavailable' }
		else { 'HTTPError' }
	}
}
