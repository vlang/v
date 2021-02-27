// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// CommonHeader is an enum of the most common HTTP headers
pub enum CommonHeader {
	accept
	accept_charset
	accept_encoding
	accept_language
	accept_ranges
	cache_control
	cc
	connection
	content_id
	content_language
	content_length
	content_transfer_encoding
	content_type
	cookie
	date
	dkim_signature
	etag
	expires
	from
	host
	if_modified_since
	if_none_match
	in_reply_to
	last_modified
	location
	message_id
	mime_version
	pragma
	received
	return_path
	server
	set_cookie
	subject
	to
	user_agent
	via
	x_forwarded_for
	x_imforwards
	x_powered_by
}

pub fn (h CommonHeader) str() string {
	return match h {
		.accept { 'Accept' }
		.accept_charset { 'Accept-Charset' }
		.accept_encoding { 'Accept-Encoding' }
		.accept_language { 'Accept-Language' }
		.accept_ranges { 'Accept-Ranges' }
		.cache_control { 'Cache-Control' }
		.cc { 'Cc' }
		.connection { 'Connection' }
		.content_id { 'Content-Id' }
		.content_language { 'Content-Language' }
		.content_length { 'Content-Length' }
		.content_transfer_encoding { 'Content-Transfer-Encoding' }
		.content_type { 'Content-Type' }
		.cookie { 'Cookie' }
		.date { 'Date' }
		.dkim_signature { 'Dkim-Signature' }
		.etag { 'Etag' }
		.expires { 'Expires' }
		.from { 'From' }
		.host { 'Host' }
		.if_modified_since { 'If-Modified-Since' }
		.if_none_match { 'If-None-Match' }
		.in_reply_to { 'In-Reply-To' }
		.last_modified { 'Last-Modified' }
		.location { 'Location' }
		.message_id { 'Message-Id' }
		.mime_version { 'Mime-Version' }
		.pragma { 'Pragma' }
		.received { 'Received' }
		.return_path { 'Return-Path' }
		.server { 'Server' }
		.set_cookie { 'Set-Cookie' }
		.subject { 'Subject' }
		.to { 'To' }
		.user_agent { 'User-Agent' }
		.via { 'Via' }
		.x_forwarded_for { 'X-Forwarded-For' }
		.x_imforwards { 'X-Imforwards' }
		.x_powered_by { 'X-Powered-By' }
	}
}

// HeaderKey can be either a CommonHeader or a custom header string
pub type HeaderKey = CommonHeader | string

// canonicalize will convert the HeaderKey to its canonical form, or return
// an error if there are invalid characters in the key.
pub fn (h HeaderKey) canonicalize() ?string {
	match h {
		CommonHeader {
			return h.str()
		}
		string {
			s := canonicalize(h) or { return error('Invalid header key') }
			return s
		}
	}
}

// Header represents the key-value pairs in an HTTP header
[noinit]
pub struct Header {
mut:
	data map[string][]string
}

pub struct HeaderConfig {
	key   HeaderKey
	value string
}

// Create a new Header object
pub fn new_header(kvs ...HeaderConfig) ?Header {
	mut h := Header{
		data: map[string][]string{}
	}
	for kv in kvs {
		h.add(kv.key, kv.value) ?
	}
	return h
}

// Append a value to the header key. This function will return an error
// if the key contains invalid header characters.
pub fn (mut h Header) add(key HeaderKey, value string) ? {
	k := key.canonicalize() ?
	h.data[k] << value
}

// Sets the key-value pair. This function will clear any other values
// that exist for the HeaderKey.
pub fn (mut h Header) set(key HeaderKey, value string) {
	k := key.canonicalize() or { return }
	h.data[k] = [value]
}

// Delete all key-value pairs.
pub fn (mut h Header) delete(key HeaderKey) {
	k := key.canonicalize() or { return }
	h.data.delete(k)
}

// Returns whether the header key exists in the map.
pub fn (h Header) contains(key HeaderKey) bool {
	k := key.canonicalize() or { return false }
	return k in h.data
}

// Gets the first value for the HeaderKey, or none if the key does
// not exist.
pub fn (h Header) get(key HeaderKey) ?string {
	k := key.canonicalize() ?
	if h.data[k].len == 0 {
		return none
	}
	return h.data[k][0]
}

// Gets all values for the HeaderKey.
pub fn (h Header) values(key HeaderKey) []string {
	k := key.canonicalize() or { return [] }
	return h.data[k]
}

// Validate and canonicalize an HTTP header key
// A canonical header is all lowercase except for the first character
// and any character after a `-`. Example: `Example-Header-Key`
fn canonicalize(s string) ?string {
	// check for valid header bytes
	for _, c in s {
		if int(c) >= 128 || !is_token(c) {
			return error('Invalid header key')
		}
	}

	// check for canonicalization; create a new string if not
	mut upper := true
	for _, c in s {
		if upper && `a` <= c && c <= `z` {
			return s.to_lower().split('-').map(it.capitalize()).join('-')
		}
		if !upper && `A` <= c && c <= `Z` {
			return s.to_lower().split('-').map(it.capitalize()).join('-')
		}
		upper = c == `-`
	}
	return s
}

// Checks if the byte is valid for a header token
fn is_token(b byte) bool {
	return match b {
		33, 35...39, 42, 43, 45, 46, 48...57, 65...90, 94...122, 124, 126 { true }
		else { false }
	}
}
