// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import strings

// Header represents the key-value pairs in an HTTP header
[noinit]
pub struct Header {
mut:
	data map[string][]string
	// map of lowercase header keys to their original keys
	// in order of appearance
	keys map[string][]string
}

// CommonHeader is an enum of the most common HTTP headers
pub enum CommonHeader {
	accept
	accept_ch
	accept_charset
	accept_ch_lifetime
	accept_encoding
	accept_language
	accept_patch
	accept_post
	accept_ranges
	access_control_allow_credentials
	access_control_allow_headers
	access_control_allow_methods
	access_control_allow_origin
	access_control_expose_headers
	access_control_max_age
	access_control_request_headers
	access_control_request_method
	age
	allow
	alt_svc
	authorization
	authority
	cache_control
	clear_site_data
	connection
	content_disposition
	content_encoding
	content_language
	content_length
	content_location
	content_range
	content_security_policy
	content_security_policy_report_only
	content_type
	cookie
	cross_origin_embedder_policy
	cross_origin_opener_policy
	cross_origin_resource_policy
	date
	device_memory
	digest
	dnt
	early_data
	etag
	expect
	expect_ct
	expires
	feature_policy
	forwarded
	from
	host
	if_match
	if_modified_since
	if_none_match
	if_range
	if_unmodified_since
	index
	keep_alive
	large_allocation
	last_modified
	link
	location
	nel
	origin
	pragma
	proxy_authenticate
	proxy_authorization
	range
	referer
	referrer_policy
	retry_after
	save_data
	sec_fetch_dest
	sec_fetch_mode
	sec_fetch_site
	sec_fetch_user
	sec_websocket_accept
	server
	server_timing
	set_cookie
	sourcemap
	strict_transport_security
	te
	timing_allow_origin
	tk
	trailer
	transfer_encoding
	upgrade
	upgrade_insecure_requests
	user_agent
	vary
	via
	want_digest
	warning
	www_authenticate
	x_content_type_options
	x_dns_prefetch_control
	x_forwarded_for
	x_forwarded_host
	x_forwarded_proto
	x_frame_options
	x_xss_protection
}

pub fn (h CommonHeader) str() string {
	return match h {
		.accept { 'Accept' }
		.accept_ch { 'Accept-CH' }
		.accept_charset { 'Accept-Charset' }
		.accept_ch_lifetime { 'Accept-CH-Lifetime' }
		.accept_encoding { 'Accept-Encoding' }
		.accept_language { 'Accept-Language' }
		.accept_patch { 'Accept-Patch' }
		.accept_post { 'Accept-Post' }
		.accept_ranges { 'Accept-Ranges' }
		.access_control_allow_credentials { 'Access-Control-Allow-Credentials' }
		.access_control_allow_headers { 'Access-Control-Allow-Headers' }
		.access_control_allow_methods { 'Access-Control-Allow-Methods' }
		.access_control_allow_origin { 'Access-Control-Allow-Origin' }
		.access_control_expose_headers { 'Access-Control-Expose-Headers' }
		.access_control_max_age { 'Access-Control-Max-Age' }
		.access_control_request_headers { 'Access-Control-Request-Headers' }
		.access_control_request_method { 'Access-Control-Request-Method' }
		.age { 'Age' }
		.allow { 'Allow' }
		.alt_svc { 'Alt-Svc' }
		.authorization { 'Authorization' }
		.authority { 'Authority' }
		.cache_control { 'Cache-Control' }
		.clear_site_data { 'Clear-Site-Data' }
		.connection { 'Connection' }
		.content_disposition { 'Content-Disposition' }
		.content_encoding { 'Content-Encoding' }
		.content_language { 'Content-Language' }
		.content_length { 'Content-Length' }
		.content_location { 'Content-Location' }
		.content_range { 'Content-Range' }
		.content_security_policy { 'Content-Security-Policy' }
		.content_security_policy_report_only { 'Content-Security-Policy-Report-Only' }
		.content_type { 'Content-Type' }
		.cookie { 'Cookie' }
		.cross_origin_embedder_policy { 'Cross-Origin-Embedder-Policy' }
		.cross_origin_opener_policy { 'Cross-Origin-Opener-Policy' }
		.cross_origin_resource_policy { 'Cross-Origin-Resource-Policy' }
		.date { 'Date' }
		.device_memory { 'Device-Memory' }
		.digest { 'Digest' }
		.dnt { 'DNT' }
		.early_data { 'Early-Data' }
		.etag { 'ETag' }
		.expect { 'Expect' }
		.expect_ct { 'Expect-CT' }
		.expires { 'Expires' }
		.feature_policy { 'Feature-Policy' }
		.forwarded { 'Forwarded' }
		.from { 'From' }
		.host { 'Host' }
		.if_match { 'If-Match' }
		.if_modified_since { 'If-Modified-Since' }
		.if_none_match { 'If-None-Match' }
		.if_range { 'If-Range' }
		.if_unmodified_since { 'If-Unmodified-Since' }
		.index { 'Index' }
		.keep_alive { 'Keep-Alive' }
		.large_allocation { 'Large-Allocation' }
		.last_modified { 'Last-Modified' }
		.link { 'Link' }
		.location { 'Location' }
		.nel { 'NEL' }
		.origin { 'Origin' }
		.pragma { 'Pragma' }
		.proxy_authenticate { 'Proxy-Authenticate' }
		.proxy_authorization { 'Proxy-Authorization' }
		.range { 'Range' }
		.referer { 'Referer' }
		.referrer_policy { 'Referrer-Policy' }
		.retry_after { 'Retry-After' }
		.save_data { 'Save-Data' }
		.sec_fetch_dest { 'Sec-Fetch-Dest' }
		.sec_fetch_mode { 'Sec-Fetch-Mode' }
		.sec_fetch_site { 'Sec-Fetch-Site' }
		.sec_fetch_user { 'Sec-Fetch-User' }
		.sec_websocket_accept { 'Sec-WebSocket-Accept' }
		.server { 'Server' }
		.server_timing { 'Server-Timing' }
		.set_cookie { 'Set-Cookie' }
		.sourcemap { 'SourceMap' }
		.strict_transport_security { 'Strict-Transport-Security' }
		.te { 'TE' }
		.timing_allow_origin { 'Timing-Allow-Origin' }
		.tk { 'Tk' }
		.trailer { 'Trailer' }
		.transfer_encoding { 'Transfer-Encoding' }
		.upgrade { 'Upgrade' }
		.upgrade_insecure_requests { 'Upgrade-Insecure-Requests' }
		.user_agent { 'User-Agent' }
		.vary { 'Vary' }
		.via { 'Via' }
		.want_digest { 'Want-Digest' }
		.warning { 'Warning' }
		.www_authenticate { 'WWW-Authenticate' }
		.x_content_type_options { 'X-Content-Type-Options' }
		.x_dns_prefetch_control { 'X-DNS-Prefetch-Control' }
		.x_forwarded_for { 'X-Forwarded-For' }
		.x_forwarded_host { 'X-Forwarded-Host' }
		.x_forwarded_proto { 'X-Forwarded-Proto' }
		.x_frame_options { 'X-Frame-Options' }
		.x_xss_protection { 'X-XSS-Protection' }
	}
}

const common_header_map = {
	'accept':                              CommonHeader.accept
	'accept-ch':                           .accept_ch
	'accept-charset':                      .accept_charset
	'accept-ch-lifetime':                  .accept_ch_lifetime
	'accept-encoding':                     .accept_encoding
	'accept-language':                     .accept_language
	'accept-patch':                        .accept_patch
	'accept-post':                         .accept_post
	'accept-ranges':                       .accept_ranges
	'access-control-allow-credentials':    .access_control_allow_credentials
	'access-control-allow-headers':        .access_control_allow_headers
	'access-control-allow-methods':        .access_control_allow_methods
	'access-control-allow-origin':         .access_control_allow_origin
	'access-control-expose-headers':       .access_control_expose_headers
	'access-control-max-age':              .access_control_max_age
	'access-control-request-headers':      .access_control_request_headers
	'access-control-request-method':       .access_control_request_method
	'age':                                 .age
	'allow':                               .allow
	'alt-svc':                             .alt_svc
	'authorization':                       .authorization
	'cache-control':                       .cache_control
	'clear-site-data':                     .clear_site_data
	'connection':                          .connection
	'content-disposition':                 .content_disposition
	'content-encoding':                    .content_encoding
	'content-language':                    .content_language
	'content-length':                      .content_length
	'content-location':                    .content_location
	'content-range':                       .content_range
	'content-security-policy':             .content_security_policy
	'content-security-policy-report-only': .content_security_policy_report_only
	'content-type':                        .content_type
	'cookie':                              .cookie
	'cross-origin-embedder-policy':        .cross_origin_embedder_policy
	'cross-origin-opener-policy':          .cross_origin_opener_policy
	'cross-origin-resource-policy':        .cross_origin_resource_policy
	'date':                                .date
	'device-memory':                       .device_memory
	'digest':                              .digest
	'dnt':                                 .dnt
	'early-data':                          .early_data
	'etag':                                .etag
	'expect':                              .expect
	'expect-ct':                           .expect_ct
	'expires':                             .expires
	'feature-policy':                      .feature_policy
	'forwarded':                           .forwarded
	'from':                                .from
	'host':                                .host
	'if-match':                            .if_match
	'if-modified-since':                   .if_modified_since
	'if-none-match':                       .if_none_match
	'if-range':                            .if_range
	'if-unmodified-since':                 .if_unmodified_since
	'index':                               .index
	'keep-alive':                          .keep_alive
	'large-allocation':                    .large_allocation
	'last-modified':                       .last_modified
	'link':                                .link
	'location':                            .location
	'nel':                                 .nel
	'origin':                              .origin
	'pragma':                              .pragma
	'proxy-authenticate':                  .proxy_authenticate
	'proxy-authorization':                 .proxy_authorization
	'range':                               .range
	'referer':                             .referer
	'referrer-policy':                     .referrer_policy
	'retry-after':                         .retry_after
	'save-data':                           .save_data
	'sec-fetch-dest':                      .sec_fetch_dest
	'sec-fetch-mode':                      .sec_fetch_mode
	'sec-fetch-site':                      .sec_fetch_site
	'sec-fetch-user':                      .sec_fetch_user
	'sec-websocket-accept':                .sec_websocket_accept
	'server':                              .server
	'server-timing':                       .server_timing
	'set-cookie':                          .set_cookie
	'sourcemap':                           .sourcemap
	'strict-transport-security':           .strict_transport_security
	'te':                                  .te
	'timing-allow-origin':                 .timing_allow_origin
	'tk':                                  .tk
	'trailer':                             .trailer
	'transfer-encoding':                   .transfer_encoding
	'upgrade':                             .upgrade
	'upgrade-insecure-requests':           .upgrade_insecure_requests
	'user-agent':                          .user_agent
	'vary':                                .vary
	'via':                                 .via
	'want-digest':                         .want_digest
	'warning':                             .warning
	'www-authenticate':                    .www_authenticate
	'x-content-type-options':              .x_content_type_options
	'x-dns-prefetch-control':              .x_dns_prefetch_control
	'x-forwarded-for':                     .x_forwarded_for
	'x-forwarded-host':                    .x_forwarded_host
	'x-forwarded-proto':                   .x_forwarded_proto
	'x-frame-options':                     .x_frame_options
	'x-xss-protection':                    .x_xss_protection
}

pub fn (mut h Header) free() {
	unsafe {
		h.data.free()
		h.keys.free()
	}
}

pub struct HeaderConfig {
	key   CommonHeader
	value string
}

// Create a new Header object
pub fn new_header(kvs ...HeaderConfig) Header {
	mut h := Header{
		data: map[string][]string{}
	}
	for kv in kvs {
		h.add(kv.key, kv.value)
	}
	return h
}

// new_header_from_map creates a Header from key value pairs
pub fn new_header_from_map(kvs map[CommonHeader]string) Header {
	mut h := new_header()
	h.add_map(kvs)
	return h
}

// new_custom_header_from_map creates a Header from string key value pairs
pub fn new_custom_header_from_map(kvs map[string]string) ?Header {
	mut h := new_header()
	h.add_custom_map(kvs) ?
	return h
}

// add appends a value to the header key.
pub fn (mut h Header) add(key CommonHeader, value string) {
	k := key.str()
	h.data[k] << value
	h.add_key(k)
}

// add_custom appends a value to a custom header key. This function will
// return an error if the key contains invalid header characters.
pub fn (mut h Header) add_custom(key string, value string) ? {
	is_valid(key) ?
	h.data[key] << value
	h.add_key(key)
}

// add_map appends the value for each header key.
pub fn (mut h Header) add_map(kvs map[CommonHeader]string) {
	for k, v in kvs {
		h.add(k, v)
	}
}

// add_custom_map appends the value for each custom header key.
pub fn (mut h Header) add_custom_map(kvs map[string]string) ? {
	for k, v in kvs {
		h.add_custom(k, v) ?
	}
}

// set sets the key-value pair. This function will clear any other values
// that exist for the CommonHeader.
pub fn (mut h Header) set(key CommonHeader, value string) {
	k := key.str()
	h.data[k] = [value]
	h.add_key(k)
}

// set_custom sets the key-value pair for a custom header key. This
// function will clear any other values that exist for the header. This
// function will return an error if the key contains invalid header
// characters.
pub fn (mut h Header) set_custom(key string, value string) ? {
	is_valid(key) ?
	h.data[key] = [value]
	h.add_key(key)
}

// delete deletes all values for a key.
pub fn (mut h Header) delete(key CommonHeader) {
	h.delete_custom(key.str())
}

// delete_custom deletes all values for a custom header key.
pub fn (mut h Header) delete_custom(key string) {
	h.data.delete(key)

	// remove key from keys metadata
	kl := key.to_lower()
	if kl in h.keys {
		h.keys[kl] = h.keys[kl].filter(it != key)
	}
}

[params]
pub struct HeaderCoerceConfig {
	canonicalize bool
}

// coerce coerces data in the Header by joining keys that match
// case-insensitively into one entry.
pub fn (mut h Header) coerce(flags HeaderCoerceConfig) {
	for kl, data_keys in h.keys {
		master_key := if flags.canonicalize { canonicalize(kl) } else { data_keys[0] }

		// save master data
		master_data := h.data[master_key]
		h.data.delete(master_key)

		for key in data_keys {
			if key == master_key {
				h.data[master_key] << master_data
				continue
			}
			h.data[master_key] << h.data[key]
			h.data.delete(key)
		}
		h.keys[kl] = [master_key]
	}
}

// contains returns whether the header key exists in the map.
pub fn (h Header) contains(key CommonHeader) bool {
	return h.contains_custom(key.str())
}

[params]
pub struct HeaderQueryConfig {
	exact bool
}

// contains_custom returns whether the custom header key exists in the map.
pub fn (h Header) contains_custom(key string, flags HeaderQueryConfig) bool {
	if flags.exact {
		return key in h.data
	}
	return key.to_lower() in h.keys
}

// get gets the first value for the CommonHeader, or none if the key
// does not exist.
pub fn (h Header) get(key CommonHeader) ?string {
	return h.get_custom(key.str())
}

// get_custom gets the first value for the custom header, or none if
// the key does not exist.
pub fn (h Header) get_custom(key string, flags HeaderQueryConfig) ?string {
	mut data_key := key
	if !flags.exact {
		// get the first key from key metadata
		k := key.to_lower()
		if h.keys[k].len == 0 {
			return none
		}
		data_key = h.keys[k][0]
	}
	if h.data[data_key].len == 0 {
		return none
	}
	return h.data[data_key][0]
}

// starting_with gets the first header starting with key, or none if
// the key does not exist.
pub fn (h Header) starting_with(key string) ?string {
	for k, _ in h.data {
		if k.starts_with(key) {
			return k
		}
	}
	return none
}

// values gets all values for the CommonHeader.
pub fn (h Header) values(key CommonHeader) []string {
	return h.custom_values(key.str())
}

// custom_values gets all values for the custom header.
pub fn (h Header) custom_values(key string, flags HeaderQueryConfig) []string {
	if flags.exact {
		return h.data[key]
	}
	// case insensitive lookup
	mut values := []string{cap: 10}
	for k in h.keys[key.to_lower()] {
		values << h.data[k]
	}
	return values
}

// keys gets all header keys as strings
pub fn (h Header) keys() []string {
	return h.data.keys()
}

[params]
pub struct HeaderRenderConfig {
	version      Version
	coerce       bool
	canonicalize bool
}

// render renders the Header into a string for use in sending HTTP
// requests. All header lines will end in `\r\n`
[manualfree]
pub fn (h Header) render(flags HeaderRenderConfig) string {
	// estimate ~48 bytes per header
	mut sb := strings.new_builder(h.data.len * 48)
	if flags.coerce {
		for kl, data_keys in h.keys {
			key := if flags.version == .v2_0 {
				kl
			} else if flags.canonicalize {
				canonicalize(kl)
			} else {
				data_keys[0]
			}
			for k in data_keys {
				for v in h.data[k] {
					sb.write_string(key)
					sb.write_string(': ')
					sb.write_string(v)
					sb.write_string('\r\n')
				}
			}
		}
	} else {
		for k, vs in h.data {
			key := if flags.version == .v2_0 {
				k.to_lower()
			} else if flags.canonicalize {
				canonicalize(k.to_lower())
			} else {
				k
			}
			for v in vs {
				sb.write_string(key)
				sb.write_string(': ')
				sb.write_string(v)
				sb.write_string('\r\n')
			}
		}
	}
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// join combines two Header structs into a new Header struct
pub fn (h Header) join(other Header) Header {
	mut combined := Header{
		data: h.data.clone()
		keys: h.keys.clone()
	}
	for k in other.keys() {
		for v in other.custom_values(k, exact: true) {
			combined.add_custom(k, v) or {
				// panic because this should never fail
				panic('unexpected error: $err')
			}
		}
	}
	return combined
}

// canonicalize canonicalizes an HTTP header key
// Common headers are determined by the common_header_map
// Custom headers are capitalized on the first letter and any letter after a '-'
// NOTE: Assumes sl is lowercase, since the caller usually already has the lowercase key
fn canonicalize(sl string) string {
	// check if we have a common header
	if sl in http.common_header_map {
		return http.common_header_map[sl].str()
	}
	return sl.split('-').map(it.capitalize()).join('-')
}

// Helper function to add a key to the keys map
fn (mut h Header) add_key(key string) {
	kl := key.to_lower()
	if !h.keys[kl].contains(key) {
		h.keys[kl] << key
	}
}

// Custom error struct for invalid header tokens
struct HeaderKeyError {
	Error
	code         int
	header       string
	invalid_char byte
}

pub fn (err HeaderKeyError) msg() string {
	return "Invalid header key: '$err.header'"
}

pub fn (err HeaderKeyError) code() int {
	return err.code
}

// is_valid checks if the header token contains all valid bytes
fn is_valid(header string) ? {
	for _, c in header {
		if int(c) >= 128 || !is_token(c) {
			return IError(HeaderKeyError{
				code: 1
				header: header
				invalid_char: c
			})
		}
	}
	if header.len == 0 {
		return IError(HeaderKeyError{
			code: 2
			header: header
			invalid_char: 0
		})
	}
}

// is_token checks if the byte is valid for a header token
fn is_token(b byte) bool {
	return match b {
		33, 35...39, 42, 43, 45, 46, 48...57, 65...90, 94...122, 124, 126 { true }
		else { false }
	}
}

// str returns the headers string as seen in HTTP/1.1 requests.
// Key order is not guaranteed.
pub fn (h Header) str() string {
	return h.render(version: .v1_1)
}

// parse_headers parses a newline delimited string into a Header struct
fn parse_headers(s string) ?Header {
	mut h := new_header()
	mut last_key := ''
	mut last_value := ''
	for line in s.split_into_lines() {
		if line.len == 0 {
			break
		}
		// handle header fold
		if line[0] == ` ` || line[0] == `\t` {
			last_value += ' ${line.trim(' \t')}'
			continue
		} else if last_key != '' {
			h.add_custom(last_key, last_value) ?
		}
		last_key, last_value = parse_header(line) ?
	}
	h.add_custom(last_key, last_value) ?
	return h
}

fn parse_header(s string) ?(string, string) {
	if !s.contains(':') {
		return error('missing colon in header')
	}
	words := s.split_nth(':', 2)
	// TODO: parse quoted text according to the RFC
	return words[0], words[1].trim(' \t')
}
