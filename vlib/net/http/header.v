// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

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

const common_header_map = map{
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

// Header represents the key-value pairs in an HTTP header
[noinit]
pub struct Header {
mut:
	data map[string][]string
}

pub fn (mut h Header) free() {
	unsafe {
		h.data.free()
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

// Append a value to the header key.
pub fn (mut h Header) add(key CommonHeader, value string) {
	h.data[key.str()] << value
}

// Append a value to a custom header key. This function will return an error
// if the key contains invalid header characters.
pub fn (mut h Header) add_str(key string, value string) ? {
	k := canonicalize(key) ?
	h.data[k] << value
}

// Sets the key-value pair. This function will clear any other values
// that exist for the CommonHeader.
pub fn (mut h Header) set(key CommonHeader, value string) {
	h.data[key.str()] = [value]
}

// Sets the key-value pair for a custom header key. This function will
// clear any other values that exist for the CommonHeader.
pub fn (mut h Header) set_str(key string, value string) {
	k := canonicalize(key) or { return }
	h.data[k] = [value]
}

// Delete all values for a key.
pub fn (mut h Header) delete(key CommonHeader) {
	h.data.delete(key.str())
}

// Delete all values for a custom header key.
pub fn (mut h Header) delete_str(key string) {
	k := canonicalize(key) or { return }
	h.data.delete(k)
}

// Returns whether the header key exists in the map.
pub fn (h Header) contains(key CommonHeader) bool {
	return key.str() in h.data
}

// Returns whether the custom header key exists in the map.
pub fn (h Header) contains_str(key string) bool {
	k := canonicalize(key) or { return false }
	return k in h.data
}

// Gets the first value for the CommonHeader, or none if the key does
// not exist.
pub fn (h Header) get(key CommonHeader) ?string {
	k := key.str()
	if h.data[k].len == 0 {
		return none
	}
	return h.data[k][0]
}

// Gets the first value for the custom header, or none if the key does
// not exist.
pub fn (h Header) get_str(key string) ?string {
	k := canonicalize(key) or { return none }
	if h.data[k].len == 0 {
		return none
	}
	return h.data[k][0]
}

// Gets all values for the CommonHeader.
pub fn (h Header) values(key CommonHeader) []string {
	return h.data[key.str()]
}

// Gets all values for the custom header.
pub fn (h Header) values_str(key string) []string {
	k := canonicalize(key) or { return [] }
	return h.data[k]
}

// Gets all header keys as strings
pub fn (h Header) keys() []string {
	return h.data.keys()
}

// Validate and canonicalize an HTTP header key
// A canonical header is all lowercase except for the first character
// and any character after a `-`. Example: `Example-Header-Key`
// There are some exceptions like `DNT`, `WWW-Authenticate`, etc. For these we
// check if the lowercase matches any in the common_header_map and return that.
fn canonicalize(s string) ?string {
	// check for valid header bytes
	for _, c in s {
		if int(c) >= 128 || !is_token(c) {
			return error('Invalid header key')
		}
	}

	// check if we have a common header
	sl := s.to_lower()
	if sl in http.common_header_map {
		return http.common_header_map[sl].str()
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
