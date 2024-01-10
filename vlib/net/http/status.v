// Copyright (c) 2020 Justin E. Jones. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// The status codes listed here are based on the comprehensive list,
// available at:
// https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
pub enum Status {
	unknown                         = -1
	unassigned                      = 0
	cont                            = 100
	switching_protocols             = 101
	processing                      = 102
	checkpoint_draft                = 103
	ok                              = 200
	created                         = 201
	accepted                        = 202
	non_authoritative_information   = 203
	no_content                      = 204
	reset_content                   = 205
	partial_content                 = 206
	multi_status                    = 207
	already_reported                = 208
	im_used                         = 226
	multiple_choices                = 300
	moved_permanently               = 301
	found                           = 302
	see_other                       = 303
	not_modified                    = 304
	use_proxy                       = 305
	switch_proxy                    = 306
	temporary_redirect              = 307
	permanent_redirect              = 308
	bad_request                     = 400
	unauthorized                    = 401
	payment_required                = 402
	forbidden                       = 403
	not_found                       = 404
	method_not_allowed              = 405
	not_acceptable                  = 406
	proxy_authentication_required   = 407
	request_timeout                 = 408
	conflict                        = 409
	gone                            = 410
	length_required                 = 411
	precondition_failed             = 412
	request_entity_too_large        = 413
	request_uri_too_long            = 414
	unsupported_media_type          = 415
	requested_range_not_satisfiable = 416
	expectation_failed              = 417
	im_a_teapot                     = 418
	misdirected_request             = 421
	unprocessable_entity            = 422
	locked                          = 423
	failed_dependency               = 424
	unordered_collection            = 425
	upgrade_required                = 426
	precondition_required           = 428
	too_many_requests               = 429
	request_header_fields_too_large = 431
	unavailable_for_legal_reasons   = 451
	client_closed_request           = 499
	internal_server_error           = 500
	not_implemented                 = 501
	bad_gateway                     = 502
	service_unavailable             = 503
	gateway_timeout                 = 504
	http_version_not_supported      = 505
	variant_also_negotiates         = 506
	insufficient_storage            = 507
	loop_detected                   = 508
	bandwidth_limit_exceeded        = 509
	not_extended                    = 510
	network_authentication_required = 511
}

// status_from_int returns the corresponding enum field of Status
// given the `code` in integer value.
pub fn status_from_int(code int) Status {
	return match code {
		100 { Status.cont }
		101 { Status.switching_protocols }
		102 { Status.processing }
		103 { Status.checkpoint_draft }
		104...199 { Status.unassigned }
		200 { Status.ok }
		201 { Status.created }
		202 { Status.accepted }
		203 { Status.non_authoritative_information }
		204 { Status.no_content }
		205 { Status.reset_content }
		206 { Status.partial_content }
		207 { Status.multi_status }
		208 { Status.already_reported }
		209...225 { Status.unassigned }
		226 { Status.im_used }
		227...299 { Status.unassigned }
		300 { Status.multiple_choices }
		301 { Status.moved_permanently }
		302 { Status.found }
		303 { Status.see_other }
		304 { Status.not_modified }
		305 { Status.use_proxy }
		306 { Status.switch_proxy }
		307 { Status.temporary_redirect }
		308 { Status.permanent_redirect }
		309...399 { Status.unassigned }
		400 { Status.bad_request }
		401 { Status.unauthorized }
		402 { Status.payment_required }
		403 { Status.forbidden }
		404 { Status.not_found }
		405 { Status.method_not_allowed }
		406 { Status.not_acceptable }
		407 { Status.proxy_authentication_required }
		408 { Status.request_timeout }
		409 { Status.conflict }
		410 { Status.gone }
		411 { Status.length_required }
		412 { Status.precondition_failed }
		413 { Status.request_entity_too_large }
		414 { Status.request_uri_too_long }
		415 { Status.unsupported_media_type }
		416 { Status.requested_range_not_satisfiable }
		417 { Status.expectation_failed }
		418 { Status.im_a_teapot }
		419...420 { Status.unassigned }
		421 { Status.misdirected_request }
		422 { Status.unprocessable_entity }
		423 { Status.locked }
		424 { Status.failed_dependency }
		425 { Status.unordered_collection }
		426 { Status.upgrade_required }
		428 { Status.precondition_required }
		429 { Status.too_many_requests }
		431 { Status.request_header_fields_too_large }
		432...450 { Status.unassigned }
		451 { Status.unavailable_for_legal_reasons }
		452...499 { Status.unassigned }
		500 { Status.internal_server_error }
		501 { Status.not_implemented }
		502 { Status.bad_gateway }
		503 { Status.service_unavailable }
		504 { Status.gateway_timeout }
		505 { Status.http_version_not_supported }
		506 { Status.variant_also_negotiates }
		507 { Status.insufficient_storage }
		508 { Status.loop_detected }
		509 { Status.bandwidth_limit_exceeded }
		510 { Status.not_extended }
		511 { Status.network_authentication_required }
		512...599 { Status.unassigned }
		else { Status.unknown }
	}
}

// str returns the string representation of Status `code`.
pub fn (code Status) str() string {
	return match code {
		.cont { 'Continue' }
		.switching_protocols { 'Switching Protocols' }
		.processing { 'Processing' }
		.checkpoint_draft { 'Checkpoint Draft' }
		.ok { 'OK' }
		.created { 'Created' }
		.accepted { 'Accepted' }
		.non_authoritative_information { 'Non Authoritative Information' }
		.no_content { 'No Content' }
		.reset_content { 'Reset Content' }
		.partial_content { 'Partial Content' }
		.multi_status { 'Multi Status' }
		.already_reported { 'Already Reported' }
		.im_used { 'IM Used' }
		.multiple_choices { 'Multiple Choices' }
		.moved_permanently { 'Moved Permanently' }
		.found { 'Found' }
		.see_other { 'See Other' }
		.not_modified { 'Not Modified' }
		.use_proxy { 'Use Proxy' }
		.switch_proxy { 'Switch Proxy' }
		.temporary_redirect { 'Temporary Redirect' }
		.permanent_redirect { 'Permanent Redirect' }
		.bad_request { 'Bad Request' }
		.unauthorized { 'Unauthorized' }
		.payment_required { 'Payment Required' }
		.forbidden { 'Forbidden' }
		.not_found { 'Not Found' }
		.method_not_allowed { 'Method Not Allowed' }
		.not_acceptable { 'Not Acceptable' }
		.proxy_authentication_required { 'Proxy Authentication Required' }
		.request_timeout { 'Request Timeout' }
		.conflict { 'Conflict' }
		.gone { 'Gone' }
		.length_required { 'Length Required' }
		.precondition_failed { 'Precondition Failed' }
		.request_entity_too_large { 'Request Entity Too Large' }
		.request_uri_too_long { 'Request URI Too Long' }
		.unsupported_media_type { 'Unsupported Media Type' }
		.requested_range_not_satisfiable { 'Requested Range Not Satisfiable' }
		.expectation_failed { 'Expectation Failed' }
		.im_a_teapot { 'Im a teapot' }
		.misdirected_request { 'Misdirected Request' }
		.unprocessable_entity { 'Unprocessable Entity' }
		.locked { 'Locked' }
		.failed_dependency { 'Failed Dependency' }
		.unordered_collection { 'Unordered Collection' }
		.upgrade_required { 'Upgrade Required' }
		.precondition_required { 'Precondition Required' }
		.too_many_requests { 'Too Many Requests' }
		.request_header_fields_too_large { 'Request Header Fields Too Large' }
		.unavailable_for_legal_reasons { 'Unavailable For Legal Reasons' }
		.internal_server_error { 'Internal Server Error' }
		.not_implemented { 'Not Implemented' }
		.bad_gateway { 'Bad Gateway' }
		.service_unavailable { 'Service Unavailable' }
		.gateway_timeout { 'Gateway Timeout' }
		.http_version_not_supported { 'HTTP Version Not Supported' }
		.variant_also_negotiates { 'Variant Also Negotiates' }
		.insufficient_storage { 'Insufficient Storage' }
		.loop_detected { 'Loop Detected' }
		.bandwidth_limit_exceeded { 'Bandwidth Limit Exceeded' }
		.not_extended { 'Not Extended' }
		.network_authentication_required { 'Network Authentication Required' }
		.unassigned { 'Unassigned' }
		else { 'Unknown' }
	}
}

// int converts an assigned and known Status to its integral equivalent.
// if a Status is unknown or unassigned, this method will return zero
pub fn (code Status) int() int {
	if code in [.unknown, .unassigned] {
		return 0
	}
	return int(code)
}

// is_valid returns true if the status code is assigned and known
pub fn (code Status) is_valid() bool {
	number := code.int()
	return number >= 100 && number < 600
}

// is_error will return true if the status code represents either a client or
// a server error; otherwise will return false
pub fn (code Status) is_error() bool {
	number := code.int()
	return number >= 400 && number < 600
}

// is_success will return true if the status code represents either an
// informational, success, or redirection response; otherwise will return false
pub fn (code Status) is_success() bool {
	number := code.int()
	return number >= 100 && number < 400
}
