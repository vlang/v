module http

fn test_str() {
	code := Status.bad_gateway
	actual := code.str()
	assert actual == 'Bad Gateway'
}

fn test_int() {
	code := Status.see_other
	actual := code.int()
	assert actual == 303
}

fn test_is_valid() {
	code := Status.gateway_timeout
	actual := code.is_valid()
	assert actual == true
}

fn test_is_valid_negative() {
	code := Status.unassigned
	actual := code.is_valid()
	assert actual == false
}

fn test_is_error() {
	code := Status.too_many_requests
	actual := code.is_error()
	assert actual == true
}

fn test_is_error_negative() {
	code := Status.cont
	actual := code.is_error()
	assert actual == false
}

fn test_is_success() {
	code := Status.accepted
	actual := code.is_success()
	assert actual == true
}

fn test_is_success_negative() {
	code := Status.forbidden
	actual := code.is_success()
	assert actual == false
}

fn test_standard_reason_phrases_for_every_status_code() {
	// Standard phrases from Go's net/http StatusText, including empty unknown values.
	expected := {
		100: 'Continue'
		101: 'Switching Protocols'
		102: 'Processing'
		103: 'Early Hints'
		200: 'OK'
		201: 'Created'
		202: 'Accepted'
		203: 'Non-Authoritative Information'
		204: 'No Content'
		205: 'Reset Content'
		206: 'Partial Content'
		207: 'Multi-Status'
		208: 'Already Reported'
		226: 'IM Used'
		300: 'Multiple Choices'
		301: 'Moved Permanently'
		302: 'Found'
		303: 'See Other'
		304: 'Not Modified'
		305: 'Use Proxy'
		307: 'Temporary Redirect'
		308: 'Permanent Redirect'
		400: 'Bad Request'
		401: 'Unauthorized'
		402: 'Payment Required'
		403: 'Forbidden'
		404: 'Not Found'
		405: 'Method Not Allowed'
		406: 'Not Acceptable'
		407: 'Proxy Authentication Required'
		408: 'Request Timeout'
		409: 'Conflict'
		410: 'Gone'
		411: 'Length Required'
		412: 'Precondition Failed'
		413: 'Request Entity Too Large'
		414: 'Request URI Too Long'
		415: 'Unsupported Media Type'
		416: 'Requested Range Not Satisfiable'
		417: 'Expectation Failed'
		418: "I'm a teapot"
		421: 'Misdirected Request'
		422: 'Unprocessable Entity'
		423: 'Locked'
		424: 'Failed Dependency'
		425: 'Too Early'
		426: 'Upgrade Required'
		428: 'Precondition Required'
		429: 'Too Many Requests'
		431: 'Request Header Fields Too Large'
		451: 'Unavailable For Legal Reasons'
		500: 'Internal Server Error'
		501: 'Not Implemented'
		502: 'Bad Gateway'
		503: 'Service Unavailable'
		504: 'Gateway Timeout'
		505: 'HTTP Version Not Supported'
		506: 'Variant Also Negotiates'
		507: 'Insufficient Storage'
		508: 'Loop Detected'
		510: 'Not Extended'
		511: 'Network Authentication Required'
	}
	for code in 100 .. 600 {
		assert status_from_int(code).str() == expected[code]
	}
	for code in [-1, 0, 99, 600, 999] {
		assert status_from_int(code).str() == ''
	}
	assert Status.unknown.str() == ''
	assert Status.unassigned.str() == ''
}
