module grpc

fn test_code_from_int() {
	assert code_from_int(0)! == .ok
	assert code_from_int(12)! == .unimplemented
	assert code_from_int(16)! == .unauthenticated
	for bad in [-1, 17, 255] {
		if _ := code_from_int(bad) {
			assert false, 'code ${bad} must error'
		}
	}
}

fn test_status_ok() {
	assert Status{}.is_ok()
	assert !Status{
		code:    .internal
		message: 'boom'
	}.is_ok()
}

fn test_status_error_msg() {
	assert (StatusError{
		status: Status{
			code:    .not_found
			message: 'gone'
		}
	}).msg() == 'grpc: not_found: gone'
	// empty message: no trailing separator
	assert (StatusError{
		status: Status{
			code: .not_found
		}
	}).msg() == 'grpc: not_found'
}

fn test_percent_encode() {
	assert percent_encode('hello world') == 'hello world'
	assert percent_encode('a%b') == 'a%25b' // literal percent escaped
	assert percent_encode('\n') == '%0A' // control byte, uppercase hex
	assert percent_encode('é') == '%C3%A9' // non-ASCII utf-8 bytes
}

fn test_percent_decode() {
	assert percent_decode('A') == 'A'
	assert percent_decode('%41') == 'A'
	assert percent_decode('%0a') == '\n' // lowercase hex accepted
	assert percent_decode('%C3%A9') == 'é'
	// malformed escapes pass through untouched (spec: lenient decode)
	assert percent_decode('100%') == '100%' // trailing percent
	assert percent_decode('%2') == '%2' // truncated escape
	assert percent_decode('%GG') == '%GG' // non-hex digits
}

fn test_percent_roundtrip() {
	inputs := ['', 'hello world', 'a%b%%c', 'tab\tnl\n', 'café ☃ résumé', '100% sure', '\x00\x01\x1f\x7f\x80\xfe\xff']
	for s in inputs {
		assert percent_decode(percent_encode(s)) == s
	}
}
