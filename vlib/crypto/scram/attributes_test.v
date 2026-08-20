// Unit tests for the message grammar helpers of RFC 5802 §7.
module scram

fn test_escape_saslname_escapes_only_what_the_grammar_forbids() {
	assert escape_saslname('user') == 'user'
	assert escape_saslname('') == ''
	assert escape_saslname('a,b') == 'a=2Cb'
	assert escape_saslname('a=b') == 'a=3Db'
	assert escape_saslname('a,b=c') == 'a=2Cb=3Dc'
	assert escape_saslname(',') == '=2C'
	assert escape_saslname('=') == '=3D'
	// An equals sign must be escaped before a comma, otherwise the `=` of the
	// `=2C` produced for the comma would be escaped in turn.
	assert escape_saslname('=,') == '=3D=2C'
	assert escape_saslname('=2C') == '=3D2C'
	// Non-ASCII passes through: the grammar is defined over UTF-8 octets.
	assert escape_saslname('rené') == 'rené'
}

fn test_unescape_saslname_reverses_escape_saslname() {
	for name in ['user', '', 'a,b', 'a=b', 'a,b=c', ',', '=', '=,', '=2C', 'rené', '=3D=2C=3D'] {
		assert unescape_saslname(escape_saslname(name))! == name, name
	}
}

fn test_unescape_saslname_refuses_anything_it_did_not_produce() {
	cases := {
		'an unknown escape':           'a=FFb'
		'a lowercase escape':          'a=2cb'
		'a truncated escape':          'ab='
		'a nearly truncated one':      'ab=2'
		'a raw comma':                 'a,b'
		'a raw comma after an escape': '=2C,'
	}
	for what, name in cases {
		unescape_saslname(name) or {
			assert err is MalformedMessage, what
			continue
		}
		assert false, 'accepted ${what}: `${name}`'
	}
}

fn test_parse_attributes_splits_a_well_formed_message() {
	attrs := parse_attributes('r=abc,s=c2FsdA==,i=4096')!
	assert attrs.len == 3
	assert attrs[0].key == `r` && attrs[0].value == 'abc'
	assert attrs[1].key == `s` && attrs[1].value == 'c2FsdA=='
	assert attrs[2].key == `i` && attrs[2].value == '4096'
}

fn test_parse_attributes_keeps_equals_signs_inside_values() {
	// Base64 padding lives in the value, so only the first `=` separates.
	attrs := parse_attributes('p=dGVzdA==')!
	assert attrs.len == 1
	assert attrs[0].value == 'dGVzdA=='
}

fn test_parse_attributes_refuses_malformed_input() {
	cases := {
		'an empty message':       ''
		'no equals sign':         'abc'
		'an empty value':         'r='
		'a long name':            'rr=abc'
		'a digit as a name':      '1=abc'
		'a punctuation name':     '-=abc'
		'an empty trailing part': 'r=abc,'
		'an empty leading part':  ',r=abc'
	}
	for what, message in cases {
		parse_attributes(message) or {
			assert err is MalformedMessage, what
			continue
		}
		assert false, 'accepted ${what}: `${message}`'
	}
}

fn test_find_returns_the_first_matching_attribute() {
	attrs := parse_attributes('r=one,s=two,r=three')!
	assert attrs.find(`r`)? == 'one'
	assert attrs.find(`s`)? == 'two'
	assert attrs.find(`z`) == none
}

fn test_parse_positive_int_follows_the_posit_number_rule() {
	assert parse_positive_int('1', 'n')! == 1
	assert parse_positive_int('4096', 'n')! == 4096
	assert parse_positive_int('999999999', 'n')! == 999999999
}

fn test_parse_positive_int_refuses_anything_lenient() {
	cases := {
		'empty':            ''
		'a leading zero':   '04096'
		'just zero':        '0'
		'a plus sign':      '+1'
		'a minus sign':     '-1'
		'a space':          ' 1'
		'a trailing space': '1 '
		'a float':          '1.5'
		'hexadecimal':      '0x10'
		'letters':          'lots'
		'too many digits':  '1234567890'
	}
	for what, value in cases {
		parse_positive_int(value, 'iteration count') or {
			assert err is MalformedMessage, what
			continue
		}
		assert false, 'accepted ${what}: `${value}`'
	}
}

fn test_decode_base64_refuses_non_canonical_encodings() {
	assert decode_base64('', 'x')! == []u8{}
	assert decode_base64('dGVzdA==', 'x')! == 'test'.bytes()
	for value in ['dGVzdA', 'dGVzdA=', 'dGVzdA===', 'not!base64', 'dGVz dA==', '===='] {
		decode_base64(value, 'the salt') or {
			assert err.msg() == 'scram: malformed message: the salt is not valid base64'
			assert err is MalformedMessage, value
			continue
		}
		assert false, 'accepted the non-canonical base64 `${value}`'
	}
}

fn test_gs2_header_rendering() {
	none_binding := ChannelBinding{}
	assert none_binding.gs2_header('')! == 'n,,'
	assert none_binding.gs2_header('admin')! == 'n,a=admin,'
	assert none_binding.gs2_header('a,b')! == 'n,a=a=2Cb,'

	downgraded := ChannelBinding{
		mode: .unsupported_by_server
	}
	assert downgraded.gs2_header('')! == 'y,,'

	bound := ChannelBinding{
		mode: .required
		name: 'tls-server-end-point'
		data: [u8(1), 2, 3]
	}
	assert bound.gs2_header('')! == 'p=tls-server-end-point,,'
	assert bound.gs2_header('admin')! == 'p=tls-server-end-point,a=admin,'
}

fn test_gs2_header_refuses_an_unusable_binding_name() {
	for name in ['', 'has,comma', 'has=equals'] {
		binding := ChannelBinding{
			mode: .required
			name: name
			data: [u8(1)]
		}
		binding.gs2_header('') or { continue }
		assert false, 'accepted the channel binding name `${name}`'
	}
}

fn test_cbind_input_appends_data_only_when_bound() {
	none_binding := ChannelBinding{}
	assert none_binding.cbind_input('n,,') == 'n,,'.bytes()

	// Data set but not bound: it must be ignored, not silently mixed in.
	unused := ChannelBinding{
		mode: .unsupported_by_server
		data: [u8(9), 9]
	}
	assert unused.cbind_input('y,,') == 'y,,'.bytes()

	bound := ChannelBinding{
		mode: .required
		name: 'tls-exporter'
		data: [u8(1), 2, 3]
	}
	mut expected := 'p=tls-exporter,,'.bytes()
	expected << [u8(1), 2, 3]
	assert bound.cbind_input('p=tls-exporter,,') == expected
}

fn test_split_gs2_header_separates_the_three_parts() {
	header, authzid, bare := split_gs2_header('n,,n=user,r=abc')!
	assert header == 'n,,'
	assert authzid == ''
	assert bare == 'n=user,r=abc'

	header2, authzid2, bare2 := split_gs2_header('y,a=admin,n=user,r=abc')!
	assert header2 == 'y,a=admin,'
	assert authzid2 == 'admin'
	assert bare2 == 'n=user,r=abc'

	header3, authzid3, bare3 := split_gs2_header('p=tls-exporter,a=a=2Cb,n=user,r=abc')!
	assert header3 == 'p=tls-exporter,a=a=2Cb,'
	assert authzid3 == 'a,b'
	assert bare3 == 'n=user,r=abc'
}

fn test_split_gs2_header_refuses_a_broken_header() {
	for message in ['', 'n', 'n,', 'nn=user,r=abc', 'n,zz,n=user'] {
		split_gs2_header(message) or {
			assert err is MalformedMessage, message
			continue
		}
		assert false, 'accepted the GS2 header of `${message}`'
	}
}

fn test_validate_nonce_matches_the_printable_rule() {
	// RFC 5802 §7: printable is 0x21-0x7E minus the comma.
	validate_nonce('!')!
	validate_nonce('~')!
	validate_nonce('rOprNGfwEbeRWgbNEkqO')!
	validate_nonce('%hvYDpWUa2RaTCAfuxFIlj)hNlF\$k0')!
	validate_nonce('a+b/c=')!
	for bad in ['', ' ', 'a,b', 'a b', 'tab\there', 'nul\0here', 'newline\n', 'é'] {
		validate_nonce(bad) or { continue }
		assert false, 'accepted the nonce `${bad}`'
	}
}
