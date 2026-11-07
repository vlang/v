module sasl

// The two examples of RFC 4616, section 4.
const rfc_example_1 = '\x00tim\x00tanstaaftanstaaf'
const rfc_example_2 = 'Ursel\x00Kurt\x00xipj3plmq'

const invalid_utf8 = [u8(0x66), 0xff, 0x6f].bytestr()

// A UTF-16 surrogate encoded as UTF-8, which RFC 4616 excludes.
const encoded_surrogate = [u8(0xed), 0xa0, 0x80].bytestr()

fn test_encode_plain_matches_the_rfc_examples() {
	assert encode_plain(authcid: 'tim', password: 'tanstaaftanstaaf')! == rfc_example_1
	assert encode_plain(authzid: 'Ursel', authcid: 'Kurt', password: 'xipj3plmq')! == rfc_example_2
}

fn test_parse_plain_reads_the_rfc_examples() {
	m1 := parse_plain(rfc_example_1)!
	assert m1.authzid == ''
	assert m1.authcid == 'tim'
	assert m1.password == 'tanstaaftanstaaf'
	m2 := parse_plain(rfc_example_2)!
	assert m2.authzid == 'Ursel'
	assert m2.authcid == 'Kurt'
	assert m2.password == 'xipj3plmq'
}

fn test_plain_round_trips_utf8_and_long_fields() {
	long := 'é'.repeat(200)
	for m in [
		PlainMessage{
			authzid:  'Zoë'
			authcid:  'José'
			password: 'pässwörd 🔑'
		},
		PlainMessage{
			authzid:  'a'.repeat(255)
			authcid:  'b'.repeat(255)
			password: 'c'.repeat(255)
		},
		PlainMessage{
			authcid:  long
			password: long
		},
	] {
		parsed := parse_plain(encode_plain(m)!)!
		assert parsed.authzid == m.authzid
		assert parsed.authcid == m.authcid
		assert parsed.password == m.password
	}
}

fn test_encode_plain_rejects_invalid_fields() {
	cases := {
		'empty authcid':                 PlainMessage{
			password: 'secret'
		}
		'empty password':                PlainMessage{
			authcid: 'bob'
		}
		'NUL in authzid':                PlainMessage{
			authzid:  'al\x00ice'
			authcid:  'bob'
			password: 'secret'
		}
		'NUL in authcid':                PlainMessage{
			authcid:  'bo\x00b'
			password: 'secret'
		}
		'NUL in password':               PlainMessage{
			authcid:  'bob'
			password: 'sec\x00ret'
		}
		'invalid UTF-8 in authzid':      PlainMessage{
			authzid:  invalid_utf8
			authcid:  'bob'
			password: 'secret'
		}
		'invalid UTF-8 in authcid':      PlainMessage{
			authcid:  invalid_utf8
			password: 'secret'
		}
		'invalid UTF-8 in password':     PlainMessage{
			authcid:  'bob'
			password: invalid_utf8
		}
		'encoded surrogate in password': PlainMessage{
			authcid:  'bob'
			password: encoded_surrogate
		}
	}
	for name, m in cases {
		if encoded := encode_plain(m) {
			assert false, '${name}: encoded as ${encoded.bytes()}'
		} else {
			assert err.msg().starts_with('sasl: '), name
		}
	}
}

fn test_parse_plain_rejects_malformed_messages() {
	cases := {
		'empty':                    ''
		'no separator':             'bobsecret'
		'one separator':            '\x00bobsecret'
		'three separators':         '\x00bob\x00sec\x00ret'
		'empty authcid':            'alice\x00\x00secret'
		'empty password':           '\x00bob\x00'
		'only separators':          '\x00\x00'
		'invalid UTF-8':            '\x00bob\x00sec${invalid_utf8}'
		'encoded surrogate':        '\x00bob\x00${encoded_surrogate}'
		'invalid UTF-8 in authzid': '${invalid_utf8}\x00bob\x00secret'
	}
	for name, message in cases {
		if m := parse_plain(message) {
			assert false, '${name}: parsed as ${m}'
		} else {
			assert err.msg().starts_with('sasl: '), name
			assert !err.msg().contains('secret'), name
		}
	}
}

fn test_plain_message_str_redacts_the_password() {
	m := parse_plain('alice\x00bob\x00hunter2')!
	s := m.str()
	assert s.contains("authzid: 'alice'")
	assert s.contains("authcid: 'bob'")
	assert !s.contains('hunter2')
	assert !'${m}'.contains('hunter2')
}
