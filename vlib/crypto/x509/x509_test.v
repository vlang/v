module x509

fn testdata_pem() string {
	return $embed_file('testdata/cert.pem').to_string()
}

fn testdata_der() []u8 {
	return $embed_file('testdata/cert.der').to_bytes()
}

fn test_parse_pem_real_cert() {
	cert := parse_pem(testdata_pem())!
	assert cert.version == 3
	assert cert.common_name() == 'mail.example.com'
	assert cert.organization() == 'Example Org'
	assert cert.dns_names == ['mail.example.com', 'www.example.com']
	assert cert.not_before.year == 2026
	assert cert.not_after.year == 2036
	assert cert.not_before.unix() < cert.not_after.unix()
	assert cert.serial.len > 0
	assert cert.raw.len == 886
}

fn test_parse_der_matches_pem() {
	pem_cert := parse_pem(testdata_pem())!
	der_cert := parse_der(testdata_der())!
	assert der_cert.common_name() == pem_cert.common_name()
	assert der_cert.dns_names == pem_cert.dns_names
	assert der_cert.serial == pem_cert.serial
}

fn test_check_host() {
	cert := parse_pem(testdata_pem())!
	for name in ['mail.example.com', 'www.example.com', 'MAIL.EXAMPLE.COM', 'mail.example.com.'] {
		cert.check_host(name)!
	}
	for name in ['other.example.com', 'sub.mail.example.com'] {
		if _ := cert.check_host(name) {
			assert false, 'non-SAN name must fail closed: ${name}'
		} else {
			assert err.msg().contains('not covered by SAN')
		}
	}
}

fn test_check_host_cn_fallback() {
	// wildcard semantics are unit-tested directly: dns_match is the whole rule.
	assert dns_match('*.example.com', 'a.example.com')
	assert dns_match('*.example.com', 'A.EXAMPLE.COM')
	assert !dns_match('*.example.com', 'a.b.example.com')
	assert !dns_match('*.example.com', 'example.com')
	assert !dns_match('*.example.com', 'aexample.com')
	assert dns_match('exact.example.com', 'exact.example.com')
	assert !dns_match('exact.example.com', 'other.example.com')
}

fn assert_bad_der(data []u8) {
	if _ := parse_der(data) {
		assert false, 'malformed certificate must error'
	}
}

fn test_parse_errors() {
	assert_bad_der([]u8{})
	assert_bad_der('not a cert'.bytes())
	for input in ['no blocks here', '-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n'] {
		if _ := parse_pem(input) {
			assert false
		}
	}
	der := testdata_der()
	for n in 0 .. der.len {
		assert_bad_der(der[..n])
	}
	assert_bad_der([u8(0x30), 0x84, 0xff, 0xff, 0xff, 0xff])
	assert_bad_der([u8(0x30), 0x80, 0, 0])
	assert_bad_der([u8(0x30), 0x81, 0])
	mut trailing := der.clone()
	trailing << u8(0)
	assert_bad_der(trailing)
	body := expect_tag(der, 0, tag_seq, 'certificate')!
	tbs, after_tbs := split_tlv(body, 0)!
	assert_bad_der(encode_test_tlv(tag_seq, tbs)) // required outer signature fields missing
	algorithm, _ := split_tlv(body, after_tbs)!
	mut mismatched := body.clone()
	// RSA signature OID's final arc differs from the TBS algorithm.
	mismatched[after_tbs + algorithm.len - 3] ^= 1
	assert_bad_der(encode_test_tlv(tag_seq, mismatched))
}

fn encode_test_tlv(tag u8, body []u8) []u8 {
	mut out := [tag]
	if body.len < 128 {
		out << u8(body.len)
	} else if body.len < 256 {
		out << [u8(0x81), u8(body.len)]
	} else {
		out << [u8(0x82), u8(body.len >> 8), u8(body.len)]
	}
	out << body
	return out
}

fn test_optional_unique_ids_do_not_hide_san() {
	der := testdata_der()
	body := expect_tag(der, 0, tag_seq, 'certificate')!
	tbs := expect_tag(body, 0, tag_seq, 'TBS')!
	mut pos := 0
	for _ in 0 .. 7 {
		pos = skip_tlv(tbs, pos)!
	}
	assert tbs[pos] == tag_ctx3
	mut modified := tbs[..pos].clone()
	modified << encode_test_tlv(0x81, [u8(0), 0x55])
	modified << encode_test_tlv(0x82, [u8(0), 0xaa])
	modified << tbs[pos..]
	mut cert_body := encode_test_tlv(tag_seq, modified)
	cert_body << body[skip_tlv(body, 0)!..]
	cert := parse_der(encode_test_tlv(tag_seq, cert_body))!
	assert cert.dns_names == ['mail.example.com', 'www.example.com']
	cert.check_host('www.example.com')!
	if _ := cert.check_host('other.example.com') {
		assert false
	}
}

fn test_hostname_identity_policy() {
	legacy := Certificate{
		subject: {
			oid_common_name: '*.example.com'
		}
	}
	legacy.check_host('a.example.com')!
	for invalid in ['', 'a.b.example.com', 'example.com', 'a..example.com', 'a*.example.com', '1.2.3.4',
		'::1', 'bad_name.example.com'] {
		if _ := legacy.check_host(invalid) {
			assert false, invalid
		}
	}
	with_san := Certificate{
		subject:   {
			oid_common_name: 'mail.example.com'
		}
		dns_names: ['www.example.com']
	}
	if _ := with_san.check_host('mail.example.com') {
		assert false
	}
	nondns_san := Certificate{
		subject: {
			oid_common_name: 'mail.example.com'
		}
		has_san: true
	}
	if _ := nondns_san.check_host('mail.example.com') {
		assert false
	}
	empty := Certificate{
		subject: {
			oid_common_name: ''
		}
	}
	if _ := empty.check_host('') {
		assert false
	}
	ip := Certificate{
		subject: {
			oid_common_name: '127.0.0.1'
		}
	}
	if _ := ip.check_host('127.0.0.1') {
		assert false
	}
}

fn test_san_without_dns_prevents_common_name_fallback() {
	der := testdata_der()
	body := expect_tag(der, 0, tag_seq, 'certificate')!
	mut tbs := expect_tag(body, 0, tag_seq, 'TBS')!.clone()
	// The fixture's dNSNames are contiguous raw IA5 strings; change their tag to rfc822Name.
	for i in 0 .. tbs.len - 2 {
		if tbs[i] == 0x82 && tbs[i + 1] == 16 && tbs[i + 2] == `m` {
			tbs[i] = 0x81
		}
		if tbs[i] == 0x82 && tbs[i + 1] == 15 && tbs[i + 2] == `w` {
			tbs[i] = 0x81
		}
	}
	mut modified := encode_test_tlv(tag_seq, tbs)
	modified << body[skip_tlv(body, 0)!..]
	cert := parse_der(encode_test_tlv(tag_seq, modified))!
	assert cert.has_san
	assert cert.dns_names.len == 0
	if _ := cert.check_host('mail.example.com') {
		assert false
	}
}

fn test_strict_time_fields() {
	for valid in ['260101000000Z', '500101000000Z', '491231235959Z'] {
		_ := parse_time(encode_test_tlv(tag_utctime, valid.bytes()))!
	}
	assert parse_time(encode_test_tlv(tag_gentime, '20240229000000Z'.bytes()))!.day == 29
	for invalid in ['260001000000Z', '261301000000Z', '260230000000Z', '260101240000Z', '260101006000Z',
		'260101000060Z', '2x0101000000Z', '2601010000000Z', '260101000000+0000'] {
		if _ := parse_time(encode_test_tlv(tag_utctime, invalid.bytes())) {
			assert false, invalid
		}
	}
	if _ := parse_time(encode_test_tlv(tag_gentime, '20230229000000Z'.bytes())) {
		assert false
	}
	mut extra := encode_test_tlv(tag_utctime, '260101000000Z'.bytes())
	extra << u8(0)
	if _ := parse_time(extra) {
		assert false
	}
}

fn test_multivalued_rdn_bmpstring_and_oids() {
	mut cn := encode_test_tlv(tag_oid, [u8(0x55), 4, 3])
	cn << encode_test_tlv(0x1e, [u8(0), `A`, 0, 0xe9])
	mut org := encode_test_tlv(tag_oid, [u8(0x55), 4, 10])
	org << encode_test_tlv(tag_utf8string, 'Example'.bytes())
	mut set := encode_test_tlv(tag_seq, cn)
	set << encode_test_tlv(tag_seq, org)
	name := parse_name(encode_test_tlv(tag_set, set))!
	assert name[oid_common_name] == 'Aé'
	assert name[oid_organization] == 'Example'
	assert oid_string([u8(0x88), 0x37, 3])! == '2.999.3'
	for invalid in [[u8(0x80), 1], [u8(0x81)],
		[u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]] {
		if _ := oid_string(invalid) {
			assert false
		}
	}
}

fn test_invalid_versions_and_inner_lengths() {
	der := testdata_der()
	body := expect_tag(der, 0, tag_seq, 'certificate')!
	mut tbs := expect_tag(body, 0, tag_seq, 'TBS')!.clone()
	assert tbs[..5] == [u8(0xa0), 3, 2, 1, 2]
	tbs[4] = 3
	mut modified := encode_test_tlv(tag_seq, tbs)
	modified << body[skip_tlv(body, 0)!..]
	assert_bad_der(encode_test_tlv(tag_seq, modified))
	for pos in [-1, 2147483647] {
		if _ := skip_tlv(der, pos) {
			assert false
		}
		if _ := expect_tag(der, pos, tag_seq, 'certificate') {
			assert false
		}
	}
	if _, _ := split_tlv([u8(0x30), 0x84, 0xff, 0xff, 0xff, 0xff], 0) {
		assert false
	}
	if _, _ := split_tlv([u8(0x30), 0x82, 0, 0x80], 0) {
		assert false
	}
}

fn test_pem_skips_noncertificate_blocks() {
	prefix := '-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n'
	cert := parse_pem(prefix + testdata_pem())!
	assert cert.common_name() == 'mail.example.com'
	if _ := parse_pem(prefix) {
		assert false
	} else {
		assert err.msg().contains('no CERTIFICATE')
	}
}

fn test_directory_string_repertoires() {
	value, decoded := der_string(encode_test_tlv(0x14, [u8(0xe9)]))!
	assert !decoded
	assert value == ''
	for invalid in [[u8(0)], [u8(10)], [u8(`@`)]] {
		if _, _ := der_string(encode_test_tlv(tag_printable, invalid)) {
			assert false
		}
	}
	printable, ok := der_string(encode_test_tlv(tag_printable, "AZaz09 '()+,-./:=?".bytes()))!
	assert ok
	assert printable == "AZaz09 '()+,-./:=?"
}
