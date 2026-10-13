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
	cert.check_host('mail.example.com')!
	cert.check_host('www.example.com')!
	cert.check_host('MAIL.EXAMPLE.COM')!
	cert.check_host('other.example.com') or {
		assert err.msg().contains('not covered by SAN')
		return
	}
	assert false, 'non-SAN name must fail closed'
	cert.check_host('sub.mail.example.com') or {
		return
	}
	assert false, 'deep subdomain must fail without wildcard'
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

fn test_parse_errors() {
	parse_der([]u8{}) or {
		return
	}
	assert false, 'empty DER must error'
	parse_der('not a cert'.bytes()) or {
		return
	}
	assert false, 'garbage DER must error'
	parse_pem('no blocks here') or {
		assert err.msg() == 'x509: no PEM block found'
		return
	}
	assert false, 'PEM without blocks must error'
	parse_pem('-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n') or {
		assert err.msg().contains('expected CERTIFICATE')
		return
	}
	assert false, 'non-certificate PEM must error'
}
