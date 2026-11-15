module pkcs12

import os

fn openssl_bin() string {
	res := os.exec(['openssl', 'version'])
	if res.exit_code == 0 {
		return 'openssl'
	}
	win := 'C:\\Program Files\\OpenSSL-Win64\\bin\\openssl.exe'
	res2 := os.exec([win, 'version'])
	if res2.exit_code == 0 {
		return win
	}
	return ''
}

fn read_fixture(name string) []u8 {
	return os.read_bytes('vlib/crypto/pkcs12/testdata/' + name) or {
		panic('missing fixture ${name}: ${err}')
	}
}

fn hex_to_bytes(s string) []u8 {
	mut out := []u8{}
	clean := s.replace(' ', '').replace('\n', '')
	assert clean.len % 2 == 0
	for i := 0; i < clean.len; i += 2 {
		out << u8(clean[i..i + 2].parse_uint(16, 8) or { panic('bad hex') })
	}
	return out
}

fn test_rc2_rfc2268_vectors() {
	vectors := [
		['0000000000000000', '63', '0000000000000000', 'ebb773f993278eff'],
		['ffffffffffffffff', '64', 'ffffffffffffffff', '278b27e42e2f0d49'],
		['30000000 00000000', '64', '10000000 00000001', '30649edf9be7d2c2'],
		['88', '64', '0000000000000000', '61a8a244adacccf0'],
		['88bca90e90875a', '64', '0000000000000000', '6ccf4308974c267f'],
	]
	for v in vectors {
		key := hex_to_bytes(v[0])
		eff := v[1].int()
		pt := hex_to_bytes(v[2])
		want := v[3].replace(' ', '').to_lower()
		block := new_rc2_cipher(key, eff) or { panic(err) }
		mut ct := []u8{len: 8}
		block.encrypt(mut ct, pt)
		assert ct.hex().to_lower() == want, 'RC2 enc mismatch for ${v[0]}'
		mut back := []u8{len: 8}
		block.decrypt(mut back, ct)
		assert back == pt, 'RC2 roundtrip failed for ${v[0]}'
	}
}

// test_key_cert derives throwaway key material from the committed openssl
// fixture itself (no committed private keys, no RSA keygen): decode the
// modern fixture and reuse its key/cert for roundtrips.
fn get_test_key_cert() !([]u8, []u8) {
	d := decode(read_fixture('openssl_modern.p12'), 'testpass')!
	if d.certs.len == 0 {
		return error('pkcs12: fixture has no certs')
	}
	return d.key_der, d.certs[0]
}

fn test_decode_openssl_modern() {
	data := read_fixture('openssl_modern.p12')
	d := decode(data, 'testpass') or { panic('decode modern: ${err}') }
	assert d.key_der.len > 500
	assert d.certs.len == 1
	assert d.certs[0].len > 100
	assert d.certs[0][0] == 0x30
	assert 'myalias' in d.friendly_names
}

fn test_decode_openssl_empty_password() {
	data := read_fixture('openssl_empty.p12')
	d := decode(data, '') or { panic('decode empty: ${err}') }
	assert d.key_der.len > 500
	assert d.certs.len == 1
	assert 'emptyalias' in d.friendly_names
}

fn test_decode_openssl_sha1mac() {
	data := read_fixture('openssl_sha1mac.p12')
	d := decode(data, 'testpass') or { panic('decode sha1mac: ${err}') }
	assert d.key_der.len > 500
	assert d.certs.len == 1
}

fn test_wrong_password_fails() {
	data := read_fixture('openssl_modern.p12')
	decode(data, 'wrongpass') or {
		assert err.msg().contains('MAC') || err.msg().contains('password')
			|| err.msg().contains('padding') || err.msg().contains('bad'), 'unexpected error: ${err}'
		return
	}
	panic('expected wrong-password failure')
}

fn test_empty_password_on_protected_fails() {
	data := read_fixture('openssl_modern.p12')
	decode(data, '') or {
		assert err.msg().contains('MAC') || err.msg().contains('password')
			|| err.msg().contains('padding') || err.msg().contains('bad'), 'unexpected error: ${err}'
		return
	}
	panic('expected empty-password failure on protected file')
}

fn test_tampered_ciphertext_fails() {
	mut data := read_fixture('openssl_modern.p12').clone()
	assert data.len > 1500
	data[1200] ^= 0xff
	decode(data, 'testpass') or {
		assert err.msg().len > 0
		return
	}
	panic('expected tamper failure')
}

fn test_tampered_mac_fails() {
	mut data := read_fixture('openssl_modern.p12').clone()
	data[data.len - 1] ^= 0x01
	decode(data, 'testpass') or {
		assert err.msg().contains('MAC') || err.msg().len > 0
		return
	}
	panic('expected MAC-tamper failure')
}

fn test_truncated_inputs_fail() {
	// Length guards must return errors, never panic with index out of range.
	for bad in [[]u8{}, [u8(0x30)], [u8(0x30), 0x03, 0x02, 0x01, 0x03]] {
		decode(bad, 'x') or { continue }
		panic('expected failure for truncated input len=${bad.len}')
	}
	mut data := read_fixture('openssl_modern.p12').clone()
	for cut in [10, 100, 500] {
		decode(data[..cut], 'testpass') or { continue }
		panic('expected failure for cut=${cut}')
	}
	// Trailing garbage must be rejected.
	mut with_trail := data.clone()
	with_trail << u8(0x00)
	decode(with_trail, 'testpass') or { return }
	panic('expected trailing-data failure')
}

fn test_unsupported_pbe_fails() {
	// RC4-based PBE is explicitly unsupported.
	mut saw_rc4 := false
	pbe_decrypt(oid_pbe_sha1_rc4_128, der_seq([]u8{}), true, [u8(1), 2, 3, 4, 5, 6, 7, 8],
		'testpass'.bytes()) or {
		assert err.msg().contains('unsupported PBE'), 'got: ${err}'
		saw_rc4 = true
	}
	assert saw_rc4, 'expected unsupported-RC4 failure'
	// Unknown PBE OID.
	mut saw_oid := false
	pbe_decrypt('1.2.3.4.5', der_seq([]u8{}), true, [u8(1), 2, 3], 'x'.bytes()) or {
		assert err.msg().contains('unsupported PBE'), 'got: ${err}'
		saw_oid = true
	}
	assert saw_oid, 'expected unsupported-OID failure'
}

fn test_unsupported_mac_hash_fails() {
	key_der, cert_der := get_test_key_cert()!
	cfg := EncodeConfig{
		mac_hash: 'md5'
	}
	encode(key_der, [cert_der], 'pw', cfg) or {
		assert err.msg().contains('unsupported MAC'), 'got: ${err}'
		return
	}
	panic('expected unsupported-MAC failure')
}

fn test_roundtrip_all_pbes() {
	key_der, cert_der := get_test_key_cert()!
	kinds := [PbeKind.pbe_sha1_3des, PbeKind.pbe_sha1_rc2_40, PbeKind.pbes2_aes128_sha256,
		PbeKind.pbes2_aes256_sha256, PbeKind.pbes2_aes128_sha1, PbeKind.pbes2_aes256_sha1]
	for kind in kinds {
		for mac in ['sha1', 'sha256'] {
			cfg := EncodeConfig{
				key_pbe:       kind
				cert_pbe:      kind
				mac_hash:      mac
				iterations:    50
				friendly_name: 'v-roundtrip'
			}
			p12 := encode(key_der, [cert_der], 'roundtrip', cfg) or {
				panic('encode ${kind}/${mac}: ${err}')
			}
			d := decode(p12, 'roundtrip') or { panic('decode ${kind}/${mac}: ${err}') }
			assert d.key_der == key_der, 'key mismatch ${kind}/${mac}'
			assert d.certs.len == 1 && d.certs[0] == cert_der, 'cert mismatch ${kind}/${mac}'
			assert 'v-roundtrip' in d.friendly_names
		}
	}
}

fn test_roundtrip_empty_password() {
	key_der, cert_der := get_test_key_cert()!
	cfg := EncodeConfig{
		iterations:    50
		friendly_name: 'v-empty'
	}
	p12 := encode(key_der, [cert_der], '', cfg) or { panic('encode empty: ${err}') }
	d := decode(p12, '') or { panic('decode empty: ${err}') }
	assert d.key_der == key_der
	assert d.certs.len == 1 && d.certs[0] == cert_der
	// Wrong password on our own empty-password file must fail.
	decode(p12, 'nope') or { return }
	panic('expected wrong-password failure on empty-password file')
}

fn test_openssl_live_export_decodes() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	dir := os.temp_dir()
	keyf := dir + '/pkcs12_live_key.pem'
	certf := dir + '/pkcs12_live_cert.pem'
	p12f := dir + '/pkcs12_live.p12'
	defer {
		os.rm(keyf) or {}
		os.rm(certf) or {}
		os.rm(p12f) or {}
	}
	gen := os.exec([bin, 'req', '-x509', '-newkey', 'ec', '-pkeyopt', 'ec_paramgen_curve:P-256',
		'-keyout', keyf, '-out', certf, '-days', '2', '-nodes', '-subj', '/CN=pkcs12-live'])
	assert gen.exit_code == 0, 'openssl req failed: ${gen.output}'
	exp := os.exec([bin, 'pkcs12', '-export', '-inkey', keyf, '-in', certf, '-out', p12f, '-passout',
		'pass:livepass', '-name', 'live'])
	assert exp.exit_code == 0, 'openssl export failed: ${exp.output}'
	data := os.read_bytes(p12f)!
	d := decode(data, 'livepass') or { panic('decode live openssl: ${err}') }
	assert d.key_der.len > 0
	assert d.certs.len == 1
	assert 'live' in d.friendly_names
	// Empty-password live export.
	exp2 := os.exec([bin, 'pkcs12', '-export', '-inkey', keyf, '-in', certf, '-out', p12f, '-passout',
		'pass:', '-name', 'live-empty'])
	assert exp2.exit_code == 0, 'openssl empty export failed: ${exp2.output}'
	data2 := os.read_bytes(p12f)!
	d2 := decode(data2, '') or { panic('decode live empty: ${err}') }
	assert d2.certs.len == 1
}

fn test_v_generated_verifies_under_openssl() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	key_der, cert_der := get_test_key_cert()!
	// PBES2 (modern) and 3DES (legacy) verify under stock openssl 4.1.
	// RC2-40 needs the legacy provider (absent on this box), so it is
	// covered by V-only roundtrips, not by openssl verify.
	good_kinds := [PbeKind.pbes2_aes256_sha256, PbeKind.pbes2_aes128_sha256, PbeKind.pbe_sha1_3des]
	for kind in good_kinds {
		cfg := EncodeConfig{
			key_pbe:       kind
			cert_pbe:      kind
			mac_hash:      'sha256'
			iterations:    100
			friendly_name: 'v-interop'
		}
		p12 := encode(key_der, [cert_der], 'testpass', cfg) or {
			panic('encode ${kind}: ${err}')
		}
		tmp := os.temp_dir() + '/v_pkcs12_${int(kind)}.p12'
		os.write_file_array(tmp, p12)!
		res := os.exec([bin, 'pkcs12', '-info', '-in', tmp, '-passin', 'pass:testpass', '-noout'])
		os.rm(tmp) or {}
		assert res.exit_code == 0, 'openssl verify failed for ${kind}: ${res.output}'
	}
}
