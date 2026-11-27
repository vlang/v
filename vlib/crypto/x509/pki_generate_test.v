module x509

import crypto.ecdsa
import crypto.sha1
import time

// pki_generate_test holds golden assertions on generated certificates:
// input validation (check_ip_names), multi-byte keyUsage encoding
// (decipherOnly is bit 8), SKI/AKI binding to the issuer, and the rule
// that leaf certificates carry no basicConstraints extension.

fn pki_gen_key() ecdsa.PrivateKey {
	_, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	return priv
}

fn pki_gen_window() (time.Time, time.Time) {
	now := time.now()
	return now.add_days(-1), now.add_days(30)
}

// test_pki_generate_check_ip_names_rejects_short rejects malformed raw
// IP SANs (3 bytes is neither IPv4 nor IPv6).
fn test_pki_generate_check_ip_names_rejects_short() {
	nb, na := pki_gen_window()
	priv := pki_gen_key()
	if _, _ := generate_self_signed(CertGenOptions{
		common_name: 'bad ip'
		not_before:  nb
		not_after:   na
		ip_names:    [[u8(1), 2, 3]]
	}, priv) {
		assert false, '3-byte IP SAN must be rejected'
	} else {
		assert err.msg().contains('invalid IP')
	}
}

// test_pki_generate_ku_decipher_only_multibyte proves the high keyUsage
// bit (decipherOnly, bit 8) encodes as a multi-byte BIT STRING and
// round-trips through the parser.
fn test_pki_generate_ku_decipher_only_multibyte() {
	nb, na := pki_gen_window()
	ca_priv := pki_gen_key()
	ca, _ := generate_self_signed(CertGenOptions{
		common_name: 'PKI KU CA'
		not_before:  nb
		not_after:   na.add_days(3620)
		is_ca:       true
	}, ca_priv) or { panic('ca gen: ${err}') }
	leaf_priv := pki_gen_key()
	_, leaf_der := generate_ca_signed(CertGenOptions{
		common_name:   'leaf.example.com'
		dns_names:     ['leaf.example.com']
		not_before:    nb
		not_after:     na
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_decipher_only
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	back := parse_der(leaf_der) or { panic('reparse: ${err}') }
	assert back.has_key_usage
	assert back.key_usage & ku_decipher_only != 0
	assert back.key_usage & ku_digital_signature != 0
}

// test_pki_generate_ski_aki_binding asserts SKI == sha1(pubkey) on issued
// certificates (parsed back from DER) and AKI == the issuer SKI.
fn test_pki_generate_ski_aki_binding() {
	nb, na := pki_gen_window()
	ca_priv := pki_gen_key()
	ca, ca_der := generate_self_signed(CertGenOptions{
		common_name: 'PKI SKI CA'
		not_before:  nb
		not_after:   na.add_days(3620)
		is_ca:       true
	}, ca_priv) or { panic('ca gen: ${err}') }
	ca_back := parse_der(ca_der) or { panic('ca reparse: ${err}') }
	assert ca_back.subject_key_id.len == 20
	ca_pk := ca.public_key() or { panic('ca pubkey: ${err}') }
	match ca_pk {
		EcdsaP256PublicKey {
			assert ca_back.subject_key_id == sha1.sum(ca_pk.point)
		}
		else {
			assert false, 'expected P-256 CA key'
		}
	}
	leaf_priv := pki_gen_key()
	leaf, leaf_der := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  nb
		not_after:   na
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	leaf_back := parse_der(leaf_der) or { panic('leaf reparse: ${err}') }
	assert leaf_back.subject_key_id.len == 20
	assert leaf_back.authority_key_id == ca_back.subject_key_id
	leaf_pk := leaf.public_key() or { panic('leaf pubkey: ${err}') }
	match leaf_pk {
		EcdsaP256PublicKey {
			assert leaf_back.subject_key_id == sha1.sum(leaf_pk.point)
		}
		else {
			assert false, 'expected P-256 leaf key'
		}
	}
}

// test_pki_generate_leaf_has_no_basic_constraints asserts leaves omit
// the basicConstraints extension while CAs carry it as critical CA:TRUE.
fn test_pki_generate_leaf_has_no_basic_constraints() {
	nb, na := pki_gen_window()
	ca_priv := pki_gen_key()
	ca, ca_der := generate_self_signed(CertGenOptions{
		common_name: 'PKI BC CA'
		not_before:  nb
		not_after:   na.add_days(3620)
		is_ca:       true
	}, ca_priv) or { panic('ca gen: ${err}') }
	ca_back := parse_der(ca_der) or { panic('ca reparse: ${err}') }
	assert ca_back.is_ca
	assert ca_back.has_basic_constraints
	leaf_priv := pki_gen_key()
	_, leaf_der := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  nb
		not_after:   na
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	leaf_back := parse_der(leaf_der) or { panic('leaf reparse: ${err}') }
	assert !leaf_back.is_ca
	assert !leaf_back.has_basic_constraints
}

// test_pki_generate_serial_minimal_encoding locks the DER minimality of
// serial numbers: the previous strip-then-mask logic emitted a redundant
// leading zero whenever the first random byte was exactly 0x80, which the
// parser rejects ("invalid serial integer") once every few hundred certs.
fn test_pki_generate_serial_minimal_encoding() {
	// Fixed vectors through the normalizer, including the old failure.
	assert normalize_serial_bytes([u8(0x00), 0x00, 0x12]) == [u8(0x12)]
	assert normalize_serial_bytes([u8(0x7f)]) == [u8(0x7f)]
	assert normalize_serial_bytes([u8(0x80)]) == [u8(0x00), 0x80]
	assert normalize_serial_bytes([u8(0x80), 0x34]) == [u8(0x00), 0x80, 0x34]
	assert normalize_serial_bytes([u8(0x80), 0xff]) == [u8(0x00), 0x80, 0xff]
	assert normalize_serial_bytes([u8(0x00)]) == [u8(0x00)]
	// Property: every fresh serial is minimal and non-negative, so it
	// always survives our own parser.
	for _ in 0 .. 64 {
		s := fresh_serial() or { panic('serial: ${err}') }
		assert s.len >= 1
		assert s[0] & 0x80 == 0
		if s.len > 1 {
			assert s[0] != 0 || s[1] & 0x80 != 0
		}
	}
	// End to end: issuance round-trips through the parser.
	nb, na := pki_gen_window()
	priv := pki_gen_key()
	_, der := generate_self_signed(CertGenOptions{
		common_name: 'serial.example.com'
		dns_names:   ['serial.example.com']
		not_before:  nb
		not_after:   na
	}, priv) or { panic('gen: ${err}') }
	_ = parse_der(der) or { panic('reparse: ${err}') }
}
