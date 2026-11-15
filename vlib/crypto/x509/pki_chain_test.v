module x509

import crypto.ecdsa
import os
import time

// pki_chain_test covers build_chain + verify_chain: direct-root fast
// paths, intermediate chains (incl. cross-signed longest-path
// preference), wrong-intermediate rejection, and EKU/keyUsage/name
// constraint enforcement.

fn pki_now() time.Time {
	return time.now()
}

fn pki_ca_opts(name string) CertGenOptions {
	now := pki_now()
	return CertGenOptions{
		common_name: 'PKI ' + name
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}
}

fn pki_leaf_opts(name string, dns ...string) CertGenOptions {
	now := pki_now()
	return CertGenOptions{
		common_name: name
		dns_names:   dns
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}
}

fn pki_ecdsa_key() ecdsa.PrivateKey {
	_, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	return priv
}

// test_pki_chain_direct_root_self_signed covers the fast path where the
// leaf IS a trust anchor.
fn test_pki_chain_direct_root_self_signed() {
	priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), priv) or { panic('ca gen: ${err}') }
	chain := build_chain(ca, [], [ca]) or { panic('build: ${err}') }
	assert chain.len == 1
	verify_chain(chain, VerifyOptions{}) or { panic('verify: ${err}') }
}

// test_pki_chain_direct_root_issued covers the fast path where the leaf
// is issued directly by a root (no intermediates scanned).
fn test_pki_chain_direct_root_issued() {
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), ca_priv) or { panic('ca gen: ${err}') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(pki_leaf_opts('leaf.example.com', 'leaf.example.com'),
		leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	assert chain.len == 2
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('verify: ${err}') }
}

// test_pki_chain_intermediate builds a 3-level chain through one
// intermediate and enforces hostname on the leaf.
fn test_pki_chain_intermediate() {
	now := pki_now()
	root_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(CertGenOptions{
		common_name: 'PKI Root'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, root_priv) or { panic('root gen: ${err}') }
	mid_priv := pki_ecdsa_key()
	mid, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI Mid'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(1825)
		is_ca:       true
	}, mid_priv, &root, root_priv) or { panic('mid gen: ${err}') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'www.example.com'
		dns_names:   ['www.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &mid, mid_priv) or { panic('leaf gen: ${err}') }
	chain := build_chain(leaf, [mid], [root]) or { panic('build: ${err}') }
	assert chain.len == 3
	assert chain[1].raw == mid.raw
	assert chain[2].raw == root.raw
	verify_chain(chain, VerifyOptions{
		hostname: 'www.example.com'
	}) or { panic('verify: ${err}') }
	// Wrong hostname fails closed.
	verify_chain(chain, VerifyOptions{
		hostname: 'other.example.com'
	}) or {
		assert err.msg().contains('not covered by SAN') || err.msg().contains('does not match')
		return
	}
	assert false, 'wrong hostname must fail'
}

// test_pki_chain_cross_signed_longest prefers the longest valid path:
// L issued by B; B issued by A issued by R; X is a cross-certificate
// (B's key, R's signature) giving the shorter L->X->R path.
fn test_pki_chain_cross_signed_longest() {
	now := pki_now()
	r_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(CertGenOptions{
		common_name: 'PKI Root'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, r_priv) or { panic('root gen') }
	a_priv := pki_ecdsa_key()
	a, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI A'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(1825)
		is_ca:       true
	}, a_priv, &root, r_priv) or { panic('A gen') }
	b_priv := pki_ecdsa_key()
	b, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI B'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(900)
		is_ca:       true
	}, b_priv, &a, a_priv) or { panic('B gen') }
	// Cross-certificate: B's identity and key, signed directly by R.
	x, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI B'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(900)
		is_ca:       true
	}, b_priv, &root, r_priv) or { panic('X gen') }
	assert x.subject_der == b.subject_der
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &b, b_priv) or { panic('leaf gen') }
	chain := build_chain(leaf, [b, a, x], [root]) or { panic('build: ${err}') }
	assert chain.len == 4, 'longest path must win, got len ${chain.len}'
	assert chain[1].raw == b.raw
	assert chain[2].raw == a.raw
	assert chain[3].raw == root.raw
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('verify: ${err}') }
}

// test_pki_chain_wrong_intermediate rejects chains that cannot reach a
// trust anchor.
fn test_pki_chain_wrong_intermediate() {
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), ca_priv) or { panic('ca gen') }
	other_priv := pki_ecdsa_key()
	other, _ := generate_self_signed(pki_ca_opts('Other'), other_priv) or {
		panic('other gen')
	}
	// An intermediate from the unrelated tree cannot reach the real root.
	other_mid_priv := pki_ecdsa_key()
	other_mid, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI Other Mid'
		not_before:  pki_now().add_days(-1)
		not_after:   pki_now().add_days(1825)
		is_ca:       true
	}, other_mid_priv, &other, other_priv) or { panic('other mid gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(pki_leaf_opts('leaf.example.com', 'leaf.example.com'),
		leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	// An unrelated intermediate is ignored while the direct root path
	// still works; without the true root nothing builds.
	direct := build_chain(leaf, [other_mid], [ca]) or { panic('direct build: ${err}') }
	assert direct.len == 2
	build_chain(leaf, [other_mid], [other]) or {
		assert err.msg().contains('no valid chain')
		return
	}
	assert false, 'unrelated intermediate must fail'
	build_chain(leaf, [], [other]) or {
		assert err.msg().contains('no valid chain')
		return
	}
	assert false, 'wrong root must fail'
}

// test_pki_chain_non_ca_intermediate rejects an issuer without CA:TRUE.
fn test_pki_chain_non_ca_intermediate() {
	root_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(pki_ca_opts('Root'), root_priv) or { panic('root gen') }
	// A leaf (is_ca false, no basicConstraints) presented as intermediate.
	mid_priv := pki_ecdsa_key()
	mid, _ := generate_ca_signed(pki_leaf_opts('mid.example.com', 'mid.example.com'),
		mid_priv, &root, root_priv) or { panic('mid gen') }
	assert !mid.is_ca
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(pki_leaf_opts('leaf.example.com', 'leaf.example.com'),
		leaf_priv, &mid, mid_priv) or { panic('leaf gen') }
	build_chain(leaf, [mid], [root]) or {
		assert err.msg().contains('not a CA') || err.msg().contains('no valid chain')
		return
	}
	assert false, 'non-CA intermediate must fail'
}

// test_pki_chain_eku enforces serverAuth/clientAuth per usage. The leaf
// carries EKU serverAuth only (mirroring the openssl fixtures).
fn test_pki_chain_eku() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), ca_priv) or { panic('ca gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name:   'leaf.example.com'
		dns_names:     ['leaf.example.com']
		not_before:    now.add_days(-1)
		not_after:     now.add_days(30)
		ext_key_usage: [oid_eku_server_auth]
		has_key_usage: true
		key_usage:     ku_digital_signature
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	assert leaf.ext_key_usage == [oid_eku_server_auth]
	assert leaf.has_key_usage
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('server verify: ${err}') }
	verify_chain(chain, VerifyOptions{
		usage: .client_auth
	}) or {
		assert err.msg().contains('extended key usage')
		return
	}
	assert false, 'clientAuth against serverAuth-only leaf must fail'
}

// test_pki_chain_client_auth_ok passes a clientAuth leaf under client
// usage with no hostname check.
fn test_pki_chain_client_auth_ok() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), ca_priv) or { panic('ca gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name:   'client'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(30)
		ext_key_usage: [oid_eku_client_auth]
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	verify_chain(chain, VerifyOptions{
		usage: .client_auth
	}) or { panic('client verify: ${err}') }
}

// test_pki_chain_leaf_key_usage requires digitalSignature on TLS leaves
// that assert keyUsage.
fn test_pki_chain_leaf_key_usage() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), ca_priv) or { panic('ca gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name:   'leaf.example.com'
		dns_names:     ['leaf.example.com']
		not_before:    now.add_days(-1)
		not_after:     now.add_days(30)
		has_key_usage: true
		key_usage:     ku_key_encipherment
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or {
		assert err.msg().contains('cannot sign')
		return
	}
	assert false, 'leaf without digitalSignature must fail'
}

// test_pki_chain_openssl_rsa_fixture verifies an openssl-generated RSA
// CA/leaf pair (KU/EKU/BC critical, SAN DNS+IP) through the new path.
fn test_pki_chain_openssl_rsa_fixture() {
	td := os.join_path(@VMODROOT, 'vlib/net/tls/testdata')
	ca_der := os.read_file(os.join_path(td, 'rsa_ca_cert.pem')) or { panic('read ca: ${err}') }.bytes()
	leaf_der := os.read_file(os.join_path(td, 'rsa_leaf_cert.pem')) or {
		panic('read leaf: ${err}')
	}.bytes()
	ca := parse_pem(ca_der.bytestr()) or { panic('parse ca: ${err}') }
	leaf := parse_pem(leaf_der.bytestr()) or { panic('parse leaf: ${err}') }
	assert ca.is_ca
	assert leaf.has_key_usage && leaf.key_usage & ku_digital_signature != 0
	assert leaf.ext_key_usage == [oid_eku_server_auth]
	assert leaf.ip_addresses.len == 1
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	assert chain.len == 2
	verify_chain(chain, VerifyOptions{
		hostname: 'localhost'
	}) or { panic('verify: ${err}') }
	// clientAuth usage must fail against the serverAuth-only fixture.
	verify_chain(chain, VerifyOptions{
		usage: .client_auth
	}) or {
		assert err.msg().contains('extended key usage')
		return
	}
	assert false, 'clientAuth against fixture leaf must fail'
}

// test_pki_chain_name_constraints_dns enforces permitted/excluded
// dNSName subtrees from a CA.
fn test_pki_chain_name_constraints_dns() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI NC Root'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		permitted_dns: ['example.com']
	}, ca_priv) or { panic('ca gen: ${err}') }
	assert ca.has_name_constraints
	ok_priv := pki_ecdsa_key()
	ok_leaf, _ := generate_ca_signed(pki_leaf_opts('ok.example.com', 'ok.example.com'),
		ok_priv, &ca, ca_priv) or { panic('leaf gen') }
	chain := build_chain(ok_leaf, [], [ca]) or { panic('permitted build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: 'ok.example.com'
	}) or { panic('permitted verify: ${err}') }
	bad_priv := pki_ecdsa_key()
	bad_leaf, _ := generate_ca_signed(pki_leaf_opts('evil.com', 'evil.com'), bad_priv,
		&ca, ca_priv) or { panic('bad leaf gen') }
	build_chain(bad_leaf, [], [ca]) or {
		assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
		return
	}
	assert false, 'leaf outside permitted subtree must fail'
}

// test_pki_chain_name_constraints_excluded enforces excluded subtrees.
fn test_pki_chain_name_constraints_excluded() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:  'PKI NC Root'
		not_before:   now.add_days(-1)
		not_after:    now.add_days(3650)
		is_ca:        true
		excluded_dns: ['bad.example.com']
	}, ca_priv) or { panic('ca gen: ${err}') }
	bad_priv := pki_ecdsa_key()
	bad_leaf, _ := generate_ca_signed(pki_leaf_opts('bad.example.com', 'bad.example.com'),
		bad_priv, &ca, ca_priv) or { panic('bad leaf gen') }
	build_chain(bad_leaf, [], [ca]) or {
		assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
		return
	}
	assert false, 'excluded leaf must fail'
	// A sibling outside the excluded subtree still verifies.
	ok_priv := pki_ecdsa_key()
	ok_leaf, _ := generate_ca_signed(pki_leaf_opts('good.example.com', 'good.example.com'),
		ok_priv, &ca, ca_priv) or { panic('leaf gen') }
	chain := build_chain(ok_leaf, [], [ca]) or { panic('build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: 'good.example.com'
	}) or { panic('verify: ${err}') }
}

// test_pki_chain_name_constraints_ip enforces permitted iP ranges.
fn test_pki_chain_name_constraints_ip() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI NC Root'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		permitted_ips: [[u8(10), 0, 0, 0, 255, 0, 0, 0]]
	}, ca_priv) or { panic('ca gen: ${err}') }
	ok_priv := pki_ecdsa_key()
	ok_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'ip leaf'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
		ip_names:    [[u8(10), 1, 2, 3]]
	}, ok_priv, &ca, ca_priv) or { panic('leaf gen') }
	assert ok_leaf.ip_addresses.len == 1
	chain := build_chain(ok_leaf, [], [ca]) or { panic('permitted build: ${err}') }
	verify_chain(chain, VerifyOptions{}) or { panic('permitted verify: ${err}') }
	bad_priv := pki_ecdsa_key()
	bad_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'ip leaf 2'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
		ip_names:    [[u8(192), 168, 1, 1]]
	}, bad_priv, &ca, ca_priv) or { panic('bad leaf gen') }
	build_chain(bad_leaf, [], [ca]) or {
		assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
		return
	}
	assert false, 'leaf outside permitted IP range must fail'
}

// test_pki_chain_unknown_critical_extension locks fail-closed parsing:
// an unknown critical extension rejects the certificate.
fn test_pki_chain_unknown_critical_extension() {
	mut content := []u8{}
	for b in encode_oid('1.2.3.4.5.6.7.8') {
		content << b
	}
	for b in der_tlv(0x01, [u8(0xff)]) {
		content << b
	}
	for b in der_octstr([]u8{}) {
		content << b
	}
	ext := der_seq(content)
	// parse_extensions takes the Extension list contents (as parse_der
	// passes them), i.e. the single Extension TLV here.
	body := ext
	// Unknown critical => reject.
	parse_extensions(body) or {
		assert err.msg().contains('unsupported critical')
		// Unknown non-critical => skip.
		mut nc := []u8{}
		for b in encode_oid('1.2.3.4.5.6.7.8') {
			nc << b
		}
		for b in der_octstr([]u8{}) {
			nc << b
		}
		parsed := parse_extensions(der_seq(nc)) or {
			panic('non-critical unknown must parse: ${err}')
		}
		assert !parsed.has_san
		return
	}
	assert false, 'unknown critical extension must fail'
}

// test_pki_chain_expired_leaf rejects out-of-window leaves at build.
fn test_pki_chain_expired_leaf() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(pki_ca_opts('Root'), ca_priv) or { panic('ca gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'old.example.com'
		dns_names:   ['old.example.com']
		not_before:  now.add_days(-60)
		not_after:   now.add_days(-1)
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	build_chain(leaf, [], [ca]) or {
		assert err.msg().contains('not valid')
		return
	}
	assert false, 'expired leaf must fail'
}

// test_pki_chain_expired_intermediate rejects a chain whose intermediate
// is out of its validity window (the failure surfaces as no valid chain,
// since the leaf itself is still in-window).
fn test_pki_chain_expired_intermediate() {
	now := pki_now()
	root_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(pki_ca_opts('Root'), root_priv) or { panic('root gen') }
	mid_priv := pki_ecdsa_key()
	mid, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI Old Mid'
		not_before:  now.add_days(-60)
		not_after:   now.add_days(-1)
		is_ca:       true
	}, mid_priv, &root, root_priv) or { panic('mid gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(pki_leaf_opts('leaf.example.com', 'leaf.example.com'),
		leaf_priv, &mid, mid_priv) or { panic('leaf gen') }
	build_chain(leaf, [mid], [root]) or {
		assert err.msg().contains('no valid chain')
		return
	}
	assert false, 'expired intermediate must fail'
}

// test_pki_chain_intermediate_missing_keycertsign rejects an intermediate
// whose keyUsage lacks keyCertSign.
fn test_pki_chain_intermediate_missing_keycertsign() {
	now := pki_now()
	root_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(pki_ca_opts('Root'), root_priv) or { panic('root gen') }
	mid_priv := pki_ecdsa_key()
	mid, _ := generate_ca_signed(CertGenOptions{
		common_name:   'PKI Weak Mid'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(1825)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature
	}, mid_priv, &root, root_priv) or { panic('mid gen') }
	assert mid.is_ca
	assert mid.has_key_usage
	assert mid.key_usage & ku_key_cert_sign == 0
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(pki_leaf_opts('leaf.example.com', 'leaf.example.com'),
		leaf_priv, &mid, mid_priv) or { panic('leaf gen') }
	build_chain(leaf, [mid], [root]) or {
		assert err.msg().contains('no valid chain')
		return
	}
	assert false, 'intermediate without keyCertSign must fail'
}

// test_pki_chain_short_path_when_long_invalid prefers the shorter valid
// path when the longest path is broken: L issued by B; B issued by an A
// that cannot sign (no keyCertSign); X cross-signs B's key from R. Only
// L->X->R validates.
fn test_pki_chain_short_path_when_long_invalid() {
	now := pki_now()
	r_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(CertGenOptions{
		common_name: 'PKI Root'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, r_priv) or { panic('root gen') }
	a_priv := pki_ecdsa_key()
	a, _ := generate_ca_signed(CertGenOptions{
		common_name:   'PKI A'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(1825)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature
	}, a_priv, &root, r_priv) or { panic('A gen') }
	assert a.key_usage & ku_key_cert_sign == 0
	b_priv := pki_ecdsa_key()
	b, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI B'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(900)
		is_ca:       true
	}, b_priv, &a, a_priv) or { panic('B gen') }
	x, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI B'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(900)
		is_ca:       true
	}, b_priv, &root, r_priv) or { panic('X gen') }
	assert x.subject_der == b.subject_der
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &b, b_priv) or { panic('leaf gen') }
	chain := build_chain(leaf, [b, a, x], [root]) or { panic('build: ${err}') }
	assert chain.len == 3, 'only the short path validates, got len ${chain.len}'
	assert chain[1].raw == x.raw
	assert chain[2].raw == root.raw
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('verify: ${err}') }
}

// test_pki_chain_name_constraints_ipv6 enforces permitted iPAddress ranges
// over IPv6 SANs.
fn test_pki_chain_name_constraints_ipv6() {
	now := pki_now()
	// 2001:db8::/64 permitted.
	permitted := [u8(0x20), 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0xff,
		0xff, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0, 0, 0, 0, 0]
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI NCv6 Root'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		permitted_ips: [permitted]
	}, ca_priv) or { panic('ca gen: ${err}') }
	assert ca.has_name_constraints
	ok_priv := pki_ecdsa_key()
	ok_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'ip6 leaf'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
		ip_names:    [[u8(0x20), 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]]
	}, ok_priv, &ca, ca_priv) or { panic('leaf gen') }
	chain := build_chain(ok_leaf, [], [ca]) or { panic('permitted build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: '2001:db8::1'
	}) or { panic('permitted verify: ${err}') }
	bad_priv := pki_ecdsa_key()
	bad_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'ip6 leaf 2'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
		ip_names:    [[u8(0x20), 0x01, 0x0d, 0xb9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]]
	}, bad_priv, &ca, ca_priv) or { panic('bad leaf gen') }
	build_chain(bad_leaf, [], [ca]) or {
		assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
		return
	}
	assert false, 'IPv6 leaf outside the permitted range must fail'
}

// test_pki_chain_name_constraints_overlap checks that excluded subtrees
// win over permitted ones, for both DNS names and IP ranges.
fn test_pki_chain_name_constraints_overlap() {
	now := pki_now()
	ca_priv := pki_ecdsa_key()
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI Overlap Root'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		permitted_dns: ['example.com']
		excluded_dns:  ['bad.example.com']
		permitted_ips: [[u8(10), 0, 0, 0, 255, 0, 0, 0]]
		excluded_ips:  [[u8(10), 0, 0, 0, 255, 255, 255, 0]]
	}, ca_priv) or { panic('ca gen: ${err}') }
	// Permitted and not excluded: fine.
	ok_priv := pki_ecdsa_key()
	ok_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'ok leaf'
		dns_names:   ['good.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
		ip_names:    [[u8(10), 1, 2, 3]]
	}, ok_priv, &ca, ca_priv) or { panic('leaf gen') }
	chain := build_chain(ok_leaf, [], [ca]) or { panic('overlap build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: 'good.example.com'
	}) or { panic('overlap verify: ${err}') }
	// Excluded DNS wins over the permitted parent.
	bad_priv := pki_ecdsa_key()
	bad_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'bad leaf'
		dns_names:   ['bad.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
		ip_names:    [[u8(10), 1, 2, 3]]
	}, bad_priv, &ca, ca_priv) or { panic('bad leaf gen') }
	build_chain(bad_leaf, [], [ca]) or {
		assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
		// Excluded IP wins over the permitted range.
		ip_priv := pki_ecdsa_key()
		ip_leaf, _ := generate_ca_signed(CertGenOptions{
			common_name: 'ip leaf'
			dns_names:   ['good.example.com']
			not_before:  now.add_days(-1)
			not_after:   now.add_days(30)
			ip_names:    [[u8(10), 0, 0, 5]]
		}, ip_priv, &ca, ca_priv) or { panic('ip leaf gen') }
		build_chain(ip_leaf, [], [ca]) or {
			assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
			return
		}
		assert false, 'leaf in the excluded IP range must fail'
	}
	assert false, 'leaf in the excluded DNS subtree must fail'
}

// test_pki_chain_two_deep_intermediate_constraints enforces a name
// constraint two levels down: the root constrains mid1, mid1 constrains
// mid2's SANs, and the chain validates end to end.
fn test_pki_chain_two_deep_intermediate_constraints() {
	now := pki_now()
	root_priv := pki_ecdsa_key()
	root, _ := generate_self_signed(CertGenOptions{
		common_name: 'PKI Deep Root'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, root_priv) or { panic('root gen') }
	mid1_priv := pki_ecdsa_key()
	mid1, _ := generate_ca_signed(CertGenOptions{
		common_name:   'PKI Mid One'
		dns_names:     ['mid1.example.com']
		not_before:    now.add_days(-1)
		not_after:     now.add_days(1825)
		is_ca:         true
		permitted_dns: ['example.com']
	}, mid1_priv, &root, root_priv) or { panic('mid1 gen') }
	mid2_priv := pki_ecdsa_key()
	mid2, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI Mid Two'
		dns_names:   ['sub.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(900)
		is_ca:       true
	}, mid2_priv, &mid1, mid1_priv) or { panic('mid2 gen') }
	leaf_priv := pki_ecdsa_key()
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &mid2, mid2_priv) or { panic('leaf gen') }
	chain := build_chain(leaf, [mid2, mid1], [root]) or { panic('deep build: ${err}') }
	assert chain.len == 4
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('deep verify: ${err}') }
	// A mid2 outside mid1's permitted subtree breaks the chain.
	evil_priv := pki_ecdsa_key()
	evil_mid, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI Evil Mid'
		dns_names:   ['evil.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(900)
		is_ca:       true
	}, evil_priv, &mid1, mid1_priv) or { panic('evil mid gen') }
	evil_leaf_priv := pki_ecdsa_key()
	evil_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'evil-leaf.example.com'
		dns_names:   ['evil-leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, evil_leaf_priv, &evil_mid, evil_priv) or { panic('evil leaf gen') }
	build_chain(evil_leaf, [evil_mid, mid1], [root]) or {
		assert err.msg().contains('name constraint') || err.msg().contains('no valid chain')
		return
	}
	assert false, 'chain through an out-of-constraint mid2 must fail'
}
