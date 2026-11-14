module x509

import crypto.ecdsa
import crypto.sha256
import time

fn test_der_roundtrip_self_signed() {
	key, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen') }
	_ = key
	now := time.now()
	opts := CertGenOptions{
		common_name: 'rt.example.com'
		dns_names:   ['rt.example.com']
		not_before:  now
		not_after:   now.add_days(30)
	}
	cert, der := generate_self_signed(opts, priv) or { panic('gen') }
	parsed := parse_der(der) or { panic('parse') }
	parsed.check_host('rt.example.com') or { panic('host') }
	pk := parsed.public_key() or { panic('pubkey') }
	match pk {
		EcdsaP256PublicKey {
			assert pk.point.len == 65
		}
		RsaPublicKey { panic('expected ecdsa') }
	}
	msg := 'hello'.bytes()
	sig := priv.sign(sha256.sum256(msg), ecdsa.SignerOpts{}) or { panic('sign') }
	match pk {
		EcdsaP256PublicKey {
			pk2 := ecdsa.PublicKey.from_uncompressed_bytes(pk.point, ecdsa.CurveOptions{}) or { panic('pk2') }
			ok := pk2.verify(sha256.sum256(msg), sig, ecdsa.SignerOpts{}) or { panic('verify') }
			assert ok
		}
		else {}
	}
	mut bad := der.clone()
	if bad.len > 0 {
		bad[0] = 0x00
	}
	parse_der(bad) or { return }
	panic('expected parse error on tampered DER')
}

fn test_der_roundtrip_ca_signed() {
	ca_key, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('ca keygen') }
	_ = ca_key
	leaf_key, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('leaf keygen') }
	_ = leaf_key
	now := time.now()
	ca_opts := CertGenOptions{ common_name: 'CA', not_before: now, not_after: now.add_days(3650) }
	ca_cert, _ := generate_self_signed(ca_opts, ca_priv) or { panic('ca gen') }
	leaf_opts := CertGenOptions{ common_name: 'leaf.example.com', dns_names: ['leaf.example.com'], not_before: now, not_after: now.add_days(365) }
	leaf_cert, leaf_der := generate_ca_signed(leaf_opts, leaf_priv, &ca_cert, ca_priv) or { panic('leaf gen') }
	parsed := parse_der(leaf_der) or { panic('parse') }
	parsed.check_host('leaf.example.com') or { panic('host') }
	_ = leaf_cert
	mut bad := leaf_der.clone()
	if bad.len > 0 { bad[0] = 0x00 }
	parse_der(bad) or { return }
	panic('expected parse error on tampered DER')
}
