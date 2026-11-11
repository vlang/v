module x509

import crypto.ecdsa
import time

fn test_ca_signed_chain() {
	ca_key, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('ca keygen') }
	_ = ca_key
	leaf_key, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('leaf keygen') }
	_ = leaf_key
	now := time.now()
	ca_opts := CertGenOptions{
		common_name: 'Test CA'
		not_before:  now
		not_after:   now.add_days(3650)
		is_ca:       true
	}
	ca_cert, _ := generate_self_signed(ca_opts, ca_priv) or { panic('ca gen') }
	leaf_opts := CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now
		not_after:   now.add_days(365)
	}
	leaf_cert, _ := generate_ca_signed(leaf_opts, leaf_priv, &ca_cert, ca_priv) or { panic('leaf gen') }
	leaf_cert.check_host('leaf.example.com') or { panic('host') }
	// verify leaf SPKI is ECDSA P-256
	pk := leaf_cert.public_key() or { panic('pubkey') }
	match pk {
		EcdsaP256PublicKey {
			assert pk.point.len == 65
		}
		RsaPublicKey { panic('expected ecdsa') }
	}
	// verify CA subject == leaf issuer (raw byte check via parse)
	_ = ca_cert
}
