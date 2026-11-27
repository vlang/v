module tls

import crypto.ecdsa
import crypto.x509
import time

fn test_verify_cert_signature_ecdsa() {
	ca_key, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('ca keygen') }
	_ = ca_key
	leaf_key, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('leaf keygen') }
	_ = leaf_key
	now := time.now()
	ca_opts := x509.CertGenOptions{
		common_name: 'Test CA'
		not_before:  now
		not_after:   now.add_days(3650)
	}
	ca_cert, _ := x509.generate_self_signed(ca_opts, ca_priv) or { panic('ca gen') }
	leaf_opts := x509.CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now
		not_after:   now.add_days(365)
	}
	leaf_cert, leaf_der := x509.generate_ca_signed(leaf_opts, leaf_priv, &ca_cert, ca_priv) or { panic('leaf gen') }
	_ = leaf_cert
	// extract CA public key point
	ca_pk := ca_cert.public_key() or { panic('ca pubkey') }
	match ca_pk {
		x509.EcdsaP256PublicKey {
			verify_cert_signature_ecdsa(leaf_der, ca_pk.point) or { panic('verify failed: ${err}') }
		}
		else { panic('expected ecdsa') }
	}
}
