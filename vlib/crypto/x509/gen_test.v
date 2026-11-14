module x509

import crypto.ecdsa
import time

fn test_generate_self_signed() {
	key, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen failed') }
	_ = key
	opts := CertGenOptions{
		common_name: 'test.example.com'
		dns_names:   ['test.example.com', 'www.example.com']
		not_before:  time.now()
		not_after:   time.now().add_days(365)
	}
	cert, der := generate_self_signed(opts, priv) or { panic('gen failed') }
	assert der.len > 0
	cert.check_host('test.example.com') or { panic('host check failed') }
	cert.check_host('other.example.com') or { return }
	panic('expected host mismatch')
}
