module tls

import os
import crypto.pem
import crypto.x509

fn test_cert_expiry_and_host() {
	txt := os.read_file(@VMODROOT + '/vlib/crypto/x509/testdata/cert.pem') or { panic('read failed') }
	block, _ := pem.decode(txt) or { panic('decode failed') }
	cert := x509.parse_der(block.data) or { panic('parse failed') }
	cert.check_host('mail.example.com') or { panic('host mismatch') }
	assert cert.not_after.unix() > 0
}

fn test_cert_hostname_mismatch() {
	txt := os.read_file(@VMODROOT + '/vlib/crypto/x509/testdata/cert.pem') or { panic('read failed') }
	block, _ := pem.decode(txt) or { panic('decode failed') }
	cert := x509.parse_der(block.data) or { panic('parse failed') }
	cert.check_host('wrong.example.com') or { return }
	panic('expected hostname mismatch')
}

fn test_certstore_contains() {
	txt := os.read_file(@VMODROOT + '/vlib/crypto/x509/testdata/cert.pem') or { panic('read failed') }
	block, _ := pem.decode(txt) or { panic('decode failed') }
	cert := x509.parse_der(block.data) or { panic('parse failed') }
	cs := CertStore{ roots: [cert] }
	assert cs.contains(cert)
}
