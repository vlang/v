module x509

import os
import crypto.pem

fn test_public_key_ecdsa() {
	txt := os.read_file(@VMODROOT + '/vlib/crypto/x509/testdata/cert.pem') or { panic('read') }
	block, _ := pem.decode(txt) or { panic('decode') }
	cert := parse_der(block.data) or { panic('parse') }
	pk := cert.public_key() or { panic('pubkey: ${err}') }
	match pk {
		RsaPublicKey {
			assert pk.der.len > 0
		}
		EcdsaP256PublicKey { panic('expected rsa') }
	}
}
