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
		EcdsaP384PublicKey { panic('expected rsa') }
		EcdsaP521PublicKey { panic('expected rsa') }
		MlDsa44PublicKey { panic('expected rsa') }
		MlDsa65PublicKey { panic('expected rsa') }
		MlDsa87PublicKey { panic('expected rsa') }
		SlhDsaShake128sPublicKey { panic('expected rsa') }
		SlhDsaShake128fPublicKey { panic('expected rsa') }
		SlhDsaShake192sPublicKey { panic('expected rsa') }
		SlhDsaShake192fPublicKey { panic('expected rsa') }
		SlhDsaShake256sPublicKey { panic('expected rsa') }
		SlhDsaShake256fPublicKey { panic('expected rsa') }
		SlhDsaSha2128sPublicKey { panic('expected rsa') }
		SlhDsaSha2128fPublicKey { panic('expected rsa') }
		SlhDsaSha2192sPublicKey { panic('expected rsa') }
		SlhDsaSha2192fPublicKey { panic('expected rsa') }
		SlhDsaSha2256sPublicKey { panic('expected rsa') }
		SlhDsaSha2256fPublicKey { panic('expected rsa') }
	}
}
