module main

import encoding.hex
import encoding.base64
import crypto.ed25519

// adapted from https://asecuritysite.com/signatures/ed25519
fn main() {
	msg := 'Hello Girl'

	publ, priv := ed25519.generate_key()!

	m := msg.bytes()

	sig := ed25519.sign(priv, m)!

	println('=== Message ===')
	println('Msg: ${msg} \nHash: ${m}')

	println('=== Public key ===')
	println('Public key (Hex): ${hex.encode(publ)}')
	println('   Public key (Base64): ${base64.encode(publ)}')

	println('=== Private key ===')
	println('Private key: ${priv.seed().hex()}') // priv[0:32]
	println('   Private key (Base64): ${base64.encode(priv.seed())}') // priv[0:32]
	println('   Private key (Base64) Full key:  ${base64.encode(priv)}')
	println('   Private key (Full key in Hex): ${hex.encode(priv)}')

	println('=== signature (R,s) ===')
	println('signature: R=${sig[0..32].hex()} s=${sig[32..64].hex()}')
	println('   signature (Base64)=${base64.encode(sig)}')

	rtn := ed25519.verify(publ, m, sig)!

	if rtn {
		println('Signature verified :${rtn}')
	} else {
		println('signature does not verify :${!rtn}')
	}
}
