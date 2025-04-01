module main

import x.crypto.slhdsa

fn main() {
	// creates default key
	mut pvkey := slhdsa.PrivateKey.new()!

	// example of message
	xmsg := 'Sample message'.bytes()

	// signing the message with the key
	sig := pvkey.sign(xmsg)!

	// example of public key
	mut pbkey := pvkey.public_key()!

	// verify signature with the public key
	verified := pbkey.verify(sig, xmsg)!
	dump(verified) // true
	assert verified == true

	// release the resources
	pvkey.free()
	pbkey.free()
}
