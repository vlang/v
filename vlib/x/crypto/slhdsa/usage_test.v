// Copyright (c) blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: has_modern_openssl?
import x.crypto.slhdsa

fn test_slhdsa_basic_default_functionality() ! {
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
	println(verified)
	assert verified == true

	// release
	pvkey.free()
	pbkey.free()
}
