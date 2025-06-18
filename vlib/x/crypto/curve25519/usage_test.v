// Copyright © 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Usage example of the `curve25519` module
import x.crypto.curve25519

fn test_curve25519_shared_secret_key_exchange() ! {
	// Lets for example, Alice generates own private key
	mut alice_pvkey := curve25519.PrivateKey.new()!
	// Get the Alice's PublicKey
	alice_pbkey := alice_pvkey.public_key()!

	// The others peer Bob, has an own private key
	mut bob_pvkey := curve25519.PrivateKey.new()!
	// Get the Bob's public key
	bob_pbkey := bob_pvkey.public_key()!

	// Lets two peer has received and send their respective public key
	//
	// Alice derives shared secret with her own private key with shared Bob's public key
	alice_shared_sec := curve25519.derive_shared_secret(mut alice_pvkey, bob_pbkey)!

	// Bob derives shared secret with his own private key with Alice's public key
	bob_shared_sec := curve25519.derive_shared_secret(mut bob_pvkey, alice_pbkey)!

	// two's shared secret should identical
	assert alice_shared_sec == bob_shared_sec
}
