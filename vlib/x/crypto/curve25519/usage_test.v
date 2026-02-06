// Copyright © 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Usage example of the `curve25519` module
import x.crypto.curve25519

fn test_curve25519_shared_secret_key_exchange() ! {
	// Alice generates a private key
	mut alice_pvkey := curve25519.PrivateKey.new()!
	// Alice's PublicKey to be shared with Bob
	alice_pbkey := alice_pvkey.public_key()!

	// The other peer, Bob, has a different private key
	mut bob_pvkey := curve25519.PrivateKey.new()!
	// Bob's public key to be shared
	bob_pbkey := bob_pvkey.public_key()!

	// Let the two peers exchange their respective public keys
	//
	// Alice derives the shared secret, using her own private key, and the public key that Bob shared
	alice_shared_sec := curve25519.derive_shared_secret(mut alice_pvkey, bob_pbkey)!

	// Bob derives the shared secret, using his own private key, and the public key that Alice shared
	bob_shared_sec := curve25519.derive_shared_secret(mut bob_pvkey, alice_pbkey)!

	// the two shared secrets (derived by Alice, and derived by Bob), should be the same
	//
	assert alice_shared_sec == bob_shared_sec
}
