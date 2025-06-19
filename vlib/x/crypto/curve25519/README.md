# curve25519
------------
This module implement Diffie-Hellman key exchange mechanism (ECDHA) based on elliptic curve 
known as Curve25519 in pure V.

## About curve25519

From wikipedia, Curve25519 is an elliptic curve used in elliptic-curve cryptography (ECC) 
offering 128 bits of security (256-bit key size) and designed for use with the elliptic 
curve Diffie–Hellman (ECDH) key agreement scheme. 
It is one of the fastest curves in ECC, and is not covered by any known patents. 
The reference implementation is public domain software. The original Curve25519 paper defined it as 
a Diffie–Hellman (DH) function. 
Daniel J. Bernstein has since proposed that the name "Curve25519" be used for the underlying curve,
and the name "X25519" for the DH function Curve25519 is an elliptic curve 
that offers 128 security bits 
and is designed for use in the Elliptic Curve Diffie-Hellman (ECDH) key agreement key design scheme.

Examples
--------
```v
import x.crypto.curve25519

fn main() {
	// For example, two peers, Alice and Bob, want to create a shared secret together
	//
	// Alice generates a private key
	mut alice_pvkey := curve25519.PrivateKey.new()!
	// Alice's PublicKey to be shared with Bob
	alice_pbkey := alice_pvkey.public_key()!

	// The other peer, Bob, has a different private key
	mut bob_pvkey := curve25519.PrivateKey.new()!
	// Bob's public key to be shared with Alice
	bob_pbkey := bob_pvkey.public_key()!

	// Let the two peers exchange their respective public keys
	//
	// Alice derives the shared secret, using her own private key, and the public key that Bob shared
	alice_shared_sec := curve25519.derive_shared_secret(mut alice_pvkey, bob_pbkey)!

	// Bob derives the shared secret, using his own private key, and the public key that Alice shared
	bob_shared_sec := curve25519.derive_shared_secret(mut bob_pvkey, alice_pbkey)!

	// the two shared secrets (derived by Alice, and derived by Bob), should be the same
	//
	println(alice_shared_sec.hex()) // 49fd4a4d0637d2413cd501b20111fc50a592dc21460e45f451c03d1fd3cef900
	println(bob_shared_sec.hex()) // 49fd4a4d0637d2413cd501b20111fc50a592dc21460e45f451c03d1fd3cef900
	dump(alice_shared_sec == bob_shared_sec) // alice_shared_sec == bob_shared_sec: true

	assert alice_shared_sec == bob_shared_sec
}
```