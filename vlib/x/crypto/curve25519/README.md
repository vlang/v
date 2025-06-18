# curve25519
------------
This module implement Diffie-Hellman key exchange mechanism (ECDHA) based on elliptic curve 
known as Curve25519 in pure v.

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
	// Lets for example, two peers, Alice and Bob want to create and share a shared secret together
	//
	// Alice generates own private key
	mut alice_pvkey := curve25519.PrivateKey.new()!
	// The Alice's PublicKey to be shared with Bob
	alice_pbkey := alice_pvkey.public_key()!

	// The others peer Bob, has an own private key
	mut bob_pvkey := curve25519.PrivateKey.new()!
	// The Bob's public key to be shared with Alice
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
```