// Copyright © 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This code snippet give us an example on how to use this `curve25519` module
// on creating shared secret with custom derivation function
import crypto.sha512
import encoding.hex
import x.crypto.curve25519

// Custom SHA384 based derivator
struct Sha384Derivator {
}

// derives a new shared secret by hashing the sec bytes with sha384 hash
fn (sd Sha384Derivator) derive(sec []u8, opt curve25519.DeriveOpts) ![]u8 {
	return sha512.sum384(sec)
}

fn main() {
	// Sample of randomly generated 32 bytes length of the key as a Alice key
	a_pvkey := hex.decode('844f744729d93369147d48d82d18b979f0b4be5f27b4b21c68fd42804575fe29')!
	mut alice_pvkey := curve25519.PrivateKey.new_from_seed(a_pvkey)!

	// For example, Alice receives Bob's public key
	b_pubkey := hex.decode('7f869b403ddb1f5708bc2f8246ae78fa53ea304af585d5bb14fea3aafe5315c7')!
	bob_pubkey := curve25519.PublicKey.new_from_bytes(b_pubkey)!

	opt := curve25519.SharedOpts{
		should_derive: true
		derivator:     Sha384Derivator{}
	}
	shared_sec := curve25519.derive_shared_secret(mut alice_pvkey, bob_pubkey, opt)!

	// shared_sec now contains sha384 digest form, derived from sec bytes supplied
	// shared_sec.hex(): 88d648a1c5437e2807ed48aaea7ae3190ffe4b2b74008cd2c1ace9bddf28afe50b9a0c9e06dd50bbf73e0203bc1775dc
	dump(shared_sec.hex())
	dump(shared_sec.len) // shared_sec.len: 48
}
