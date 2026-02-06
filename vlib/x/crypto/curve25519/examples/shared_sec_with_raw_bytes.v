// Copyright © 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// This code snippet give us an example on how to use this `curve25519` module
// on creating shared secret through `x25519` function thats accepts raw bytes
import encoding.hex
import x.crypto.curve25519

fn main() {
	// Sample of randomly generated 32 bytes length of the key as a Alice key
	a_privkey := '844f744729d93369147d48d82d18b979f0b4be5f27b4b21c68fd42804575fe29'
	mut alice_privkey := hex.decode(a_privkey)!

	// For example, Alice receives Bob's public key
	b_pubkey := '7f869b403ddb1f5708bc2f8246ae78fa53ea304af585d5bb14fea3aafe5315c7'
	bob_pubkey := hex.decode(b_pubkey)!

	// Then, Alice can generated shared secret to be shared with Bob
	shared_sec := curve25519.x25519(mut alice_privkey, bob_pubkey)!
	dump(shared_sec.hex()) // shared_sec.hex(): dfa1c80d488e3de14389a852ae8f4b2f6831f8e5cea80694c7ea104ffe694858
	dump(shared_sec.len) // shared_sec.len: 32
}
