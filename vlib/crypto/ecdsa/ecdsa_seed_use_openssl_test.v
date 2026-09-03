// vtest build: present_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
// vtest vflags: -d use_openssl
module ecdsa

// Every test in this file exercises new_key_from_seed, which is not
// implemented for the default mbedTLS backend (see its own doc comment in
// ecdsa_notd_use_openssl.v) -- moved here, out of ecdsa_test.v, so that
// file's own backend-neutral tests can run (and now do) against the new
// default backend too. This file only ever runs under -d use_openssl (the
// vtest vflags directive above forces it), same as util_test.v and
// example/ecdsa_seed_test.v.

fn test_new_key_from_seed() ! {
	// Test generating a key from a seed
	seed := [u8(1), 2, 3, 4, 5]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	retrieved_seed := priv_key.bytes() or { panic(err) }
	assert seed == retrieved_seed
	priv_key.free()
}

fn test_new_key_from_seed_with_leading_zeros_bytes() ! {
	// Test generating a key from a seed
	seed := [u8(0), u8(1), 2, 3, 4, 5]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	retrieved_seed := priv_key.bytes() or { panic(err) }
	assert seed == retrieved_seed
	priv_key.free()
}

fn test_private_key_equal() ! {
	// Test private key equality
	pbk, priv_key1 := generate_key() or { panic(err) }
	seed := priv_key1.bytes() or { panic(err) }
	priv_key2 := new_key_from_seed(seed) or { panic(err) }
	assert priv_key1.equal(priv_key2)

	pbk.free()
	priv_key1.free()
	priv_key2.free()
}

fn test_private_key_equality_on_different_curve() ! {
	// default group
	pbk, priv_key1 := generate_key() or { panic(err) }
	seed := priv_key1.bytes() or { panic(err) }
	// using different group
	priv_key2 := new_key_from_seed(seed, nid: .secp384r1) or { panic(err) }
	assert !priv_key1.equal(priv_key2)
	pbk.free()
	priv_key1.free()
	priv_key2.free()
}

fn test_sign_with_new_key_from_seed() ! {
	// Test signing with a key generated from a seed
	seed := [u8(10), 20, 30, 40, 50]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	message := 'Another test message'.bytes()
	signature := priv_key.sign(message) or { panic(err) }
	pub_key := priv_key.public_key() or { panic(err) }
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	assert is_valid
	priv_key.free()
	pub_key.free()
}

// test_private_key_new_seed_roundtrip is the new_key_from_seed-dependent
// continuation of ecdsa_test.v's own test_private_key_new (which keeps the
// backend-neutral half: keygen, curve-size check, sign/verify).
fn test_private_key_new_seed_roundtrip() ! {
	priv_key := PrivateKey.new()!
	pubkey := priv_key.public_key()!
	message := 'Another test message'.bytes()
	signature := priv_key.sign(message)!

	// new private key, recreated from this same key's own seed bytes
	seed := priv_key.bytes()!
	priv_key2 := new_key_from_seed(seed)!
	pubkey2 := priv_key2.public_key()!
	assert priv_key.equal(priv_key2)
	assert pubkey.equal(pubkey2)
	is_valid2 := pubkey2.verify(message, signature)!
	assert is_valid2

	// generates new key with different curve
	priv_key3 := new_key_from_seed(seed, nid: .secp384r1)!
	pubkey3 := priv_key3.public_key()!
	assert !priv_key3.equal(priv_key2)
	assert !pubkey3.equal(pubkey2)
	is_valid3 := pubkey3.verify(message, signature)!
	assert !is_valid3

	priv_key.free()
	priv_key2.free()
	priv_key3.free()
	pubkey.free()
	pubkey2.free()
	pubkey3.free()
}
