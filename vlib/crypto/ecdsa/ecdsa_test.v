// vtest build: !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
//
// No present_openssl? conjunct: this file's own tests are genuinely
// backend-neutral (they exercise the default mbedTLS backend directly, with
// zero OpenSSL dependency) -- gating the whole file behind present_openssl?
// would incorrectly skip it on any machine lacking OpenSSL, exactly the
// class of machine this PR's own mbedTLS-default backend exists to newly
// support. The other two exclusions are unrelated to OpenSSL and are kept
// unchanged.
module ecdsa

fn test_ecdsa() {
	// Generate key pair
	pub_key, priv_key := generate_key() or { panic(err) }

	// Sign a message
	message := 'Hello, ECDSA!'.bytes()
	signature := priv_key.sign(message) or { panic(err) }

	// Verify the signature
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	println('Signature valid: ${is_valid}')
	assert is_valid

	priv_key.free()
	pub_key.free()
}

fn test_ecdsa_signing_with_recommended_hash_options() {
	// Generate key pair
	pub_key, priv_key := generate_key() or { panic(err) }

	// Sign a message
	message := 'Hello, ECDSA!'.bytes()
	signature := priv_key.sign(message) or { panic(err) }

	// Verify the signature
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	println('Signature valid: ${is_valid}')
	assert is_valid
	pub_key.free()
	priv_key.free()
}

fn test_generate_key() ! {
	// Test key generation actually produced a usable key pair -- backend-
	// neutral (no direct access to either backend's own internal key-handle
	// field): a successful sign+verify round trip proves both halves are
	// real, live keys, not just that generate_key() returned without error.
	pub_key, priv_key := generate_key() or { panic(err) }
	message := 'generate_key produced a usable pair'.bytes()
	signature := priv_key.sign(message) or { panic(err) }
	assert pub_key.verify(message, signature) or { panic(err) }

	priv_key.free()
	pub_key.free()
}

fn test_sign_and_verify() ! {
	// Test signing and verifying a message
	pub_key, priv_key := generate_key() or { panic(err) }
	message := 'Test message'.bytes()
	signature := priv_key.sign(message) or { panic(err) }
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	assert is_valid

	priv_key.free()
	pub_key.free()
}

fn test_seed() ! {
	// Test retrieving the seed from a private key
	pub_key, priv_key := generate_key() or { panic(err) }
	seed := priv_key.bytes() or { panic(err) }
	assert seed.len > 0
	priv_key.free()
	pub_key.free()
}

fn test_public_key() ! {
	// Test getting the public key from a private key
	pubkk, priv_key := generate_key() or { panic(err) }
	pub_key1 := priv_key.public_key() or { panic(err) }
	pub_key2, privkk := generate_key() or { panic(err) }
	assert !pub_key1.equal(pub_key2)

	pubkk.free()
	privkk.free()
	priv_key.free()
	pub_key1.free()
	pub_key2.free()
}

fn test_public_key_equal() ! {
	// Test public key equality
	pbk, priv_key := generate_key() or { panic(err) }
	pub_key1 := priv_key.public_key() or { panic(err) }
	pub_key2 := priv_key.public_key() or { panic(err) }
	assert pub_key1.equal(pub_key2)
	pbk.free()
	priv_key.free()
	pub_key1.free()
	pub_key2.free()
}

fn test_invalid_signature() ! {
	// Test verifying an invalid signature
	pub_key, pvk := generate_key() or { panic(err) }
	message := 'Test message'.bytes()
	invalid_signature := [u8(1), 2, 3] // Deliberately invalid
	result := pub_key.verify(message, invalid_signature) or {
		// Expecting verification to fail
		assert err.msg() == 'Failed to verify signature'
		pub_key.free()
		pvk.free()
		return
	}
	assert !result
	pub_key.free()
	pvk.free()
}

fn test_different_keys_not_equal() ! {
	// Test that different keys are not equal
	pbk1, priv_key1 := generate_key() or { panic(err) }
	pbk2, priv_key2 := generate_key() or { panic(err) }
	assert !priv_key1.equal(priv_key2)
	pbk1.free()
	pbk2.free()
	priv_key1.free()
	priv_key2.free()
}

fn test_private_key_new() ! {
	priv_key := PrivateKey.new()!
	assert priv_key.ks_flag == .fixed
	// PrivateKey.new()'s default curve is prime256v1 (P-256): a .fixed-
	// flagged key's .bytes() always returns exactly the curve's native
	// size (32 bytes) -- backend-neutral equivalent of directly checking
	// the OpenSSL-only evp_key_size(priv_key.evpkey) helper.
	size := priv_key.bytes()!.len
	assert size == 32
	pubkey := priv_key.public_key()!

	message := 'Another test message'.bytes()
	signature := priv_key.sign(message)!
	is_valid := pubkey.verify(message, signature)!
	assert is_valid

	// The new_key_from_seed-based continuation of this test (recreating this
	// same key from its own .bytes(), on the same and a different curve, and
	// comparing) moved to ecdsa_seed_use_openssl_test.v's
	// test_private_key_new_seed_roundtrip -- new_key_from_seed is not
	// implemented for the default mbedTLS backend (see that function's own
	// doc comment).

	priv_key.free()
	pubkey.free()
}

// See https://discord.com/channels/592103645835821068/592114487759470596/1334319744098107423
fn test_key_with_msg_exceed_key_size() ! {
	pv := PrivateKey.new()!
	msg := 'a'.repeat(200).bytes()
	opt := SignerOpts{
		hash_config: .with_no_hash
	}
	signed := pv.sign(msg, opt)!
	pb := pv.public_key()!

	// should be verified
	st := pb.verify(msg, signed, opt)!
	assert st

	// different msg should not be verified
	other_msg := 'a'.repeat(392).bytes()
	ds := pb.verify(other_msg, signed, opt)!
	// This should assert to false.
	assert !ds

	pv.free()
	pb.free()
}
