// vtest build: has_modern_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
// vtest vflags: -d use_openssl
import crypto.ecdsa
import x.crypto.mldsa as mldsas

fn test_ecdsa_and_mldsa_are_compatible() ! {
	pubkey, prikey := ecdsa.generate_key(nid: .prime256v1)!
	defer {
		pubkey.free()
		prikey.free()
	}
	assert prikey.bytes()!.len > 0
	assert pubkey.bytes()!.len > 0
	message := 'ECDSA and ML-DSA in the same program'.bytes()
	signature := prikey.sign(message)!
	assert pubkey.verify(message, signature)!

	// Exercise the Result-returning C.EVP_PKEY helper from the original report.
	seeded := ecdsa.new_key_from_seed([]u8{len: 32, init: 1})!
	defer {
		seeded.free()
	}
	assert seeded.bytes()!.len > 0

	ml_private := mldsas.PrivateKey.generate(.ml_dsa_44)!
	ml_public := ml_private.public_key()
	assert ml_private.bytes().len > 0
	assert ml_public.bytes().len > 0
	ml_signature := ml_private.sign(message)!
	assert ml_public.verify(message, ml_signature)!
}
