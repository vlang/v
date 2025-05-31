import crypto.ecdsa

fn main() {
	// create secp256r1, NIST P-256 curve key pair
	pbkey, pvkey := ecdsa.generate_key()!

	message_tobe_signed := 'Hello ecdsa'.bytes()
	// create signature with recommended hash
	signature := pvkey.sign(message_tobe_signed, hash_config: .with_recommended_hash)!

	// verified the message with signature
	verified := pbkey.verify(message_tobe_signed, signature, hash_config: .with_recommended_hash)!
	dump(verified) // should true
	pbkey.free()
	pvkey.free()
}
