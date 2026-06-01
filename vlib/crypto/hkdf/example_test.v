import crypto.hkdf
import crypto.rand
import crypto.sha256

// Usage example that expands one master secret into three other
// cryptographically secure keys.
fn test_usage() {
	// Underlying hash function for HMAC.
	key_len := sha256.new().size()

	// Cryptographically secure master secret.
	secret := [u8(0x00), 0x01, 0x02, 0x03] // i.e. NOT this.

	// Non-secret salt, optional (can be empty).
	// Recommended: hash-length random value.
	salt := rand.read(sha256.new().size())!

	// Non-secret context info, optional (can be empty).
	info := 'hkdf example'

	// Generate three derived keys.
	mut keys := [][]u8{}
	for _ in 0 .. 3 {
		keys << hkdf.key(sha256.new, secret, salt, info, key_len)!
	}

	for derived_key in keys {
		assert derived_key != []u8{len: 16}
	}
}
