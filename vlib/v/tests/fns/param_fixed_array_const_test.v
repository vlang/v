module main

import crypto.ed25519

fn new_client_hello(public_key [ed25519.public_key_size]u8) {
	assert public_key.len == ed25519.public_key_size
	assert public_key.len != 0
}

fn test_main() {
	new_client_hello([ed25519.public_key_size]u8{})
}
