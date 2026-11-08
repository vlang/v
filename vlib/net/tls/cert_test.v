module tls

import crypto.pem
import os

fn test_extract_ecdsa_pubkey() {
	pem_text := os.read_file('vlib/net/quic/testdata/tls13_vectors/quiche_p256_handshake_server_cert.pem') or { panic(err) }
	block, _ := pem.decode(pem_text) or { panic('no pem') }
	pk := extract_ecdsa_pubkey(block.data) or { panic(err) }
	// round-trip: uncompressed bytes length 65
	ub := pk.uncompressed_bytes() or { panic(err) }
	assert ub.len == 65
	assert ub[0] == 0x04
}
