// Copyright (c) 2026 blackshirt.
//
// fixtures.v embeds a fixed test-only PKI so every fakeserver handshake test
// runs against the same certificate chain with no external file or network
// dependency. The material is a throwaway Ed25519 CA plus a "localhost" leaf;
// it secures nothing and must never be reused outside these tests.
module fakeserver

import crypto.ed25519
import encoding.hex
import tls.pki

// server_name is the DNS name the embedded leaf certificate is issued for;
// tests verify the chain against this exact hostname.
pub const server_name = 'localhost'

// ca_der_hex is the DER encoding of the test CA certificate (self-signed Ed25519,
// CN="TLS Test CA", 2026-10-20 .. 2036-10-17).
const ca_der_hex = '308201793082012ba00302010202142e1a8d3b73ef8914f09d77b75fe69aefdf10afd4300506032b657030323114301206035504030c0b544c532054657374204341311a3018060355040a0c11746c732074657374206669787475726573301e170d3236313032303031353135345a170d3336313031373031353135345a30323114301206035504030c0b544c532054657374204341311a3018060355040a0c11746c732074657374206669787475726573302a300506032b65700321007eaba7bda74999fa2638834ead73c1a66d43ef2daf7fe0c8b3572efd07dc07eea3533051301d0603551d0e04160414503648cf1ecd95c76b8bdf6b51aa40f43e843792301f0603551d23041830168014503648cf1ecd95c76b8bdf6b51aa40f43e843792300f0603551d130101ff040530030101ff300506032b657003410063209e90471662318c498c9855ce47f5b797870b666970bbf9d127e2080abaaed4e33d3b931634d897153b4a1c7010248801de2e8fe368f3aa513637ecca0e01'

// leaf_der_hex is the DER encoding of the leaf certificate (Ed25519,
// CN="localhost", SAN DNS:localhost + IP:127.0.0.1, signed by the CA above).
const leaf_der_hex = '3082019530820147a00302010202147ffc71cc679aa14ade3468f2e39ebd3c4cb0f9f7300506032b657030323114301206035504030c0b544c532054657374204341311a3018060355040a0c11746c732074657374206669787475726573301e170d3236313032303031353135345a170d3336313031373031353135345a30143112301006035504030c096c6f63616c686f7374302a300506032b657003210063d19ac6b1a144004d10939407e5f0fe7e942b259765cacd13c7d97b6d759a80a3818c308189301a0603551d110413301182096c6f63616c686f737487047f00000130090603551d1304023000300b0603551d0f04040302078030130603551d25040c300a06082b06010505070301301d0603551d0e041604148c1cf2a925a3cc5257d9ed916393862825b87a8d301f0603551d23041830168014503648cf1ecd95c76b8bdf6b51aa40f43e843792300506032b6570034100222f1e5a0019eae25adec0355ad9684df2f0f370ccc742122c9825b369ee4e0f3688077e1c56a292b11d7867ae0118cb66785e38c4a13d159d7ae88f6237f000'

// leaf_seed_hex is the raw 32-byte Ed25519 private-key seed of the leaf key
// pair (the PKCS#8 PEM wraps exactly this seed).
const leaf_seed_hex = '9d4e5c2ec9fbb043b9b9f0b3e5111de641d6ab7d969f2a64fd29bc5d9a59d2b8'

// ca_der returns the DER bytes of the embedded test CA certificate.
pub fn ca_der() ![]u8 {
	return hex.decode(ca_der_hex) or {
		return error('fakeserver: embedded CA certificate is not valid hex (${err.msg()})')
	}
}

// leaf_der returns the DER bytes of the embedded test leaf certificate.
pub fn leaf_der() ![]u8 {
	return hex.decode(leaf_der_hex) or {
		return error('fakeserver: embedded leaf certificate is not valid hex (${err.msg()})')
	}
}

// leaf_private_key reconstructs the leaf's Ed25519 signing key from its embedded
// seed so the fakeserver can produce a genuine CertificateVerify.
pub fn leaf_private_key() !ed25519.PrivateKey {
	seed := hex.decode(leaf_seed_hex) or {
		return error('fakeserver: embedded leaf seed is not valid hex (${err.msg()})')
	}
	if seed.len != 32 {
		return error('fakeserver: embedded leaf seed is ${seed.len} bytes, expected 32')
	}
	return ed25519.new_key_from_seed(seed)
}

// test_trust_store builds a TrustStore that trusts exactly the embedded test CA,
// so a client configured with it authenticates the fakeserver's chain only.
pub fn test_trust_store() !pki.TrustStore {
	mut ts := pki.new_trust_store()
	ts.add_der(ca_der() or {
		return error('fakeserver: cannot decode embedded CA certificate (${err.msg()})')
	}) or {
		return error('fakeserver: cannot add embedded CA to trust store (${err.msg()})')
	}
	return ts
}
