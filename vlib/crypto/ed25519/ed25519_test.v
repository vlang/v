module ed25519

import encoding.hex

// Vectors from RFC 8032, section 7.1. `msg` is the hex encoded message and
// `sig` the hex encoded PureEdDSA signature over it.
struct Rfc8032Vector {
	sk  string
	pk  string
	msg string
	sig string
}

// TEST 1 has an empty message, TEST 1024 signs 1023 bytes.
const rfc8032_vectors = [
	Rfc8032Vector{
		sk:  '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60'
		pk:  'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a'
		msg: ''
		sig: 'e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b'
	},
	Rfc8032Vector{
		sk:  '4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb'
		pk:  '3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c'
		msg: '72'
		sig: '92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00'
	},
	Rfc8032Vector{
		sk:  'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7'
		pk:  'fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025'
		msg: 'af82'
		sig: '6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a'
	},
	Rfc8032Vector{
		sk:  'f5e5767cf153319517630f226876b86c8160cc583bc013744c6bf255f5cc0ee5'
		pk:  '278117fc144c72340f67d0f2316e8386ceffbf2b2428c9c51fef7c597f1d426e'
		msg: rfc8032_long_message
		sig: '0aab4c900501b3e24d7cdf4663326a3a87df5e4843b2cbdb67cbf6e460fec350aa5371b1508f9f4528ecea23c436d94b5e8fcd4f681e30a6ac00a9704a188a03'
	},
]

const rfc8032_long_message = '08b8b2b733424243760fe426a4b54908632110a66c2f6591eabd3345e3e4eb98' +
	'fa6e264bf09efe12ee50f8f54e9f77b1e355f6c50544e23fb1433ddf73be84d8' +
	'79de7c0046dc4996d9e773f4bc9efe5738829adb26c81b37c93a1b270b20329d' +
	'658675fc6ea534e0810a4432826bf58c941efb65d57a338bbd2e26640f89ffbc' +
	'1a858efcb8550ee3a5e1998bd177e93a7363c344fe6b199ee5d02e82d522c4fe' +
	'ba15452f80288a821a579116ec6dad2b3b310da903401aa62100ab5d1a36553e' +
	'06203b33890cc9b832f79ef80560ccb9a39ce767967ed628c6ad573cb116dbef' +
	'efd75499da96bd68a8a97b928a8bbc103b6621fcde2beca1231d206be6cd9ec7' +
	'aff6f6c94fcd7204ed3455c68c83f4a41da4af2b74ef5c53f1d8ac70bdcb7ed1' +
	'85ce81bd84359d44254d95629e9855a94a7c1958d1f8ada5d0532ed8a5aa3fb2' +
	'd17ba70eb6248e594e1a2297acbbb39d502f1a8c6eb6f1ce22b3de1a1f40cc24' +
	'554119a831a9aad6079cad88425de6bde1a9187ebb6092cf67bf2b13fd65f270' +
	'88d78b7e883c8759d2c4f5c65adb7553878ad575f9fad878e80a0c9ba63bcbcc' +
	'2732e69485bbc9c90bfbd62481d9089beccf80cfe2df16a2cf65bd92dd597b07' +
	'07e0917af48bbb75fed413d238f5555a7a569d80c3414a8d0859dc65a46128ba' +
	'b27af87a71314f318c782b23ebfe808b82b0ce26401d2e22f04d83d1255dc51a' +
	'ddd3b75a2b1ae0784504df543af8969be3ea7082ff7fc9888c144da2af58429e' +
	'c96031dbcad3dad9af0dcbaaaf268cb8fcffead94f3c7ca495e056a9b47acdb7' +
	'51fb73e666c6c655ade8297297d07ad1ba5e43f1bca32301651339e22904cc8c' +
	'42f58c30c04aafdb038dda0847dd988dcda6f3bfd15c4b4c4525004aa06eeff8' +
	'ca61783aacec57fb3d1f92b0fe2fd1a85f6724517b65e614ad6808d6f6ee34df' +
	'f7310fdc82aebfd904b01e1dc54b2927094b2db68d6f903b68401adebf5a7e08' +
	'd78ff4ef5d63653a65040cf9bfd4aca7984a74d37145986780fc0b16ac451649' +
	'de6188a7dbdf191f64b5fc5e2ab47b57f7f7276cd419c17a3ca8e1b939ae49e4' +
	'88acba6b965610b5480109c8b17b80e1b7b750dfc7598d5d5011fd2dcc5600a3' +
	'2ef5b52a1ecc820e308aa342721aac0943bf6686b64b2579376504ccc493d97e' +
	'6aed3fb0f9cd71a43dd497f01f17c0e2cb3797aa2a2f256656168e6c496afc5f' +
	'b93246f6b1116398a346f1a641f3b041e989f7914f90cc2c7fff357876e506b5' +
	'0d334ba77c225bc307ba537152f3f1610e4eafe595f6d9d90d11faa933a15ef1' +
	'369546868a7f3a45a96768d40fd9d03412c091c6315cf4fde7cb68606937380d' +
	'b2eaaa707b4c4185c32eddcdd306705e4dc1ffc872eeee475a64dfac86aba41c' +
	'0618983f8741c5ef68d3a101e8a3b8cac60c905c15fc910840b94c00a0b9d0'

fn decode_hex(s string) ![]u8 {
	return hex.decode(s)!
}

fn vector_keys(v Rfc8032Vector) !(PrivateKey, PublicKey, []u8, []u8) {
	priv := new_key_from_seed(decode_hex(v.sk)!)
	return priv, priv.public_key(), decode_hex(v.msg)!, decode_hex(v.sig)!
}

fn test_key_sizes_are_the_rfc_8032_values() {
	assert public_key_size == 32
	assert private_key_size == 64
	assert signature_size == 64
	assert seed_size == 32
}

fn test_new_key_from_seed_matches_rfc_8032() {
	for v in rfc8032_vectors {
		priv, pub_key, _, _ := vector_keys(v) or { panic(err) }
		assert priv.len == private_key_size
		assert pub_key.len == public_key_size
		assert pub_key == decode_hex(v.pk) or { panic(err) }
	}
}

fn test_private_key_keeps_the_seed() {
	for v in rfc8032_vectors {
		priv, _, _, _ := vector_keys(v) or { panic(err) }
		assert priv.seed() == decode_hex(v.sk) or { panic(err) }
	}
}

fn test_sign_matches_rfc_8032() {
	for v in rfc8032_vectors {
		priv, _, msg, sig := vector_keys(v) or { panic(err) }
		assert sign(priv, msg) or { panic(err) } == sig
	}
}

fn test_the_method_and_the_free_function_sign_alike() {
	priv, _, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	assert priv.sign(msg) or { panic(err) } == sign(priv, msg) or { panic(err) }
	assert sig.len == signature_size
}

fn test_verify_accepts_rfc_8032_signatures() {
	for v in rfc8032_vectors {
		_, pub_key, msg, sig := vector_keys(v) or { panic(err) }
		assert verify(pub_key, msg, sig) or { panic(err) }
	}
}

fn test_sign_is_deterministic() {
	priv, _, msg, _ := vector_keys(rfc8032_vectors[2]) or { panic(err) }
	assert sign(priv, msg) or { panic(err) } == sign(priv, msg) or { panic(err) }
}

fn test_verify_rejects_a_tampered_message() {
	_, pub_key, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	mut tampered := msg.clone()
	tampered[0] ^= 0xff
	assert !verify(pub_key, tampered, sig) or { panic(err) }
}

fn test_verify_rejects_a_tampered_signature() {
	_, pub_key, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	mut tampered := sig.clone()
	tampered[0] ^= 0xff
	assert !verify(pub_key, msg, tampered) or { panic(err) }
}

fn test_verify_rejects_a_signature_from_another_key() {
	priv, _, msg, _ := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	other, other_pub, _, _ := vector_keys(rfc8032_vectors[2]) or { panic(err) }
	_ = other
	assert verify(other_pub, msg, sign(priv, msg) or { panic(err) }) or { panic(err) } == false
	assert verify(priv.public_key(), msg, sign(other, msg) or { panic(err) }) or { panic(err) } == false
}

fn test_verify_rejects_a_signature_of_the_wrong_length() {
	_, pub_key, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	mut one_short := sig.clone()
	one_short.delete_last()
	assert one_short.len == signature_size - 1
	assert verify(pub_key, msg, one_short) or { panic(err) } == false
	assert verify(pub_key, msg, sig[..0]) or { panic(err) } == false
	mut one_long := sig.clone()
	one_long << u8(0)
	assert one_long.len == signature_size + 1
	assert verify(pub_key, msg, one_long) or { panic(err) } == false
}

// The high three bits of the last signature byte carry no S bits, so a value
// with them set is not a canonical scalar encoding.
fn test_verify_rejects_a_non_canonical_s() {
	_, pub_key, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	mut malleable := sig.clone()
	malleable[63] = 0xe0
	assert !verify(pub_key, msg, malleable) or { panic(err) }
}

fn test_verify_rejects_an_all_zero_signature() {
	_, pub_key, msg, _ := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	assert !verify(pub_key, msg, []u8{len: signature_size}) or { panic(err) }
}

fn test_verify_rejects_an_all_zero_public_key() {
	_, _, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	assert !verify(PublicKey([]u8{len: public_key_size}), msg, sig) or { panic(err) }
}

fn test_verify_errors_on_a_short_public_key() {
	_, _, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	short := []u8{len: public_key_size - 1}
	if v := verify(PublicKey(short), msg, sig) {
		assert v == false, 'a 31 byte public key should not verify'
	} else {
		assert err.msg() == 'ed25519: bad public key length: 31'
	}
}

fn test_verify_errors_on_a_public_key_that_is_not_a_point() {
	_, pub_key, msg, sig := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	mut not_a_point := pub_key.clone()
	not_a_point[0] ^= 0xff
	if v := verify(PublicKey(not_a_point), msg, sig) {
		assert v == false, 'an undecodable public key should not verify'
	} else {
		assert err.msg() == 'edwards25519: invalid point encoding'
	}
}

fn test_public_key_equal_matches_the_underlying_comparison() {
	_, pub_key, _, _ := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	assert pub_key.equal(pub_key)
	mut other := pub_key.clone()
	other[0] ^= 0xff
	assert !pub_key.equal(other)
	// shorter and longer keys compare unequal rather than panicking
	assert !pub_key.equal([]u8{})
	assert !pub_key.equal([]u8{len: 5})
	assert !pub_key.equal([]u8{len: public_key_size + 1})
}

fn test_private_key_equal_matches_the_underlying_comparison() {
	priv, _, _, _ := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	assert priv.equal(priv)
	assert !priv.equal([]u8{len: 5})
	mut other := priv.clone()
	other[0] ^= 0xff
	assert !priv.equal(other)
}

fn test_public_key_of_a_private_key_is_its_own_tail() {
	priv, pub_key, _, _ := vector_keys(rfc8032_vectors[1]) or { panic(err) }
	assert priv[32..] == pub_key
}

fn test_generate_key_produces_a_usable_pair() {
	pub_key, priv := generate_key() or { panic(err) }
	assert pub_key.len == public_key_size
	assert priv.len == private_key_size
	assert priv.public_key() == pub_key
	assert priv.seed().len == seed_size
	assert new_key_from_seed(priv.seed()) == priv
	assert verify(pub_key, 'vcov'.bytes(), priv.sign('vcov'.bytes()) or { panic(err) }) or { panic(err) }
	assert !verify(pub_key, 'vcov '.bytes(), priv.sign('vcov'.bytes()) or { panic(err) }) or { panic(err) }
}

fn test_generated_keys_are_not_repeated() {
	_, first := generate_key() or { panic(err) }
	_, second := generate_key() or { panic(err) }
	assert first != second
}
