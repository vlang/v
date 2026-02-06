// Copyright (c) blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: has_modern_openssl?
module slhdsa

import rand
import encoding.hex

fn test_privatekey_from_bytes() ! {
	// for sha2_128s
	priv := hex.decode('4329f96b412b5d5ca9c730c9d54e52595265f6fc3ea67c183aa7043d90e949b8fd320c8fc0fa6a23462483535f131b9ea265848e7bb5f4d3cfd99de2595382b3')!
	mut pv := PrivateKey.from_bytes(priv, .sha2_128s)!

	sig := pv.sign('test'.bytes())!

	mut pb := pv.public_key()!
	status := pb.verify(sig, 'test'.bytes())!
	assert status == true

	pv.free()
	pb.free()
}

fn test_pvkey_from_seed() ! {
	kind := Kind.sha2_128s
	seed := rand.bytes(3 * kind.nsize())!

	mut pv := PrivateKey.from_seed(seed, kind)!

	msg := 'Hello'.bytes()
	sig := pv.sign(msg)!

	mut pb := pv.public_key()!

	verified := pb.verify(sig, msg)!
	assert verified

	pv.free()
	pb.free()
}

fn test_privkey_signing() ! {
	mut pv := PrivateKey.new(kind: .shake_256f)!
	assert pv.bytes()!.len == 128
	assert pv.public_bytes()!.len == 64

	// type name
	chr := key_type_name(pv.key)
	assert unsafe { chr.vstring() } == 'SLH-DSA-SHAKE-256f'

	msg := 'Hello'.bytes()
	sig := slhdsa_do_sign(pv.key, msg)!
	assert sig.len == 49856

	verified := slhdsa_do_verify(pv.key, sig, msg)!
	assert verified

	mut pb := pv.public_key()!
	sig2 := pv.sign(msg)!

	status := pb.verify(sig, msg)!
	status2 := pb.verify(sig2, msg)!

	assert status
	assert status2

	pv.free()
	pb.free()
}

fn test_basic_properties() ! {
	mut pv := PrivateKey.new()!
	desc := key_description(pv.key)
	unsafe { desc.vstring() } == 'OpenSSL SLH-DSA-SHA2-128f implementation'

	n := C.EVP_PKEY_is_a(pv.key, ln_slhdsa_sha2_128s)
	assert n == 1 // true
	assert C.EVP_PKEY_size(pv.key) == 7856
	assert C.EVP_PKEY_get_bits(pv.key) == 256
	assert C.EVP_PKEY_get_security_bits(pv.key) == 128

	mut pv2 := PrivateKey.new(kind: .sha2_128f)!
	n2 := C.EVP_PKEY_is_a(pv2.key, ln_slhdsa_sha2_128f)
	assert n2 == 1 // true
	assert C.EVP_PKEY_size(pv2.key) == 17088
	assert C.EVP_PKEY_get_bits(pv2.key) == 256
	assert C.EVP_PKEY_get_security_bits(pv2.key) == 128

	mut pv3 := PrivateKey.new(kind: .sha2_192s)!
	n3 := C.EVP_PKEY_is_a(pv3.key, ln_slhdsa_sha2_192s)
	assert n3 == 1 // true
	assert C.EVP_PKEY_size(pv3.key) == 16224
	assert C.EVP_PKEY_get_bits(pv3.key) == 384
	assert C.EVP_PKEY_get_security_bits(pv3.key) == 192

	mut pv4 := PrivateKey.new(kind: .shake_256f)!
	n4 := C.EVP_PKEY_is_a(pv4.key, ln_slhdsa_shake_256f)
	assert n4 == 1 // true
	assert C.EVP_PKEY_size(pv4.key) == 49856
	assert C.EVP_PKEY_get_bits(pv4.key) == 512
	assert C.EVP_PKEY_get_security_bits(pv4.key) == 256

	pv.free()
	pv2.free()
	pv3.free()
	pv4.free()
}

struct Slh128sSeed {
mut:
	seed string
	pubk string
	priv string
}

// test SLH-DSA-SHA2_128s based on samples data
fn test_privatekey_slhdsa_128s_from_seed_with_sample_data() ! {
	for obj in sample_slhdsa_128s_seed {
		seed := hex.decode(obj.seed)!
		k_opt := KeyOpts{
			kind: .sha2_128s
			flag: 1
			seed: seed
		}
		mut pv := PrivateKey.new(k_opt)!
		assert pv.bytes()! == hex.decode(obj.priv)!
		assert pv.public_bytes()! == hex.decode(obj.pubk)!
		pv.free()
	}
}

// This sample data was copied and adapted from boringssl test data in slh_keygen.txt
// at https://boringssl.googlesource.com/boringssl.git/+/chromium-stable/crypto/slhdsa/slhdsa_keygen.txt
// Its likely only support for SLH-DSA-SHA2_128s
const sample_slhdsa_128s_seed = [
	Slh128sSeed{
		seed: '2f896d61d9cd9038ca303394fadaa22a24ac5ec1d86a989ca2196c3c8632419c1a05a42fe300e87b16aee116cb2e2363'
		pubk: '1a05a42fe300e87b16aee116cb2e236358e2c3e62632c9de03d08a535a0eb7e7'
		priv: '2f896d61d9cd9038ca303394fadaa22a24ac5ec1d86a989ca2196c3c8632419c1a05a42fe300e87b16aee116cb2e236358e2c3e62632c9de03d08a535a0eb7e7'
	},
	Slh128sSeed{
		seed: '87dd614ea188940d93c7df9943caa283cbdf2ccc4940e7007b7b6c799e95e160bbaca7b93fec1a097e115f10d18e6611'
		pubk: 'bbaca7b93fec1a097e115f10d18e66112aa087f0eeed95be3ab301ebc812406f'
		priv: '87dd614ea188940d93c7df9943caa283cbdf2ccc4940e7007b7b6c799e95e160bbaca7b93fec1a097e115f10d18e66112aa087f0eeed95be3ab301ebc812406f'
	},
	Slh128sSeed{
		seed: '0436b480d04c4c47f5ecd1e4b94f103988c5f804913fac77a96572cdc26f6a92f18d59d50b6b13f71821526f3aa800a8'
		pubk: 'f18d59d50b6b13f71821526f3aa800a8cde5ccb604485df0c6f14cd64b60979c'
		priv: '0436b480d04c4c47f5ecd1e4b94f103988c5f804913fac77a96572cdc26f6a92f18d59d50b6b13f71821526f3aa800a8cde5ccb604485df0c6f14cd64b60979c'
	},
	Slh128sSeed{
		seed: 'b9341d201d49b337c84e6ac2c26361e1f5dcab582f404fe89739df4ec64eccdd04519d0d7c1ad543cd1b479602e41784'
		pubk: '04519d0d7c1ad543cd1b479602e417848aeb88695d05f531a8415a4365b62983'
		priv: 'b9341d201d49b337c84e6ac2c26361e1f5dcab582f404fe89739df4ec64eccdd04519d0d7c1ad543cd1b479602e417848aeb88695d05f531a8415a4365b62983'
	},
	Slh128sSeed{
		seed: 'cbcaec354573e8cee1373b7aced67cdc683ed4491e17ef1dff9a9163fe7bcf74d91e0df4756c799ada2a0cf1b1aada1d'
		pubk: 'd91e0df4756c799ada2a0cf1b1aada1d1fddc3692e8909f2185e11654dc84def'
		priv: 'cbcaec354573e8cee1373b7aced67cdc683ed4491e17ef1dff9a9163fe7bcf74d91e0df4756c799ada2a0cf1b1aada1d1fddc3692e8909f2185e11654dc84def'
	},
	Slh128sSeed{
		seed: '1d92b6cc53e5bb08841b2669f1fb481c62db8f494e1bac12a6c6163142842a33771c182ffb096207da6476874ebdaf15'
		pubk: '771c182ffb096207da6476874ebdaf158c0143adbd0f66057a5180c96467d31d'
		priv: '1d92b6cc53e5bb08841b2669f1fb481c62db8f494e1bac12a6c6163142842a33771c182ffb096207da6476874ebdaf158c0143adbd0f66057a5180c96467d31d'
	},
	Slh128sSeed{
		seed: '94b11b26e2ec430f413f353fae96c144915e7ec84e9c786c895593b859bdb0edd54cc085cb4f4265f8288a9d67ae5806'
		pubk: 'd54cc085cb4f4265f8288a9d67ae5806ac638ad976a1fc8e2c7c5b9db2ea0573'
		priv: '94b11b26e2ec430f413f353fae96c144915e7ec84e9c786c895593b859bdb0edd54cc085cb4f4265f8288a9d67ae5806ac638ad976a1fc8e2c7c5b9db2ea0573'
	},
	Slh128sSeed{
		seed: '9078f099c6420c6bb1303977029b1e1bac048a4375f3c48862f74a6ccedb5be4755e624791fbfd27cd195ce3aa32c62c'
		pubk: '755e624791fbfd27cd195ce3aa32c62cd9030c131ca25c8c442c4ac0b27ae548'
		priv: '9078f099c6420c6bb1303977029b1e1bac048a4375f3c48862f74a6ccedb5be4755e624791fbfd27cd195ce3aa32c62cd9030c131ca25c8c442c4ac0b27ae548'
	},
	Slh128sSeed{
		seed: 'e0c67d9fb2434e27165c861c0f1040ed58c13b7f5e9517d000770bd3e88fbdb48cdf5f1cc1aa770a8bafcf1b8c5c843b'
		pubk: '8cdf5f1cc1aa770a8bafcf1b8c5c843ba659b85d99ca18f555b8452f411b519c'
		priv: 'e0c67d9fb2434e27165c861c0f1040ed58c13b7f5e9517d000770bd3e88fbdb48cdf5f1cc1aa770a8bafcf1b8c5c843ba659b85d99ca18f555b8452f411b519c'
	},
	Slh128sSeed{
		seed: 'ccae2481cecc4a55aa2afe4874b0ce501984d48423fc2452f24f49b2a68ef9b8d66807ecbfd2dc999d274d0d6414afc3'
		pubk: 'd66807ecbfd2dc999d274d0d6414afc3dfba899a4f7799cf74e98dc1c82a26ab'
		priv: 'ccae2481cecc4a55aa2afe4874b0ce501984d48423fc2452f24f49b2a68ef9b8d66807ecbfd2dc999d274d0d6414afc3dfba899a4f7799cf74e98dc1c82a26ab'
	},
]
