// tests are taken from https://tools.ietf.org/html/rfc2202
module hmac

import crypto.md5
import crypto.sha1
import crypto.sha256
import crypto.sha512
import crypto.sha3
import crypto.blake2s
import crypto.blake2b
import crypto.blake3

// not yet supported
// import crypto.md4
// import crypto.md5sha1
// import crypto.ripemd160

const keys = [
	[u8(0xb), 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb, 0xb],
	'Jefe'.bytes(),
	[u8(0xAA), 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
		0xAA],
	[u8(0x01), 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
		0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19],
	[u8(0x0c), 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c, 0x0c,
		0x0c],
	[u8(0xaa), 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa],
	[u8(0xaa), 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		0xaa, 0xaa, 0xaa, 0xaa, 0xaa],
]
const data = ['Hi There'.bytes(), 'what do ya want for nothing?'.bytes(),
	[u8(0xDD), 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
		0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
		0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
		0xDD, 0xDD, 0xDD, 0xDD, 0xDD],
	[u8(0xcd), 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd,
		0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd,
		0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd, 0xcd,
		0xcd, 0xcd, 0xcd, 0xcd, 0xcd],
	'Test With Truncation'.bytes(), 'Test Using Larger Than Block-Size Key - Hash Key First'.bytes(),
	'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'.bytes()]

fn test_hmac_md5() {
	md5_expected_results := [
		'9294727a3638bb1c13f48ef8158bfc9d',
		'750c783e6ab0b503eaa86e310a5db738',
		'56be34521d144c88dbb8c733f0e8b3f6',
		'697eaf0aca3a3aea3a75164746ffaa79',
		'56461ef2342edc00f9bab995690efd4c',
		'6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd',
		'6f630fad67cda0ee1fb1f562db3aa53e',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], md5.sum, md5.block_size).hex()
		assert result == md5_expected_results[i]
	}
}

fn test_hmac_sha1() {
	sha1_expected_results := [
		'675b0b3a1b4ddf4e124872da6c2f632bfed957e9',
		'effcdf6ae5eb2fa2d27416d5f184df9c259a7c79',
		'd730594d167e35d5956fd8003d0db3d3f46dc7bb',
		'4c9007f4026250c6bc8414f9bf50c86c2d7235da',
		'37268b7e21e84da5720c53c4ba03ad1104039fa7',
		'aa4ae5e15272d00e95705637ce8a3b55ed402112',
		'e8e99d0f45237d786d6bbaa7965c7808bbff1a91',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha1.sum, sha1.block_size).hex()
		assert result == sha1_expected_results[i]
	}
}

fn test_hmac_sha224() {
	sha224_expected_results := [
		'4e841ce7a4ae83fbcf71e3cd64bfbf277f73a14680aae8c518ac7861',
		'a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44',
		'cbff7c2716bbaa7c77bed4f491d3e8456cb6c574e92f672b291acf5b',
		'6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a',
		'd812c97a5e1412f2eb08dc4d95548117780f2930fa4e0e553d985c68',
		'9ed2eebc0ed23576efc815e9b5bc0d9257e36d13e4dd5d5f0c809b38',
		'7358939e58683a448ac5065196d33191a1c1d33d4b8b0304dc60f5e0',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha256.sum224, sha256.block_size).hex()
		assert result == sha224_expected_results[i]
	}
}

fn test_hmac_sha256() {
	sha256_expected_results := [
		'492ce020fe2534a5789dc3848806c78f4f6711397f08e7e7a12ca5a4483c8aa6',
		'5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843',
		'7dda3cc169743a6484649f94f0eda0f9f2ff496a9733fb796ed5adb40a44c3c1',
		'82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b',
		'2282475faa2def6936685d9c06566f2d782307ace7a27ada2037e6285efcb008',
		'6953025ed96f0c09f80a96f78e6538dbe2e7b820e3dd970e7ddd39091b32352f',
		'6355ac22e890d0a3c8481a5ca4825bc884d3e7a1ff98a2fc2ac7d8e064c3b2e6',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha256.sum, sha256.block_size).hex()
		assert result == sha256_expected_results[i]
	}
}

fn test_hmac_sha384() {
	sha384_expected_results := [
		'7afaa633e20d379b02395915fbc385ff8dc27dcd3885e1068ab942eeab52ec1f20ad382a92370d8b2e0ac8b83c4d53bf',
		'af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649',
		'1383e82e28286b91f4cc7afbd13d5b5c6f887c05e7c4542484043a37a5fe45802a9470fb663bd7b6570fe2f503fc92f5',
		'3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e6801dd23c4a7d679ccf8a386c674cffb',
		'10e0150a42d0ae6f9d3f55da7a8261c383b024c8d81b40e95d120acfd53fb018af5e77846ad99451059f0579cb9a718b',
		'69d2e2f55de9f09878f04d23d8670d49cb734825cdb9cd9e72e446171a43540b90e17cf086e6fa3a599382a286c61340',
		'34f065bdedc2487c30a634d9a49cf42116f78bb386ea4d498aea05c0077f05373cfdaa9b59a7b0481bced9e3f55016a9',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha512.sum384, sha512.block_size).hex()
		assert result == sha384_expected_results[i]
	}
}

fn test_hmac_sha512() {
	sha512_expected_results := [
		'7641c48a3b4aa8f887c07b3e83f96affb89c978fed8c96fcbbf4ad596eebfe496f9f16da6cd080ba393c6f365ad72b50d15c71bfb1d6b81f66a911786c6ce932',
		'164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737',
		'ad9b5c7de72693737cd5e9d9f41170d18841fec1201c1c1b02e05cae116718009f771cad9946ddbf7e3cde3e818d9ae85d91b2badae94172d096a44a79c91e86',
		'b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd',
		'da2c03a1f8d34ce536b246c9dc47281d7052d3f82a7b4f6dfe9ee9f5accdae02dd72f9b89324f25f9b8276a2e3d31c0a87b8b6c1dcefd7602cc881a7d120e3fd',
		'132c9ebc32531071f6c4d9e8842291e9403e5940f813170a3ba3a0dd6c055c8b8ca587b24c56c47f3c1f2fb8ee8f9fbc8d92deed0f83426be3e8a2e9056778b3',
		'09441cda584ed2f4d2f5b519c71baf3c79cce19dfc89a548e73b3bb382a9124d6e792b77bf57903ff5858e5d111d15f45d6fd118eea023f28d2eb234ebe62f85',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha512.sum512, sha512.block_size).hex()
		assert result == sha512_expected_results[i]
	}
}

fn test_hmac_blake2s_256() {
	blake2s_256_expected_results := [
		'139cd736b926dae4853aab90655120e0305c476fde978166e472c7c8698c21b1',
		'90b6281e2f3038c9056af0b4a7e763cae6fe5d9eb4386a0ec95237890c104ff0',
		'92394bc2486bea31db01c74be46332edd1499e9700ea41df0670df79fcc0f7c6',
		'464434dcbece095d456a1d62d6ec56f898e625a39e5c52bdf94daf111bad83aa',
		'97a771b4e4e2fd4d4d0fd8aca2a0663ad8ad4a463cabbf603bf837dd84dec050',
		'41202b7be1fd03f84658f4c18a08b43ddbbd73eb012750c8d1ebc8601f1e064c',
		'467201ef5997a3442932b318083488cf9aa1d89bef2146154b4816d34863e33d',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2s.sum256, blake2s.block_size).hex()
		assert result == blake2s_256_expected_results[i]
	}
}

fn test_hmac_blake2s_224() {
	blake2s_224_expected_results := [
		'c0f46dbc41ffafb1d0caf1756cfb19177c78571d0d187e83f8683b3b',
		'c33653634100f77a9359bfc4b3bd40a4a629766fb1da794dbb5cc45b',
		'eeff700c648c71f8648077862d2aeb5108e7887adc2aaa2ee25aefab',
		'42c6a6688981f6f33c02ceafb0439a496748d45d99fdde22d95313ac',
		'fa78ffd75f2cdc6dc1304b874affd069783cfa8e6ae00dcb99de31f7',
		'99d1e80dbc842755776ae32309e19020bbb54834c5c7d0c49fee7df2',
		'17b9ebb1426a5a3dd6aa91567bd9cb9c19b3dc007adb726e55b98926',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2s.sum224, blake2s.block_size).hex()
		assert result == blake2s_224_expected_results[i]
	}
}

fn test_hmac_blake2s_160() {
	blake2s_160_expected_results := [
		'e53d35df4b62a18e0f9f65be35db58ffd84f4cf7',
		'8356d437404c027ab98bf581190a96ca83ac7dc0',
		'f9631e19deb77bd9ac0761015b7fddfafe14af12',
		'63a8d39896fa7c4c5dac8c82ac20c783d7865e0c',
		'05db15418a3b7938fcab45bf760a88e842033556',
		'580927d927e9279e3f3f95d19b1c79564a0d6326',
		'6f3127fcba040fe6ea552b22c39b0fd83abca19a',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2s.sum160, blake2s.block_size).hex()
		assert result == blake2s_160_expected_results[i]
	}
}

fn test_hmac_blake2s_128() {
	blake2s_128_expected_results := [
		'78a8c08a89de396ba4a75c543e6415f8',
		'5edb7b76d61965470774a215034e8b18',
		'77e6870fa0e9dd65751d29204086bed0',
		'b43e3298ab9be441dd3cd26c70208b4a',
		'43f416bd5d0933b4ad35aeef9c6680a7',
		'39c14a9cebb8e2d8d19594783aed29e4',
		'96a72e3adf5e0b02d4e6d4e8a7342a77',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2s.sum128, blake2s.block_size).hex()
		assert result == blake2s_128_expected_results[i]
	}
}

fn test_hmac_blake2b_512() {
	blake2b_512_expected_results := [
		'be2c398cbd5eef033031f9cee2caba52b280604f4afabf86de21973398c821fd120de5277f08955234182989c68b640af7dfa8cb9228eef4b48ffe768ef595eb',
		'6ff884f8ddc2a6586b3c98a4cd6ebdf14ec10204b6710073eb5865ade37a2643b8807c1335d107ecdb9ffeaeb6828c4625ba172c66379efcd222c2de11727ab4',
		'548fc7c3d5bd5a0ac74c5fe582037a259c774b81fb791d6c86e675d03b361939e21ea0fb9c6401df22170ebf8017d908675bbcf65911025a1ab5d271a372f43f',
		'e5dbb6de2fee42a1caa06e4e7b84ce408ffa5c4a9de2632eca769cde8875014c72d0720feaf53f76e6a180357f528d7bf484fa3a14e8cc1f0f3bada717b43491',
		'057f3bcc3511aa9627d96ab2ad02ec3dc86741514d19a3c9b10e539b0ca7c2587d9d04118636a67e18871633ecf8705a3ef6697cf6b64339f71b8cdffab89e34',
		'368aba23ca42648157771936b436a0ecb3d83e5fe21e020fef2a08dc9e59739ea919a8d0f46c45b99491f426f1e7c62352d9d67c066571d74e191b69bedaf718',
		'f1c9b64e121330c512dc31e0d4a2fc84b7ca5be64e08934a7fc4640c4a1f5cc3c1f34d811c8079cc2df65a4e5d68baf833a1ec558546abeaa7d564840618db7b',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2b.sum512, blake2b.block_size).hex()
		assert result == blake2b_512_expected_results[i]
	}
}

fn test_hmac_blake2b_384() {
	blake2b_384_expected_results := [
		'ce5d294bf1a260a93d29d4380fb5b66ebff87bc5b5d97c8889c115848ac0c559ef5b257c7ea1af05e6794e540a07d29a',
		'e87f61624bc6c1db706a1efcc98e7e98c0ab8fea0978dbd87e2852406fdd313c87968c5b825847f3c04d975b8b88598d',
		'9a24ae99bdddb091e966897c9ed3866d403c8d57bb99288aba05c0586304163ff1917420c746edaa327c99c419249799',
		'123edcc587acf610f9f063c8873b94af1ca814e83b65abfd30bcfc4dbc2cc0bc6f1cd8460e5970cc375842bee91f0acb',
		'08fb6e2930399c032169428b86c6799284fe3d42ac950698ecf61c08a1b40d7e84627ac578332947f661c2fe2e5f03c9',
		'18666044866b52e2d76276a803222cdeb4ee18b41e8a535dca6b14723a7a5031c09463faa88a64d6a5e448220e20e676',
		'c9d0155de83454f0720b5310b4b891ddc9ab702b8260b15aa6f7291efec95b7e7a2c986019814b7c28c105c22f0ef961',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2b.sum384, blake2b.block_size).hex()
		assert result == blake2b_384_expected_results[i]
	}
}

fn test_hmac_blake2b_256() {
	blake2b_256_expected_results := [
		'1a8becea07bab690945471ff79895bfbec032a3957bc9142a4778811915931ba',
		'3cf096eeeb2202a250db168c4823a44ef4618ebabb225789386fed316131e3a0',
		'efa4e41fcd8c0f6a568d5d274121095ff9676e6c1eb04fdd679d9c119669d778',
		'8cdc727af11f390abd2323aac291c11054ac64352cdd9b5218afcd3e8d6fab45',
		'7fe6b268c08631ac118a149b92348e02c640d1804291cc695f89f41ff9aeaee6',
		'c093736a225e9f315fc3af8ca018f6e9b0a1c7c9974b116bfa63e2b7faf96a71',
		'dce7f41e3db51656ffc97259ca0ef3358cbfb41ac3e74e2dd9cd8639ab4996a0',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2b.sum256, blake2b.block_size).hex()
		assert result == blake2b_256_expected_results[i]
	}
}

fn test_hmac_blake2b_160() {
	blake2b_160_expected_results := [
		'5e9e7dc73976e03bbb71ea70d4bebf692c49ecbe',
		'c4728ccc2f7600f89d9a480514910b9f5daf3dd6',
		'b178e2dfd75d74645c326fac538758d53e305d56',
		'253d365a8930b7ff30ae85e862c464b7309056f8',
		'a41030927a208f3234251c8ca54731adc58c3c81',
		'e8f1b365fd6a63ca64e306901604aa33ea018c8b',
		'fc5fb8ec933174d97c7712fa8f8802467ac42b1e',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake2b.sum160, blake2b.block_size).hex()
		assert result == blake2b_160_expected_results[i]
	}
}

fn test_hmac_blake3_256() {
	blake3_256_expected_results := [
		'e09d0600c83f2e8e7c6a3fd4758c95afa196dfab6adb9f408768349624779c52',
		'732da99ccc24e277b2fec6c42e0f29f1093689ff0821de4df22f7faec5168776',
		'93411058fa4acdbdb1661e487713cfa2b1196494360fdbe10759e1d632c8c52e',
		'ec84dec849126b9085c6e674d589d8eb830d9b892008cc60a2e91588c506876d',
		'694fa508d2617fd490e80b0b5059df79811c4522e9d10d1fdf159fc75c80db2b',
		'b097a8fc0bcc3e23bcbaaa200c17d64f5823be05cc222544bfcc5113cda695fe',
		'dac8165b07656b282c5b9f2f2cf22569560778cb6240b11a383f2bf466f1ba36',
	]
	mut result := ''
	for i, key in keys {
		result = new(key, data[i], blake3.sum256, blake3.block_size).hex()
		assert result == blake3_256_expected_results[i]
	}
}

fn test_hmac_sha3_512() {
	sha3_512_expected_results := [
		'd2d9588c7e7886b08e09b56a7ac9d7e30a4badf13b37a041f5dfde34d87c086b5db1a7ec679bcfce81fa2eee982573c01dfb8d988e302f78d7b20d7d7ac2dfd7',
		'5a4bfeab6166427c7a3647b747292b8384537cdb89afb3bf5665e4c5e709350b287baec921fd7ca0ee7a0c31d022a95e1fc92ba9d77df883960275beb4e62024',
		'f25055024a17dfe15a25d6c40b00f45e8548f641844f2288170430ba0b7889bfaf04d9398121d165375300fe813f3cb6db9639921dcfb712b9177b8f5261d474',
		'b27eab1d6e8d87461c29f7f5739dd58e98aa35f8e823ad38c5492a2088fa0281993bbfff9a0e9c6bf121ae9ec9bb09d84a5ebac817182ea974673fb133ca0d1d',
		'69e9553223ede3637f08f9cc01ea9ded8f3b4202b5cc1feb60071e195a942f0ca0fa1cd70d3f1f9f24b2e18057b3001e7d5160e61eb6099f75ea4e0d6b849bd2',
		'eea495d39d9a07154b1266b028e233b9fd84de884ac8f0578e679f095ef14da96d0a355ed4738565884aec755c1b3f5ff09a918b437f6526e17dd8e77f425b95',
		'1488670c683959b5304fa17c172bea81724a249b44981a3eb52cfc66ff0758b7cd1204745131b8adbc714db7fc4550ce26af5f2326067ad1e699f05cae8bb792',
	]

	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha3.sum512, sha3.rate_512).hex()
		assert result == sha3_512_expected_results[i]
	}
}

fn test_hmac_sha3_384() {
	sha3_384_expected_results := [
		'b34fdb255dc7fb7f0c4bb2c1caeb0379b81ece60ec1b3cb2c5ec509141fcb77ca16d1e06f93049734be4948e24b932e3',
		'f1101f8cbf9766fd6764d2ed61903f21ca9b18f57cf3e1a23ca13508a93243ce48c045dc007f26a21b3f5e0e9df4c20a',
		'5bd8a0b98f9f4201eaec41d01fd1e274c266a2517527c1879b0460a692e1a430aefb82f0c9aea33406582ffeeef0bba6',
		'3a5d7a879702c086bc96d1dd8aa15d9c46446b95521311c606fdc4e308f4b984da2d0f9449b3ba8425ec7fb8c31bc136',
		'0cdfc206fd95ca1f27e8e8bd443164814460ca50f8d34d776b18f9eb300231a3d5bace731f694a59faa84c2e4ae7e235',
		'7172a2a2bb002c22669a2f85b8faaacfcc4e8a19d47ef5ee7a97f79bf21e1d89403ab3768b43929f12eded01e3ddd604',
		'45081e207f796f372aff5a098249f52d045e350ed5c805b3445a79ad0d4931c4b86d41bd1bb2ac935d1b32c344d56709',
	]

	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha3.sum384, sha3.rate_384).hex()
		assert result == sha3_384_expected_results[i]
	}
}

fn test_hmac_sha3_256() {
	sha3_256_expected_results := [
		'874d1d4e6e8302439bf707052e5d787d92bffcf0715853784e30da740a81e198',
		'c7d4072e788877ae3596bbb0da73b887c9171f93095b294ae857fbe2645e1ba5',
		'b55008323817b4df9398f32fd09d3ce624a3ac2a4f329c3b750c47647990de2a',
		'57366a45e2305321a4bc5aa5fe2ef8a921f6af8273d7fe7be6cfedb3f0aea6d7',
		'a0cd54f140b61480cd22120d600e30c2508c4ae0d335fd69770f2b4ddc80cd19',
		'016a1a59d67944c350d992a9bc1e8e7f6d1ace9c9ff6be92eda103961fe897ab',
		'415c2b5cde6b2aecd637fa2384aa87e5a0b0c5bc20d53550bbac5474b18769bf',
	]

	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha3.sum256, sha3.rate_256).hex()
		assert result == sha3_256_expected_results[i]
	}
}

fn test_hmac_sha3_224() {
	sha3_224_expected_results := [
		'f68da7f7bf577de799bb1224b7acfef9e8de015a63475ed5904a4693',
		'7fdb8dd88bd2f60d1b798634ad386811c2cfc85bfaf5d52bbace5e66',
		'3c9b90dbbd88c2af888fb1b43ec9d424c7fbf0d2b9d0140952b110b5',
		'a9d7685a19c4e0dbd9df2556cc8a7d2a7733b67625ce594c78270eeb',
		'f865c4fe082e4dd1873a9d83e1ca3bf827c3256d91274574a8b66f13',
		'852c3fb04b18a04df20c007e608027c44230fdd440cf7a50a0bc4fd9',
		'14db797c7f4c69fd1d4c0ababeb9f90971fc62622cc7852dee156265',
	]

	mut result := ''
	for i, key in keys {
		result = new(key, data[i], sha3.sum224, sha3.rate_224).hex()
		assert result == sha3_224_expected_results[i]
	}
}

fn test_hmac_equal() {
	mac1_1 := '7641c48a3b4aa8f887c07b3e83f96affb89c978fed8c96fcbbf4ad596eebfe496f9f16da6cd080ba393c6f365ad72b50d15c71bfb1d6b81f66a911786c6ce932'.bytes()
	mac1_2 := '7641c48a3b4aa8f887c07b3e83f96affb89c978fed8c96fcbbf4ad596eebfe496f9f16da6cd080ba393c6f365ad72b50d15c71bfb1d6b81f66a911786c6ce932'.bytes()
	mac2_1 := '164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737'.bytes()
	mac2_2 := '164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737'.bytes()
	assert equal(mac1_1, mac1_2)
	assert equal(mac2_1, mac2_2)
	assert !equal(mac1_1, mac2_1)
	assert !equal(mac1_1, mac2_2)
}
