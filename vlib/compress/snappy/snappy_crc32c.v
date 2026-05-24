// CRC32C — Castagnoli variant used by the Snappy framing format.
//
// The lookup table is pre-computed at compile time (reflected polynomial
// 0x82F63B78). No runtime initialisation is needed.

module snappy

// crc32c_table holds all 256 pre-computed CRC32C remainders.
// Generated with polynomial 0x82F63B78 (reflected form of 0x1EDC6F41).
const crc32c_table = [u32(0x00000000), u32(0xf26b8303), u32(0xe13b70f7), u32(0x1350f3f4),
	u32(0xc79a971f), u32(0x35f1141c), u32(0x26a1e7e8), u32(0xd4ca64eb), u32(0x8ad958cf),
	u32(0x78b2dbcc), u32(0x6be22838), u32(0x9989ab3b), u32(0x4d43cfd0), u32(0xbf284cd3),
	u32(0xac78bf27), u32(0x5e133c24), u32(0x105ec76f), u32(0xe235446c), u32(0xf165b798),
	u32(0x030e349b), u32(0xd7c45070), u32(0x25afd373), u32(0x36ff2087), u32(0xc494a384),
	u32(0x9a879fa0), u32(0x68ec1ca3), u32(0x7bbcef57), u32(0x89d76c54), u32(0x5d1d08bf),
	u32(0xaf768bbc), u32(0xbc267848), u32(0x4e4dfb4b), u32(0x20bd8ede), u32(0xd2d60ddd),
	u32(0xc186fe29), u32(0x33ed7d2a), u32(0xe72719c1), u32(0x154c9ac2), u32(0x061c6936),
	u32(0xf477ea35), u32(0xaa64d611), u32(0x580f5512), u32(0x4b5fa6e6), u32(0xb93425e5),
	u32(0x6dfe410e), u32(0x9f95c20d), u32(0x8cc531f9), u32(0x7eaeb2fa), u32(0x30e349b1),
	u32(0xc288cab2), u32(0xd1d83946), u32(0x23b3ba45), u32(0xf779deae), u32(0x05125dad),
	u32(0x1642ae59), u32(0xe4292d5a), u32(0xba3a117e), u32(0x4851927d), u32(0x5b016189),
	u32(0xa96ae28a), u32(0x7da08661), u32(0x8fcb0562), u32(0x9c9bf696), u32(0x6ef07595),
	u32(0x417b1dbc), u32(0xb3109ebf), u32(0xa0406d4b), u32(0x522bee48), u32(0x86e18aa3),
	u32(0x748a09a0), u32(0x67dafa54), u32(0x95b17957), u32(0xcba24573), u32(0x39c9c670),
	u32(0x2a993584), u32(0xd8f2b687), u32(0x0c38d26c), u32(0xfe53516f), u32(0xed03a29b),
	u32(0x1f682198), u32(0x5125dad3), u32(0xa34e59d0), u32(0xb01eaa24), u32(0x42752927),
	u32(0x96bf4dcc), u32(0x64d4cecf), u32(0x77843d3b), u32(0x85efbe38), u32(0xdbfc821c),
	u32(0x2997011f), u32(0x3ac7f2eb), u32(0xc8ac71e8), u32(0x1c661503), u32(0xee0d9600),
	u32(0xfd5d65f4), u32(0x0f36e6f7), u32(0x61c69362), u32(0x93ad1061), u32(0x80fde395),
	u32(0x72966096), u32(0xa65c047d), u32(0x5437877e), u32(0x4767748a), u32(0xb50cf789),
	u32(0xeb1fcbad), u32(0x197448ae), u32(0x0a24bb5a), u32(0xf84f3859), u32(0x2c855cb2),
	u32(0xdeeedfb1), u32(0xcdbe2c45), u32(0x3fd5af46), u32(0x7198540d), u32(0x83f3d70e),
	u32(0x90a324fa), u32(0x62c8a7f9), u32(0xb602c312), u32(0x44694011), u32(0x5739b3e5),
	u32(0xa55230e6), u32(0xfb410cc2), u32(0x092a8fc1), u32(0x1a7a7c35), u32(0xe811ff36),
	u32(0x3cdb9bdd), u32(0xceb018de), u32(0xdde0eb2a), u32(0x2f8b6829), u32(0x82f63b78),
	u32(0x709db87b), u32(0x63cd4b8f), u32(0x91a6c88c), u32(0x456cac67), u32(0xb7072f64),
	u32(0xa457dc90), u32(0x563c5f93), u32(0x082f63b7), u32(0xfa44e0b4), u32(0xe9141340),
	u32(0x1b7f9043), u32(0xcfb5f4a8), u32(0x3dde77ab), u32(0x2e8e845f), u32(0xdce5075c),
	u32(0x92a8fc17), u32(0x60c37f14), u32(0x73938ce0), u32(0x81f80fe3), u32(0x55326b08),
	u32(0xa759e80b), u32(0xb4091bff), u32(0x466298fc), u32(0x1871a4d8), u32(0xea1a27db),
	u32(0xf94ad42f), u32(0x0b21572c), u32(0xdfeb33c7), u32(0x2d80b0c4), u32(0x3ed04330),
	u32(0xccbbc033), u32(0xa24bb5a6), u32(0x502036a5), u32(0x4370c551), u32(0xb11b4652),
	u32(0x65d122b9), u32(0x97baa1ba), u32(0x84ea524e), u32(0x7681d14d), u32(0x2892ed69),
	u32(0xdaf96e6a), u32(0xc9a99d9e), u32(0x3bc21e9d), u32(0xef087a76), u32(0x1d63f975),
	u32(0x0e330a81), u32(0xfc588982), u32(0xb21572c9), u32(0x407ef1ca), u32(0x532e023e),
	u32(0xa145813d), u32(0x758fe5d6), u32(0x87e466d5), u32(0x94b49521), u32(0x66df1622),
	u32(0x38cc2a06), u32(0xcaa7a905), u32(0xd9f75af1), u32(0x2b9cd9f2), u32(0xff56bd19),
	u32(0x0d3d3e1a), u32(0x1e6dcdee), u32(0xec064eed), u32(0xc38d26c4), u32(0x31e6a5c7),
	u32(0x22b65633), u32(0xd0ddd530), u32(0x0417b1db), u32(0xf67c32d8), u32(0xe52cc12c),
	u32(0x1747422f), u32(0x49547e0b), u32(0xbb3ffd08), u32(0xa86f0efc), u32(0x5a048dff),
	u32(0x8ecee914), u32(0x7ca56a17), u32(0x6ff599e3), u32(0x9d9e1ae0), u32(0xd3d3e1ab),
	u32(0x21b862a8), u32(0x32e8915c), u32(0xc083125f), u32(0x144976b4), u32(0xe622f5b7),
	u32(0xf5720643), u32(0x07198540), u32(0x590ab964), u32(0xab613a67), u32(0xb831c993),
	u32(0x4a5a4a90), u32(0x9e902e7b), u32(0x6cfbad78), u32(0x7fab5e8c), u32(0x8dc0dd8f),
	u32(0xe330a81a), u32(0x115b2b19), u32(0x020bd8ed), u32(0xf0605bee), u32(0x24aa3f05),
	u32(0xd6c1bc06), u32(0xc5914ff2), u32(0x37faccf1), u32(0x69e9f0d5), u32(0x9b8273d6),
	u32(0x88d28022), u32(0x7ab90321), u32(0xae7367ca), u32(0x5c18e4c9), u32(0x4f48173d),
	u32(0xbd23943e), u32(0xf36e6f75), u32(0x0105ec76), u32(0x12551f82), u32(0xe03e9c81),
	u32(0x34f4f86a), u32(0xc69f7b69), u32(0xd5cf889d), u32(0x27a40b9e), u32(0x79b737ba),
	u32(0x8bdcb4b9), u32(0x988c474d), u32(0x6ae7c44e), u32(0xbe2da0a5), u32(0x4c4623a6),
	u32(0x5f16d052), u32(0xad7d5351)]!

// crc32c returns the CRC-32C checksum of `data`.
pub fn crc32c(data []u8) u32 {
	mut crc := u32(0xffffffff)
	for b in data {
		crc = crc32c_table[(crc ^ u32(b)) & 0xff] ^ (crc >> 8)
	}
	return crc ^ 0xffffffff
}

// mask_crc applies the Snappy framing mask to a raw CRC32C value:
//   ((crc >> 15) | (crc << 17)) + 0xa282ead8
// This avoids accidental matches between the checksum and compressed data.
@[inline]
fn mask_crc(crc u32) u32 {
	return ((crc >> 15) | (crc << 17)) + u32(0xa282ead8)
}

// unmask_crc reverses the masking applied by mask_crc.
@[inline]
fn unmask_crc(masked u32) u32 {
	rot := masked - u32(0xa282ead8)
	return (rot >> 17) | (rot << 15)
}
