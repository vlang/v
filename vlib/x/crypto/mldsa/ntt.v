// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

// appendix b: zeta^BitRev8(k) mod q in montgomery domain
const zetas = [u32(4193792), 25847, 5771523, 7861508, 237124, 7602457, 7504169, 466468, 1826347,
	2353451, 8021166, 6288512, 3119733, 5495562, 3111497, 2680103, 2725464, 1024112, 7300517,
	3585928, 7830929, 7260833, 2619752, 6271868, 6262231, 4520680, 6980856, 5102745, 1757237,
	8360995, 4010497, 280005, 2706023, 95776, 3077325, 3530437, 6718724, 4788269, 5842901,
	3915439, 4519302, 5336701, 3574422, 5512770, 3539968, 8079950, 2348700, 7841118, 6681150,
	6736599, 3505694, 4558682, 3507263, 6239768, 6779997, 3699596, 811944, 531354, 954230,
	3881043, 3900724, 5823537, 2071892, 5582638, 4450022, 6851714, 4702672, 5339162, 6927966,
	3475950, 2176455, 6795196, 7122806, 1939314, 4296819, 7380215, 5190273, 5223087, 4747489,
	126922, 3412210, 7396998, 2147896, 2715295, 5412772, 4686924, 7969390, 5903370, 7709315,
	7151892, 8357436, 7072248, 7998430, 1349076, 1852771, 6949987, 5037034, 264944, 508951,
	3097992, 44288, 7280319, 904516, 3958618, 4656075, 8371839, 1653064, 5130689, 2389356,
	8169440, 759969, 7063561, 189548, 4827145, 3159746, 6529015, 5971092, 8202977, 1315589,
	1341330, 1285669, 6795489, 7567685, 6940675, 5361315, 4499357, 4751448, 3839961, 2091667,
	3407706, 2316500, 3817976, 5037939, 2244091, 5933984, 4817955, 266997, 2434439, 7144689,
	3513181, 4860065, 4621053, 7183191, 5187039, 900702, 1859098, 909542, 819034, 495491,
	6767243, 8337157, 7857917, 7725090, 5257975, 2031748, 3207046, 4823422, 7855319, 7611795,
	4784579, 342297, 286988, 5942594, 4108315, 3437287, 5038140, 1735879, 203044, 2842341,
	2691481, 5790267, 1265009, 4055324, 1247620, 2486353, 1595974, 4613401, 1250494, 2635921,
	4832145, 5386378, 1869119, 1903435, 7329447, 7047359, 1237275, 5062207, 6950192, 7929317,
	1312455, 3306115, 6417775, 7100756, 1917081, 5834105, 7005614, 1500165, 777191, 2235880,
	3406031, 7838005, 5548557, 6709241, 6533464, 5796124, 4656147, 594136, 4603424, 6366809,
	2432395, 2454455, 8215696, 1957272, 3369112, 185531, 7173032, 5196991, 162844, 1616392,
	3014001, 810149, 1652634, 4686184, 6581310, 5341501, 3523897, 3866901, 269760, 2213111,
	7404533, 1717735, 472078, 7953734, 1723600, 6577327, 1910376, 6712985, 7276084, 8119771,
	4546524, 5441381, 6144432, 7959518, 6094090, 183443, 7403526, 1612842, 4834730, 7826001,
	3919660, 8332111, 7018208, 3937738, 1400424, 7534263, 1976782]!

// algo. 41: NTT (s. 7.5)
@[direct_array_access]
fn ntt(f_ RingElement) NttElement {
	mut f := f_
	mut m := u8(0)

	mut len := 128
	for len >= 8 {
		mut start := 0
		for start < 256 {
			m++
			zeta := FieldElement(zetas[m])

			mut j := 0
			for j < len {
				t := field_montgomery_mul(zeta, f[start + len + j])
				f[start + len + j] = field_sub(f[start + j], t)
				f[start + j] = field_add(f[start + j], t)

				t2 := field_montgomery_mul(zeta, f[start + len + j + 1])
				f[start + len + j + 1] = field_sub(f[start + j + 1], t2)
				f[start + j + 1] = field_add(f[start + j + 1], t2)

				j += 2
			}
			start += 2 * len
		}
		len /= 2
	}

	for start := 0; start < 256; start += 8 {
		m++
		zeta := FieldElement(zetas[m])

		mut t := field_montgomery_mul(zeta, f[start + 4])
		f[start + 4] = field_sub(f[start], t)
		f[start] = field_add(f[start], t)

		t = field_montgomery_mul(zeta, f[start + 5])
		f[start + 5] = field_sub(f[start + 1], t)
		f[start + 1] = field_add(f[start + 1], t)

		t = field_montgomery_mul(zeta, f[start + 6])
		f[start + 6] = field_sub(f[start + 2], t)
		f[start + 2] = field_add(f[start + 2], t)

		t = field_montgomery_mul(zeta, f[start + 7])
		f[start + 7] = field_sub(f[start + 3], t)
		f[start + 3] = field_add(f[start + 3], t)
	}

	for start := 0; start < 256; start += 4 {
		m++
		zeta := FieldElement(zetas[m])

		mut t := field_montgomery_mul(zeta, f[start + 2])
		f[start + 2] = field_sub(f[start], t)
		f[start] = field_add(f[start], t)

		t = field_montgomery_mul(zeta, f[start + 3])
		f[start + 3] = field_sub(f[start + 1], t)
		f[start + 1] = field_add(f[start + 1], t)
	}

	for start := 0; start < 256; start += 2 {
		m++
		zeta := FieldElement(zetas[m])

		t := field_montgomery_mul(zeta, f[start + 1])
		f[start + 1] = field_sub(f[start], t)
		f[start] = field_add(f[start], t)
	}

	return NttElement(f)
}

// algo. 42: NTT^-1 (s. 7.5)
@[direct_array_access]
fn inverse_ntt(f_ NttElement) RingElement {
	mut f := f_
	mut m := u8(255)

	for start := 0; start < 256; start += 2 {
		zeta := FieldElement(zetas[m])
		m--

		t := f[start]
		f[start] = field_add(t, f[start + 1])
		f[start + 1] = field_montgomery_mul_sub(zeta, f[start + 1], t)
	}

	for start := 0; start < 256; start += 4 {
		zeta := FieldElement(zetas[m])
		m--

		mut t := f[start]
		f[start] = field_add(t, f[start + 2])
		f[start + 2] = field_montgomery_mul_sub(zeta, f[start + 2], t)

		t = f[start + 1]
		f[start + 1] = field_add(t, f[start + 3])
		f[start + 3] = field_montgomery_mul_sub(zeta, f[start + 3], t)
	}

	for start := 0; start < 256; start += 8 {
		zeta := FieldElement(zetas[m])
		m--

		mut t := f[start]
		f[start] = field_add(t, f[start + 4])
		f[start + 4] = field_montgomery_mul_sub(zeta, f[start + 4], t)

		t = f[start + 1]
		f[start + 1] = field_add(t, f[start + 5])
		f[start + 5] = field_montgomery_mul_sub(zeta, f[start + 5], t)

		t = f[start + 2]
		f[start + 2] = field_add(t, f[start + 6])
		f[start + 6] = field_montgomery_mul_sub(zeta, f[start + 6], t)

		t = f[start + 3]
		f[start + 3] = field_add(t, f[start + 7])
		f[start + 7] = field_montgomery_mul_sub(zeta, f[start + 7], t)
	}

	mut len2 := 8
	for len2 < 256 {
		mut start := 0
		for start < 256 {
			zeta := FieldElement(zetas[m])
			m--

			mut j := 0
			for j < len2 {
				mut t := f[start + j]
				f[start + j] = field_add(t, f[start + len2 + j])
				f[start + len2 + j] = field_montgomery_mul_sub(zeta, f[start + len2 + j], t)

				t = f[start + j + 1]
				f[start + j + 1] = field_add(t, f[start + len2 + j + 1])
				f[start + len2 + j + 1] = field_montgomery_mul_sub(zeta, f[start + len2 + j + 1],
					t)

				j += 2
			}
			start += 2 * len2
		}
		len2 *= 2
	}

	// algo. 42, line 21: f = 8347681 = 256^-1 mod q; in montgomery: 16382
	for i in 0 .. 256 {
		f[i] = field_montgomery_mul(f[i], 16382)
	}
	return RingElement(f)
}
