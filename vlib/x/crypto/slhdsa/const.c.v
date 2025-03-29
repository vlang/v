// Copyright (c) blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module slhdsa

// The SLH-DSA functionality was added in recent OpenSSL 3.5 version.
// So, we would panic if this module run on unmeet version on the current stock.
fn init() {
	if C.OpenSSL_version_num() < u64(0x030500000) {
		panic('Your OpenSSL version ${C.OpenSSL_version_num().hex()} does not support for SLH-DSA')
	}
}

// #define LN_SLH_DSA_SHA2_128s            "SLH-DSA-SHA2-128s"
// #define NID_SLH_DSA_SHA2_128s           1460
const nid_slhdsa_sha2_128s = C.NID_SLH_DSA_SHA2_128s
const ln_slhdsa_sha2_128s = &char(C.LN_SLH_DSA_SHA2_128s)

// #define LN_SLH_DSA_SHA2_128f            "SLH-DSA-SHA2-128f"
// #define NID_SLH_DSA_SHA2_128f           1461
const nid_slhdsa_sha2_128f = C.NID_SLH_DSA_SHA2_128f
const ln_slhdsa_sha2_128f = &char(C.LN_SLH_DSA_SHA2_128f)

// #define LN_SLH_DSA_SHA2_192s            "SLH-DSA-SHA2-192s"
// #define NID_SLH_DSA_SHA2_192s           1462
const ln_slhdsa_sha2_192s = &char(C.LN_SLH_DSA_SHA2_192s)
const nid_slhdsa_sha2_192s = C.NID_SLH_DSA_SHA2_192s

// #define LN_SLH_DSA_SHA2_192f            "SLH-DSA-SHA2-192f"
// #define NID_SLH_DSA_SHA2_192f           1463
const ln_slhdsa_sha2_192f = &char(C.LN_SLH_DSA_SHA2_192f)
const nid_slhdsa_sha2_192f = C.NID_SLH_DSA_SHA2_192f

// #define LN_SLH_DSA_SHA2_256s            "SLH-DSA-SHA2-256s"
// #define NID_SLH_DSA_SHA2_256s           1464
const ln_slhdsa_sha2_256s = &char(C.LN_SLH_DSA_SHA2_256s)
const nid_slhdsa_sha2_256s = C.NID_SLH_DSA_SHA2_256s

// #define LN_SLH_DSA_SHA2_256f            "SLH-DSA-SHA2-256f"
// #define NID_SLH_DSA_SHA2_256f           1465
const ln_slhdsa_sha2_256f = &char(C.LN_SLH_DSA_SHA2_256f)
const nid_slhdsa_sha2_256f = C.NID_SLH_DSA_SHA2_256f

// #define LN_SLH_DSA_SHAKE_128s           "SLH-DSA-SHAKE-128s"
// #define NID_SLH_DSA_SHAKE_128s          1466
const ln_slhdsa_shake_128s = &char(C.LN_SLH_DSA_SHAKE_128s)
const nid_slhdsa_shake_128s = C.NID_SLH_DSA_SHAKE_128s

// #define LN_SLH_DSA_SHAKE_128f           "SLH-DSA-SHAKE-128f"
// #define NID_SLH_DSA_SHAKE_128f          1467
const ln_slhdsa_shake_128f = &char(C.LN_SLH_DSA_SHAKE_128f)
const nid_slhdsa_shake_128f = C.NID_SLH_DSA_SHAKE_128f

// #define LN_SLH_DSA_SHAKE_192s           "SLH-DSA-SHAKE-192s"
// #define NID_SLH_DSA_SHAKE_192s          1468
const ln_slhdsa_shake_192s = &char(C.LN_SLH_DSA_SHAKE_192s)
const nid_slhdsa_shake_192s = C.NID_SLH_DSA_SHAKE_192s

// #define LN_SLH_DSA_SHAKE_192f           "SLH-DSA-SHAKE-192f"
// #define NID_SLH_DSA_SHAKE_192f          1469
const ln_slhdsa_shake_192f = &char(C.LN_SLH_DSA_SHAKE_192f)
const nid_slhdsa_shake_192f = C.NID_SLH_DSA_SHAKE_192f

// #define LN_SLH_DSA_SHAKE_256s           "SLH-DSA-SHAKE-256s"
// #define NID_SLH_DSA_SHAKE_256s          1470
const ln_slhdsa_shake_256s = &char(C.LN_SLH_DSA_SHAKE_256s)
const nid_slhdsa_shake_256s = C.NID_SLH_DSA_SHAKE_256s

// #define LN_SLH_DSA_SHAKE_256f           "SLH-DSA-SHAKE-256f"
// #define NID_SLH_DSA_SHAKE_256f          1471
const ln_slhdsa_shake_256f = &char(C.LN_SLH_DSA_SHAKE_256f)
const nid_slhdsa_shake_256f = C.NID_SLH_DSA_SHAKE_256f

const evp_pkey_keypair = C.EVP_PKEY_KEYPAIR
const evp_pkey_public_key = C.EVP_PKEY_PUBLIC_KEY
