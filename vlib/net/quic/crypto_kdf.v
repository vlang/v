module quic

// HKDF key derivation for QUIC initial secrets and traffic keys.

// EVP_PKEY_HKDF is the NID for HKDF key derivation (OpenSSL NID_hkdf = 1036).
const evp_pkey_hkdf = 1036

// HKDF C function declarations

fn C.EVP_sha256() &EVP_MD
fn C.EVP_sha384() &EVP_MD
fn C.EVP_PKEY_CTX_new_id(id int, e voidptr) EVP_PKEY_CTX
fn C.EVP_PKEY_CTX_free(ctx EVP_PKEY_CTX)
fn C.EVP_PKEY_derive_init(ctx EVP_PKEY_CTX) int
fn C.EVP_PKEY_CTX_hkdf_mode(ctx EVP_PKEY_CTX, mode int) int
fn C.EVP_PKEY_CTX_set_hkdf_md(ctx EVP_PKEY_CTX, md &EVP_MD) int
fn C.EVP_PKEY_CTX_set1_hkdf_salt(ctx EVP_PKEY_CTX, salt &u8, saltlen int) int
fn C.EVP_PKEY_CTX_set1_hkdf_key(ctx EVP_PKEY_CTX, key &u8, keylen int) int
fn C.EVP_PKEY_CTX_add1_hkdf_info(ctx EVP_PKEY_CTX, info &u8, infolen int) int
fn C.EVP_PKEY_derive(ctx EVP_PKEY_CTX, key &u8, keylen &u64) int

// derive_initial_secrets derives initial QUIC secrets per RFC 9001 §5.2.
pub fn derive_initial_secrets(dcid []u8, is_server bool) !([]u8, []u8) {
	// QUIC initial salt per RFC 9001 Section 5.2 (QUIC version 1)
	initial_salt := [u8(0x38), 0x76, 0x2c, 0xf7, 0xf5, 0x59, 0x34, 0xb3, 0x4d, 0x17, 0x9a, 0xe6,
		0xa4, 0xc8, 0x0c, 0xad, 0xcc, 0xbb, 0x7f, 0x0a]

	initial_secret := hkdf_extract(initial_salt, dcid)!

	client_label := 'client in'.bytes()
	server_label := 'server in'.bytes()

	client_secret := hkdf_expand_label(initial_secret, client_label, []u8{}, 32)!
	server_secret := hkdf_expand_label(initial_secret, server_label, []u8{}, 32)!

	if is_server {
		return server_secret, client_secret
	} else {
		return client_secret, server_secret
	}
}

// hkdf_extract performs HKDF-Extract.
fn hkdf_extract(salt []u8, ikm []u8) ![]u8 {
	pctx := C.EVP_PKEY_CTX_new_id(evp_pkey_hkdf, unsafe { nil })
	if pctx == unsafe { nil } {
		return error('failed to create PKEY context')
	}
	defer {
		C.EVP_PKEY_CTX_free(pctx)
	}

	if C.EVP_PKEY_derive_init(pctx) != 1 {
		return error('failed to init derive')
	}

	if C.EVP_PKEY_CTX_hkdf_mode(pctx, 1) != 1 { // EVP_PKEY_HKDEF_MODE_EXTRACT_ONLY
		return error('failed to set HKDF mode')
	}

	md := C.EVP_sha256()
	if C.EVP_PKEY_CTX_set_hkdf_md(pctx, md) != 1 {
		return error('failed to set hash')
	}

	if C.EVP_PKEY_CTX_set1_hkdf_salt(pctx, salt.data, salt.len) != 1 {
		return error('failed to set salt')
	}

	if C.EVP_PKEY_CTX_set1_hkdf_key(pctx, ikm.data, ikm.len) != 1 {
		return error('failed to set key')
	}

	mut out := []u8{len: 32}
	mut outlen := u64(32)
	if C.EVP_PKEY_derive(pctx, out.data, &outlen) != 1 {
		return error('failed to derive')
	}

	return out[..int(outlen)]
}

// build_hkdf_label constructs a TLS 1.3 HkdfLabel structure (RFC 8446 §7.1)
// for use as the info parameter in HKDF-Expand.
fn build_hkdf_label(label []u8, context []u8, length int) []u8 {
	mut hkdf_label := []u8{}

	hkdf_label << u8(length >> 8)
	hkdf_label << u8(length)

	mut full_label := 'tls13 '.bytes()
	full_label << label
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label

	hkdf_label << u8(context.len)
	if context.len > 0 {
		hkdf_label << context
	}

	return hkdf_label
}

fn hkdf_expand_label(secret []u8, label []u8, context []u8, length int) ![]u8 {
	hkdf_label := build_hkdf_label(label, context, length)

	pctx := C.EVP_PKEY_CTX_new_id(evp_pkey_hkdf, unsafe { nil })
	if pctx == unsafe { nil } {
		return error('failed to create PKEY context')
	}
	defer {
		C.EVP_PKEY_CTX_free(pctx)
	}

	if C.EVP_PKEY_derive_init(pctx) != 1 {
		return error('failed to init derive')
	}

	if C.EVP_PKEY_CTX_hkdf_mode(pctx, 2) != 1 { // EVP_PKEY_HKDEF_MODE_EXPAND_ONLY
		return error('failed to set HKDF mode')
	}

	md := C.EVP_sha256()
	if C.EVP_PKEY_CTX_set_hkdf_md(pctx, md) != 1 {
		return error('failed to set hash')
	}

	if C.EVP_PKEY_CTX_set1_hkdf_key(pctx, secret.data, secret.len) != 1 {
		return error('failed to set key')
	}

	if C.EVP_PKEY_CTX_add1_hkdf_info(pctx, hkdf_label.data, hkdf_label.len) != 1 {
		return error('failed to set info')
	}

	mut out := []u8{len: length}
	mut outlen := u64(length)
	if C.EVP_PKEY_derive(pctx, out.data, &outlen) != 1 {
		return error('failed to derive')
	}

	return out[..int(outlen)]
}

// derive_traffic_keys derives AES-128-GCM keys and IVs per RFC 9001 §5.1.
pub fn (mut ctx CryptoContext) derive_traffic_keys() ! {
	if ctx.tx_secret.len == 0 {
		return error('tx_secret is empty: set traffic secrets before deriving keys')
	}
	if ctx.rx_secret.len == 0 {
		return error('rx_secret is empty: set traffic secrets before deriving keys')
	}

	ctx.tx_key = hkdf_expand_label(ctx.tx_secret, 'quic key'.bytes(), []u8{}, 16)!
	ctx.tx_iv = hkdf_expand_label(ctx.tx_secret, 'quic iv'.bytes(), []u8{}, 12)!
	ctx.rx_key = hkdf_expand_label(ctx.rx_secret, 'quic key'.bytes(), []u8{}, 16)!
	ctx.rx_iv = hkdf_expand_label(ctx.rx_secret, 'quic iv'.bytes(), []u8{}, 12)!
}
