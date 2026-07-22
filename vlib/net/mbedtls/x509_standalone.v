module mbedtls

// build_certificate_chain parses a list of DER-encoded certificates
// (leaf-first, as TLS 1.3's Certificate message orders them — RFC 8446
// §4.4.2: "The sender's certificate MUST come in the first
// CertificateEntry in the list") into one mbedTLS certificate chain,
// entirely standalone: no mbedtls_ssl_context is constructed or needed
// (confirmed safe in x509_standalone_test.v). This is the call shape
// net.quic needs — QUIC carries the TLS 1.3 handshake over its own CRYPTO
// frames, bypassing mbedTLS's own SSL/record-layer state machine (and
// therefore the net.mbedtls.SSLConn-mediated path net.http's TLS clients
// use) entirely.
//
// The caller owns the returned chain and MUST call free_certificate_chain
// when done — mbedtls_x509_crt holds C-heap-allocated internal buffers
// with no GC visibility.
//
// DER bytes, NOT PEM: unlike this module's PEM-string helpers elsewhere
// (new_sslcerts_in_memory et al.), each `der_certs` entry is passed to
// mbedtls_x509_crt_parse with its EXACT length, no NUL-terminator byte
// appended. Confirmed against mbedTLS's own source (x509_crt.c): the
// PEM-vs-DER format sniff only checks buf[buflen-1]=='\0' combined with a
// "-----BEGIN CERTIFICATE-----" substring match; appending a NUL byte a
// real DER buffer doesn't have would be an out-of-bounds read one byte
// past a V []u8 slice's allocation, not just a harmless extra byte.
//
// This exact-length requirement is verified by SOURCE INSPECTION only, not
// by a passing test: empirically, mbedTLS's DER parser tolerates a
// too-long declared buflen for well-formed input (its ASN.1 SEQUENCE
// length is self-describing, so it simply stops reading where the
// structure says to) — passing der.len+1 here still passes every test in
// this file, because the extra out-of-bounds byte is real undefined
// behavior (adjacent-heap-memory dependent, not a guaranteed crash or
// parse failure) rather than something a functional test can observe.
// Trust the source-level reasoning above, not test results, for this one.
// Also confirmed mbedtls_x509_crt_parse_der always copies the buffer
// (own_buffer=1) rather than retaining a pointer into it, so the parsed
// chain has no dangling reference back into `der_certs` once this
// function returns.
//
// Repeated calls append, they don't overwrite: mbedtls_x509_crt_parse_der
// _internal walks to the existing tail (while crt->version != 0 &&
// crt->next != NULL) and allocates+links a new node there before parsing
// into it, so calling this in a loop on the same `chain` correctly builds
// a multi-certificate chain rather than clobbering earlier entries. This
// is source-verified (x509_crt.c), not test-verified — every test using
// this function so far passes only a single certificate (this codebase
// has one real test cert fixture available); a genuine 2+-certificate
// functional test needs a second cert and is deliberately deferred, not
// silently skipped.
pub fn build_certificate_chain(der_certs [][]u8) !&C.mbedtls_x509_crt {
	if der_certs.len == 0 {
		return error('net.mbedtls: cannot build a certificate chain from an empty list')
	}
	mut chain := &C.mbedtls_x509_crt{}
	C.mbedtls_x509_crt_init(chain)
	for i, der in der_certs {
		ret := C.mbedtls_x509_crt_parse(chain, der.data, usize(der.len))
		if ret != 0 {
			C.mbedtls_x509_crt_free(chain)
			return error_with_code('net.mbedtls: failed to parse certificate ${i} of ${der_certs.len} in the chain, mbedtls ret: ${ret}',
				ret)
		}
	}
	return chain
}

// free_certificate_chain releases a chain returned by
// build_certificate_chain. Like every other mbedTLS resource in this
// module, calling it twice on the same chain is a double-free — callers
// own exactly one matching free_certificate_chain call per
// build_certificate_chain call.
pub fn free_certificate_chain(chain &C.mbedtls_x509_crt) {
	C.mbedtls_x509_crt_free(chain)
}

// MbedtlsMdType names the subset of mbedtls_md_type_t (md.h) this module's
// signature-verification functions accept, as the exact enum values mbedTLS
// itself defines (confirmed against the vendored header, not assumed) --
// TLS 1.3's three mandatory-to-implement hash algorithms for
// CertificateVerify (RFC 8446 §4.2.3: ecdsa_secp256r1_sha256 and the three
// rsa_pss_rsae_* schemes) and nothing else, since nothing else is used
// anywhere in this codebase yet.
pub enum MbedtlsMdType {
	sha256 = 0x09
	sha384 = 0x0a
	sha512 = 0x0b
}

// mbedtls_pk_type_t values (pk.h) needed to select mbedtls_pk_verify_ext's
// signature-scheme dispatch. Not the full enum -- only these two are ever
// used here (mbedtls_pk_verify_ext independently determines RSA-vs-EC
// compatibility from the key itself via mbedtls_pk_can_do; these values only
// pick which SIGNATURE ALGORITHM to check the key against).
const pk_type_ecdsa = i32(4)
const pk_type_rsassa_pss = i32(6)

// get_leaf_public_key returns the public key of `chain`'s HEAD certificate
// -- the "leaf"/end-entity certificate. build_certificate_chain always
// parses the sender's own certificate first (RFC 8446 §4.4.2's own wire
// order: "The sender's certificate MUST come in the first CertificateEntry
// in the list"), and mbedtls_x509_crt_parse's chain-append walks forward
// from that head node to link subsequent certificates -- so `chain` itself,
// unwalked, already points at the leaf. The returned pointer's lifetime is
// tied to `chain`: do not call this after free_certificate_chain.
pub fn get_leaf_public_key(chain &C.mbedtls_x509_crt) &C.mbedtls_pk_context {
	return C.v_mbedtls_x509_crt_get_pk(chain)
}

// verify_ecdsa_signature checks an ECDSA signature over `hash` -- a digest
// the CALLER has already computed using the algorithm `md_alg` names; this
// function does not hash `hash` itself. `pk` may be an MBEDTLS_PK_ECKEY
// context (the type produced by parsing an EC certificate, not specifically
// MBEDTLS_PK_ECDSA) -- confirmed against pk_wrap.c's eckey_can_do(), which
// explicitly accepts MBEDTLS_PK_ECDSA verification requests against an
// MBEDTLS_PK_ECKEY-typed key, not assumed from the type names alone.
pub fn verify_ecdsa_signature(pk &C.mbedtls_pk_context, md_alg MbedtlsMdType, hash []u8, signature []u8) ! {
	ret := C.mbedtls_pk_verify_ext(pk_type_ecdsa, unsafe { nil }, pk, i32(md_alg), hash.data,
		usize(hash.len), signature.data, usize(signature.len))
	if ret != 0 {
		return error_with_code('net.mbedtls: ECDSA signature verification failed, mbedtls ret: ${ret}',
			ret)
	}
}

// verify_rsa_pss_signature checks an RSASSA-PSS signature the same way,
// with the salt length pinned to exactly `hash.len` (not
// MBEDTLS_RSA_SALT_LEN_ANY) -- RFC 8446 §4.2.3 mandates this for TLS 1.3's
// rsa_pss_rsae_* schemes ("the length of the Salt MUST equal the length of
// the digest algorithm"), and this vendored build enforces it rather than
// silently ignoring it: MBEDTLS_USE_PSA_CRYPTO is disabled (confirmed in
// mbedtls_config.h, not assumed), so mbedtls_pk_verify_ext's documented
// "salt length not verified under PSA crypto" caveat does not apply here.
// The check itself is real, not a no-op: rsa.c's rsa_rsassa_pss_verify_ext
// rejects a mismatch outright ("if (expected_salt_len !=
// MBEDTLS_RSA_SALT_LEN_ANY && observed_salt_len != (size_t)
// expected_salt_len) { ... fail }") whenever a specific length (not
// MBEDTLS_RSA_SALT_LEN_ANY) is supplied -- confirmed by reading rsa.c, not
// assumed from the header comment alone. mbedTLS's own TLS 1.3
// implementation sets expected_salt_len the identical way for the identical
// reason (ssl_tls13_generic.c: `rsassa_pss_options.expected_salt_len =
// PSA_HASH_LENGTH(hash_alg)`), so this isn't a novel usage pattern.
pub fn verify_rsa_pss_signature(pk &C.mbedtls_pk_context, md_alg MbedtlsMdType, hash []u8, signature []u8) ! {
	mut opts := C.mbedtls_pk_rsassa_pss_options{
		mgf1_hash_id:      int(md_alg)
		expected_salt_len: hash.len
	}
	ret := C.mbedtls_pk_verify_ext(pk_type_rsassa_pss, &opts, pk, i32(md_alg), hash.data,
		usize(hash.len), signature.data, usize(signature.len))
	if ret != 0 {
		return error_with_code('net.mbedtls: RSA-PSS signature verification failed, mbedtls ret: ${ret}',
			ret)
	}
}

// verify_certificate_chain validates `chain` (from build_certificate_chain)
// against `ca_bundle_pem`, one or more trusted CA certificates concatenated
// in PEM format. This mirrors SSLConnectConfig.verify's existing contract
// in this same module: the caller supplies the trust anchor explicitly —
// there is no OS trust-store lookup anywhere in this codebase today, for
// any TLS client (HTTP/1.1, HTTP/2, or this QUIC path).
//
// Hostname verification (matching the leaf certificate's SAN/CN against
// the name the client actually connected to) is deliberately NOT done
// here — it needs the SNI hostname the caller sent, which this function
// has no way to know. net.quic's caller must do that check itself using
// the returned success/failure plus its own hostname, mirroring how
// mbedtls_x509_crt_verify's own `cn` parameter would normally be supplied
// inside a full SSL handshake (which this standalone call deliberately
// isn't).
pub fn verify_certificate_chain(chain &C.mbedtls_x509_crt, ca_bundle_pem string) ! {
	mut ca_chain := C.mbedtls_x509_crt{}
	C.mbedtls_x509_crt_init(&ca_chain)
	defer {
		C.mbedtls_x509_crt_free(&ca_chain)
	}
	// ca_bundle_pem.str (not .bytes()): mbedtls_x509_crt_parse's PEM path
	// needs a real NUL-terminated buffer (it scans for
	// "-----BEGIN CERTIFICATE-----" and buf[buflen-1]=='\0'). A V string's
	// own .str buffer is always NUL-terminated internally for C interop,
	// but .bytes() allocates a fresh []u8 of exactly .len bytes with no
	// such trailing byte -- passing pem.len + 1 with a .bytes()-derived
	// buffer reads one byte past the allocation (confirmed via ASan:
	// heap-buffer-overflow in mbedtls_x509_crt_parse). Matches every other
	// PEM-parsing call site's own convention in this module
	// (new_sslcerts_in_memory_with_rng et al., which all pass `<string>.str`).
	parse_ret := C.mbedtls_x509_crt_parse(&ca_chain, ca_bundle_pem.str,
		usize(ca_bundle_pem.len + 1))
	// != 0, not < 0: for a PEM bundle specifically (unlike the DER path in
	// build_certificate_chain), mbedtls_x509_crt_parse can return a
	// POSITIVE count of certificates it failed to parse when others in the
	// same bundle succeeded. Treating that as success would silently trust
	// a caller-supplied CA bundle that's partially malformed/corrupted.
	// Matches every other PEM-parsing call site's own convention in this
	// module (new_sslcerts_in_memory_with_rng et al., all `!= 0`).
	if parse_ret != 0 {
		return error_with_code('net.mbedtls: failed to parse CA bundle, mbedtls ret: ${parse_ret}',
			parse_ret)
	}

	mut flags := u32(0)
	verify_ret := C.mbedtls_x509_crt_verify(chain, &ca_chain, unsafe { nil }, unsafe { nil },
		&flags, unsafe { nil }, unsafe { nil })
	if verify_ret != 0 {
		return error_with_code('net.mbedtls: certificate chain verification failed, mbedtls ret: ${verify_ret}, flags: 0x${flags:x}',
			verify_ret)
	}
}
