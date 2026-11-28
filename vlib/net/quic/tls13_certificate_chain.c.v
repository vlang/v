module quic

import net.mbedtls
import crypto.sha256
import crypto.sha512

// Vendored mbedTLS enum values used only to translate its parsed X.509
// signature metadata into RFC 8446 SignatureScheme codepoints. They are kept
// here, beside the translation, instead of leaking C enums into the plain-V
// server handshake.
const mbedtls_pk_rsa = 1
const mbedtls_pk_eckey = 2
const mbedtls_pk_ecdsa = 4
const mbedtls_pk_rsassa_pss = 6
const mbedtls_md_sha256 = 0x09
const mbedtls_md_sha384 = 0x0a
const mbedtls_md_sha512 = 0x0b
const mbedtls_ecp_group_secp256r1 = 3
const mbedtls_ecp_group_secp384r1 = 4
const mbedtls_ecp_group_secp521r1 = 5
const sig_scheme_rsa_pss_pss_sha256 = u16(0x0809)
const sig_scheme_rsa_pss_pss_sha384 = u16(0x080a)
const sig_scheme_rsa_pss_pss_sha512 = u16(0x080b)

fn validate_certificate_pss_parameters(info mbedtls.CertificateSignatureInfo) ! {
	digest_len := match info.signature_digest_type {
		mbedtls_md_sha256 { 32 }
		mbedtls_md_sha384 { 48 }
		mbedtls_md_sha512 { 64 }
		else {
			return error('unsupported RSA-PSS certificate signature digest ${info.signature_digest_type}')
		}
	}
	if info.signature_pss_mgf1_digest != info.signature_digest_type {
		return error('RSA-PSS certificate signature MGF1 digest ${info.signature_pss_mgf1_digest} does not match signature digest ${info.signature_digest_type}')
	}
	if info.signature_pss_salt_len != digest_len {
		return error('RSA-PSS certificate signature salt length ${info.signature_pss_salt_len} does not match digest length ${digest_len}')
	}
}

fn certificate_signature_scheme(info mbedtls.CertificateSignatureInfo) !u16 {
	if !info.issuer_known {
		return error('the issuer certificate is absent, so its public-key type and curve cannot be determined')
	}
	match info.signature_public_key_type {
		mbedtls_pk_rsa {
			if info.issuer_public_key_type != mbedtls_pk_rsa {
				return error('an RSA-PKCS1 certificate signature has issuer public-key type ${info.issuer_public_key_type}, expected RSA')
			}
			return match info.signature_digest_type {
				mbedtls_md_sha256 { sig_scheme_rsa_pkcs1_sha256 }
				mbedtls_md_sha384 { sig_scheme_rsa_pkcs1_sha384 }
				mbedtls_md_sha512 { sig_scheme_rsa_pkcs1_sha512 }
				else {
					error('unsupported RSA-PKCS1 certificate signature digest ${info.signature_digest_type}')
				}
			}
		}
		mbedtls_pk_ecdsa {
			if info.issuer_public_key_type !in [mbedtls_pk_eckey, mbedtls_pk_ecdsa] {
				return error('an ECDSA certificate signature has non-EC issuer public-key type ${info.issuer_public_key_type}')
			}
			if info.issuer_public_key_curve_id == mbedtls_ecp_group_secp256r1
				&& info.signature_digest_type == mbedtls_md_sha256 {
				return sig_scheme_ecdsa_secp256r1_sha256
			}
			if info.issuer_public_key_curve_id == mbedtls_ecp_group_secp384r1
				&& info.signature_digest_type == mbedtls_md_sha384 {
				return sig_scheme_ecdsa_secp384r1_sha384
			}
			if info.issuer_public_key_curve_id == mbedtls_ecp_group_secp521r1
				&& info.signature_digest_type == mbedtls_md_sha512 {
				return sig_scheme_ecdsa_secp521r1_sha512
			}
			return error('unsupported ECDSA certificate signature curve ${info.issuer_public_key_curve_id} and digest ${info.signature_digest_type}')
		}
		mbedtls_pk_rsassa_pss {
			validate_certificate_pss_parameters(info)!
			if info.issuer_public_key_type == mbedtls_pk_rsa {
				return match info.signature_digest_type {
					mbedtls_md_sha256 { sig_scheme_rsa_pss_rsae_sha256 }
					mbedtls_md_sha384 { sig_scheme_rsa_pss_rsae_sha384 }
					mbedtls_md_sha512 { sig_scheme_rsa_pss_rsae_sha512 }
					else {
						error('unsupported RSA-PSS certificate signature digest ${info.signature_digest_type}')
					}
				}
			}
			if info.issuer_public_key_type == mbedtls_pk_rsassa_pss {
				return match info.signature_digest_type {
					mbedtls_md_sha256 { sig_scheme_rsa_pss_pss_sha256 }
					mbedtls_md_sha384 { sig_scheme_rsa_pss_pss_sha384 }
					mbedtls_md_sha512 { sig_scheme_rsa_pss_pss_sha512 }
					else {
						error('unsupported RSA-PSS certificate signature digest ${info.signature_digest_type}')
					}
				}
			}
			return error('an RSA-PSS certificate signature has incompatible issuer public-key type ${info.issuer_public_key_type}')
		}
		else {
			return error('unsupported certificate signature public-key type ${info.signature_public_key_type}')
		}
	}
}

fn certificate_signature_scheme_candidates(info mbedtls.CertificateSignatureInfo) ![]u16 {
	if info.issuer_known {
		return [certificate_signature_scheme(info)!]
	}
	return match info.signature_public_key_type {
		mbedtls_pk_rsa {
			match info.signature_digest_type {
				mbedtls_md_sha256 { [sig_scheme_rsa_pkcs1_sha256] }
				mbedtls_md_sha384 { [sig_scheme_rsa_pkcs1_sha384] }
				mbedtls_md_sha512 { [sig_scheme_rsa_pkcs1_sha512] }
				else {
					error('unsupported RSA-PKCS1 certificate signature digest ${info.signature_digest_type}')
				}
			}
		}
		mbedtls_pk_ecdsa {
			// The signature OID identifies its digest but not the omitted
			// issuer's EC curve. Retain every currently supported ECDSA scheme
			// with that digest rather than inventing an issuer curve.
			match info.signature_digest_type {
				mbedtls_md_sha256 { [sig_scheme_ecdsa_secp256r1_sha256] }
				mbedtls_md_sha384 { [sig_scheme_ecdsa_secp384r1_sha384] }
				mbedtls_md_sha512 { [sig_scheme_ecdsa_secp521r1_sha512] }
				else {
					error('unsupported ECDSA certificate signature digest ${info.signature_digest_type}')
				}
			}
		}
		mbedtls_pk_rsassa_pss {
			validate_certificate_pss_parameters(info)!
			// The signature OID likewise cannot distinguish rsaEncryption from
			// RSASSA-PSS in an omitted issuer's SubjectPublicKeyInfo.
			match info.signature_digest_type {
				mbedtls_md_sha256 {
					[sig_scheme_rsa_pss_rsae_sha256, sig_scheme_rsa_pss_pss_sha256]
				}
				mbedtls_md_sha384 {
					[sig_scheme_rsa_pss_rsae_sha384, sig_scheme_rsa_pss_pss_sha384]
				}
				mbedtls_md_sha512 {
					[sig_scheme_rsa_pss_rsae_sha512, sig_scheme_rsa_pss_pss_sha512]
				}
				else {
					error('unsupported RSA-PSS certificate signature digest ${info.signature_digest_type}')
				}
			}
		}
		else {
			error('unsupported certificate signature public-key type ${info.signature_public_key_type}')
		}
	}
}

// validate_certificate_chain_signature_algorithms parses the configured DER
// chain and verifies every non-anchor certificate signature can be expressed
// by a scheme in the client's certificate-specific offer. A terminal
// self-issued certificate is the prospective trust anchor and its signature
// is exempt from negotiation. If a terminal certificate's issuer was omitted,
// all schemes compatible with the known signature OID/digest are considered;
// exact issuer key type and curve matching is retained whenever it is present.
fn validate_certificate_chain_signature_algorithms(certificate_list []CertificateEntry, offered_schemes []u16, offer_name string) ! {
	mut der_certs := [][]u8{cap: certificate_list.len}
	for entry in certificate_list {
		der_certs << entry.cert_data
	}
	chain := mbedtls.build_certificate_chain(der_certs)!
	defer {
		mbedtls.free_certificate_chain(chain)
	}
	infos := mbedtls.certificate_signature_infos(chain)
	if infos.len != certificate_list.len {
		return error('parsed ${infos.len} certificates from a configured chain containing ${certificate_list.len} entries')
	}
	for i, info in infos {
		if i == infos.len - 1 && info.self_issued {
			continue
		}
		candidates := certificate_signature_scheme_candidates(info) or {
			return error('certificate ${i} signature cannot be matched to a TLS SignatureScheme: ${err.msg()}')
		}
		mut compatible := false
		for scheme in candidates {
			if scheme in offered_schemes {
				compatible = true
				break
			}
		}
		if !compatible {
			if candidates.len == 1 {
				return error('certificate ${i} uses signature scheme 0x${candidates[0]:04x}, which the ClientHello did not offer in ${offer_name}')
			}
			mut candidate_names := []string{cap: candidates.len}
			for scheme in candidates {
				candidate_names << '0x${scheme:04x}'
			}
			return error('certificate ${i} uses one of the signature schemes ${candidate_names.join(', ')}, none of which the ClientHello offered in ${offer_name}')
		}
	}
}

// VerifiedCertificateChain wraps an mbedTLS certificate chain built from a
// parsed TLS 1.3 Certificate message (tls13_certificate.v's
// ParsedCertificate) once it has passed chain-trust validation. The caller
// MUST call `free()` when done — the underlying mbedtls_x509_crt chain
// holds C-heap-allocated buffers with no GC visibility (see
// net.mbedtls.build_certificate_chain's own doc comment).
pub struct VerifiedCertificateChain {
mut:
	chain &C.mbedtls_x509_crt = unsafe { nil }
}

// free releases the underlying mbedTLS chain. Nulls out `chain` after
// freeing so a second free() call (e.g. a defer racing an explicit early
// free) is a harmless no-op rather than a double-free — same discipline
// as net.mbedtls.SSLConn.shutdown()'s own documented guard for the
// identical class of repeated-cleanup-call bug.
pub fn (mut c VerifiedCertificateChain) free() {
	if c.chain != unsafe { nil } {
		mbedtls.free_certificate_chain(c.chain)
		c.chain = unsafe { nil }
	}
}

// verify_server_certificate_chain builds an mbedTLS certificate chain from
// a parsed Certificate message's certificate_list (leaf-first, per RFC
// 8446 §4.4.2) and validates it against `ca_bundle_pem` (one or more
// trusted CA certificates in PEM format — the caller's trust anchor,
// mirroring this codebase's existing
// net.mbedtls.SSLConnectConfig.verify contract, since there is no OS
// trust-store lookup anywhere in this codebase for any TLS client) AND
// that `hostname` (the SNI name this client actually sent) matches the
// leaf certificate's SAN/CN — see net.mbedtls.verify_certificate_chain's
// own doc comment for the mechanism. Without this, any otherwise-trusted
// certificate for an unrelated host would be accepted (hostname
// impersonation), since chain-of-trust alone says nothing about which
// host the certificate is actually FOR.
pub fn verify_server_certificate_chain(parsed ParsedCertificate, ca_bundle_pem string, hostname string) !&VerifiedCertificateChain {
	mut der_certs := [][]u8{cap: parsed.certificate_list.len}
	for entry in parsed.certificate_list {
		der_certs << entry.cert_data
	}
	chain := mbedtls.build_certificate_chain(der_certs)!
	mbedtls.verify_certificate_chain(chain, ca_bundle_pem, hostname) or {
		mbedtls.free_certificate_chain(chain)
		return err
	}
	// Chain-of-trust and hostname alone say nothing about whether this
	// certificate was actually ISSUED for TLS server authentication -- see
	// check_server_cert_usage's own doc comment for the missing-check this
	// closes (Codex P1, vlang/v#27680 pullrequestreview-4783410111).
	mbedtls.check_server_cert_usage(chain) or {
		mbedtls.free_certificate_chain(chain)
		return err
	}
	return &VerifiedCertificateChain{
		chain: chain
	}
}

// verify_certificate_verify_signature checks a parsed CertificateVerify
// message's signature against this chain's leaf certificate's public key.
// `role`/`transcript_hash` feed certificate_verify_signed_content's exact
// RFC 8446 §4.4.3 signed-content construction -- what was actually signed,
// not `transcript_hash` directly. Dispatches on `cv.algorithm` to the
// matching digest + mbedTLS verification call; parse_certificate_verify has
// already restricted `cv.algorithm` to v1's fixed offered set
// (sig_scheme_ecdsa_secp256r1_sha256/rsa_pss_rsae_sha256/384/512), so the
// `else` arm below is unreachable in practice, not a real fallback path.
//
// Guards against being called after free(): mbedtls.get_leaf_public_key's
// own doc comment already states the precondition ("do not call this after
// free_certificate_chain") but doesn't enforce it -- free() nulls c.chain,
// and the C shim behind get_leaf_public_key computes `&crt->pk` (pointer
// arithmetic on a NULL crt), which is undefined behavior, not a clean nil
// dereference an `or {}` could catch. No caller does this today, but the
// upcoming client state machine will hold a VerifiedCertificateChain across
// multiple calls (trust check, then this), making the free-then-use
// ordering an easy mistake to introduce later -- same defensive rationale
// as free()'s own idempotency guard, just checked from the other side.
pub fn (c &VerifiedCertificateChain) verify_certificate_verify_signature(cv ParsedCertificateVerify, role CertificateVerifyRole, transcript_hash []u8) ! {
	if c.chain == unsafe { nil } {
		return error('quic: verify_certificate_verify_signature called on a freed VerifiedCertificateChain')
	}
	signed_content := certificate_verify_signed_content(role, transcript_hash)
	pk := mbedtls.get_leaf_public_key(c.chain)
	match cv.algorithm {
		sig_scheme_ecdsa_secp256r1_sha256 {
			// mbedtls_pk_verify_ext (inside verify_ecdsa_signature) only
			// confirms the key is SOME EC key -- it never checks WHICH
			// curve. ecdsa_secp256r1_sha256 (RFC 8446 §4.2.3) specifically
			// claims P-256; a certificate whose actual key is P-384/P-521/
			// any other curve must be rejected under this scheme name even
			// though its signature would verify correctly on ITS OWN curve
			// (Codex P2, vlang/v#27680 pullrequestreview-4783410111).
			if !mbedtls.public_key_curve_is_secp256r1(pk) {
				return error('quic: CertificateVerify algorithm ecdsa_secp256r1_sha256 requires a P-256 certificate key, but the leaf key is a different curve')
			}
			hash := sha256.sum256(signed_content)
			mbedtls.verify_ecdsa_signature(pk, .sha256, hash, cv.signature)!
		}
		sig_scheme_rsa_pss_rsae_sha256 {
			hash := sha256.sum256(signed_content)
			mbedtls.verify_rsa_pss_signature(pk, .sha256, hash, cv.signature)!
		}
		sig_scheme_rsa_pss_rsae_sha384 {
			hash := sha512.sum384(signed_content)
			mbedtls.verify_rsa_pss_signature(pk, .sha384, hash, cv.signature)!
		}
		sig_scheme_rsa_pss_rsae_sha512 {
			hash := sha512.sum512(signed_content)
			mbedtls.verify_rsa_pss_signature(pk, .sha512, hash, cv.signature)!
		}
		else {
			return error('quic: CertificateVerify algorithm 0x${cv.algorithm:04x} has no signature-verification dispatch (unreachable: parse_certificate_verify already restricts algorithm to the offered set)')
		}
	}
}
