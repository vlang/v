module mbedtls

#flag -I @VEXEROOT/thirdparty/mbedtls/library
#flag -I @VEXEROOT/thirdparty/mbedtls/include
// #flag -D _FILE_OFFSET_BITS=64
#flag -I @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/include
#flag -I @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/include/everest
#flag -I @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/include/everest/kremlib

// TODO: this should be built-in to the compiler
$if prod && opt_size ? {
	#flag -Os
}

#flag @VEXEROOT/thirdparty/mbedtls/library/aes.o
#flag @VEXEROOT/thirdparty/mbedtls/library/aesce.o
#flag @VEXEROOT/thirdparty/mbedtls/library/aesni.o
#flag @VEXEROOT/thirdparty/mbedtls/library/aria.o
#flag @VEXEROOT/thirdparty/mbedtls/library/asn1parse.o
#flag @VEXEROOT/thirdparty/mbedtls/library/asn1write.o
#flag @VEXEROOT/thirdparty/mbedtls/library/base64.o
#flag @VEXEROOT/thirdparty/mbedtls/library/bignum.o
#flag @VEXEROOT/thirdparty/mbedtls/library/bignum_core.o
#flag @VEXEROOT/thirdparty/mbedtls/library/bignum_mod.o
#flag @VEXEROOT/thirdparty/mbedtls/library/bignum_mod_raw.o
#flag @VEXEROOT/thirdparty/mbedtls/library/block_cipher.o
#flag @VEXEROOT/thirdparty/mbedtls/library/camellia.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ccm.o
#flag @VEXEROOT/thirdparty/mbedtls/library/chacha20.o
#flag @VEXEROOT/thirdparty/mbedtls/library/chachapoly.o
#flag @VEXEROOT/thirdparty/mbedtls/library/cipher.o
#flag @VEXEROOT/thirdparty/mbedtls/library/cipher_wrap.o
#flag @VEXEROOT/thirdparty/mbedtls/library/cmac.o
#flag @VEXEROOT/thirdparty/mbedtls/library/constant_time.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ctr_drbg.o
#flag @VEXEROOT/thirdparty/mbedtls/library/debug.o
#flag @VEXEROOT/thirdparty/mbedtls/library/des.o
#flag @VEXEROOT/thirdparty/mbedtls/library/dhm.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ecdh.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ecdsa.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ecjpake.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ecp.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ecp_curves.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ecp_curves_new.o
#flag @VEXEROOT/thirdparty/mbedtls/library/entropy.o
#flag @VEXEROOT/thirdparty/mbedtls/library/entropy_poll.o
#flag @VEXEROOT/thirdparty/mbedtls/library/error.o
#flag @VEXEROOT/thirdparty/mbedtls/library/gcm.o
#flag @VEXEROOT/thirdparty/mbedtls/library/hkdf.o
#flag @VEXEROOT/thirdparty/mbedtls/library/hmac_drbg.o
#flag @VEXEROOT/thirdparty/mbedtls/library/lmots.o
#flag @VEXEROOT/thirdparty/mbedtls/library/lms.o
#flag @VEXEROOT/thirdparty/mbedtls/library/md5.o
#flag @VEXEROOT/thirdparty/mbedtls/library/md.o
#flag @VEXEROOT/thirdparty/mbedtls/library/memory_buffer_alloc.o
#flag @VEXEROOT/thirdparty/mbedtls/library/mps_reader.o
#flag @VEXEROOT/thirdparty/mbedtls/library/mps_trace.o
#flag @VEXEROOT/thirdparty/mbedtls/library/net_sockets.o
#flag @VEXEROOT/thirdparty/mbedtls/library/nist_kw.o
#flag @VEXEROOT/thirdparty/mbedtls/library/oid.o
#flag @VEXEROOT/thirdparty/mbedtls/library/padlock.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pem.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pk.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pkcs12.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pkcs5.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pkcs7.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pk_ecc.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pkparse.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pk_wrap.o
#flag @VEXEROOT/thirdparty/mbedtls/library/pkwrite.o
#flag @VEXEROOT/thirdparty/mbedtls/library/platform.o
#flag @VEXEROOT/thirdparty/mbedtls/library/platform_util.o
#flag @VEXEROOT/thirdparty/mbedtls/library/poly1305.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_aead.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_cipher.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_client.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_driver_wrappers_no_static.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_ecp.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_ffdh.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_hash.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_mac.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_pake.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_rsa.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_se.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_slot_management.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_storage.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_its_file.o
#flag @VEXEROOT/thirdparty/mbedtls/library/psa_util.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ripemd160.o
#flag @VEXEROOT/thirdparty/mbedtls/library/rsa_alt_helpers.o
#flag @VEXEROOT/thirdparty/mbedtls/library/rsa.o
#flag @VEXEROOT/thirdparty/mbedtls/library/sha1.o
#flag @VEXEROOT/thirdparty/mbedtls/library/sha256.o
#flag @VEXEROOT/thirdparty/mbedtls/library/sha3.o
#flag @VEXEROOT/thirdparty/mbedtls/library/sha512.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_cache.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_ciphersuites.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_client.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_cookie.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_debug_helpers_generated.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_msg.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_ticket.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls12_client.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls12_server.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_client.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_generic.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_keys.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_server.o
#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls.o
#flag @VEXEROOT/thirdparty/mbedtls/library/threading.o
#flag @VEXEROOT/thirdparty/mbedtls/library/timing.o
#flag @VEXEROOT/thirdparty/mbedtls/library/version.o
#flag @VEXEROOT/thirdparty/mbedtls/library/version_features.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509_create.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509_crl.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509_crt.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509_csr.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509write.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509write_crt.o
#flag @VEXEROOT/thirdparty/mbedtls/library/x509write_csr.o
#flag @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/library/Hacl_Curve25519_joined.o
#flag @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/library/everest.o
#flag @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/library/x25519.o

#include <mbedtls/net_sockets.h>
#include <mbedtls/ssl.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/error.h>

@[typedef]
pub struct C.mbedtls_net_context {
mut:
	fd int
}

@[typedef]
pub struct C.mbedtls_ssl_context {}

@[typedef]
pub struct C.mbedtls_ssl_config {}

@[typedef]
pub struct C.mbedtls_ssl_send_t {}

@[typedef]
pub struct C.mbedtls_ssl_recv_t {}

@[typedef]
pub struct C.mbedtls_ssl_recv_timeout_t {}

@[typedef]
pub struct C.mbedtls_pk_context {}

@[typedef]
pub struct C.mbedtls_ctr_drbg_context {}

@[typedef]
pub struct C.mbedtls_entropy_context {}

@[typedef]
pub struct C.mbedtls_x509_crt {}

@[typedef]
pub struct C.mbedtls_x509_crl {}

fn C.mbedtls_net_init(&C.mbedtls_net_context)
fn C.mbedtls_net_connect(&C.mbedtls_net_context, &char, &char, int) int
fn C.mbedtls_net_bind(&C.mbedtls_net_context, &char, &char, int) int
fn C.mbedtls_net_accept(&C.mbedtls_net_context, &C.mbedtls_net_context, voidptr, usize, &usize) int
fn C.mbedtls_net_free(&C.mbedtls_net_context)

fn C.mbedtls_ssl_init(&C.mbedtls_ssl_context)
fn C.mbedtls_ssl_setup(&C.mbedtls_ssl_context, &C.mbedtls_ssl_config) int
fn C.mbedtls_ssl_session_reset(&C.mbedtls_ssl_context)
fn C.mbedtls_ssl_conf_authmode(&C.mbedtls_ssl_config, int)
fn C.mbedtls_ssl_conf_rng(&C.mbedtls_ssl_config, fn (voidptr, &u8, usize) int, &C.mbedtls_ctr_drbg_context)
fn C.mbedtls_ssl_set_bio(&C.mbedtls_ssl_context, &C.mbedtls_net_context, &C.mbedtls_ssl_send_t, &C.mbedtls_ssl_recv_t,
	&C.mbedtls_ssl_recv_timeout_t)
fn C.mbedtls_ssl_conf_own_cert(&C.mbedtls_ssl_config, &C.mbedtls_x509_crt, &C.mbedtls_pk_context) int
fn C.mbedtls_ssl_conf_ca_chain(&C.mbedtls_ssl_config, &C.mbedtls_x509_crt, &C.mbedtls_x509_crl)
fn C.mbedtls_ssl_set_hostname(&C.mbedtls_ssl_context, &char) int
fn C.mbedtls_ssl_handshake(&C.mbedtls_ssl_context) int
fn C.mbedtls_ssl_read(&C.mbedtls_ssl_context, &u8, usize) int
fn C.mbedtls_ssl_write(&C.mbedtls_ssl_context, &u8, usize) int
fn C.mbedtls_ssl_free(&C.mbedtls_ssl_context)
fn C.mbedtls_ssl_config_init(&C.mbedtls_ssl_config)
fn C.mbedtls_ssl_config_defaults(&C.mbedtls_ssl_config, int, int, int) int
fn C.mbedtls_ssl_config_free(&C.mbedtls_ssl_config)
fn C.mbedtls_ssl_conf_sni(&C.mbedtls_ssl_config, fn (voidptr, &C.mbedtls_ssl_context, &char, int) int, voidptr)
fn C.mbedtls_ssl_set_hs_ca_chain(&C.mbedtls_ssl_config, &C.mbedtls_x509_crt, &C.mbedtls_x509_crl)
fn C.mbedtls_ssl_set_hs_own_cert(&C.mbedtls_ssl_context, &C.mbedtls_x509_crt, &C.mbedtls_pk_context) int
fn C.mbedtls_ssl_set_hs_authmode(&C.mbedtls_ssl_context, int)

fn C.mbedtls_pk_init(&C.mbedtls_pk_context)
fn C.mbedtls_pk_free(&C.mbedtls_pk_context)
fn C.mbedtls_pk_parse_key(&C.mbedtls_pk_context, &u8, usize, &u8, usize, fn (voidptr, &u8, usize) int,
	voidptr) int
fn C.mbedtls_pk_parse_keyfile(&C.mbedtls_pk_context, &char, &char, fn (voidptr, &u8, usize) int, voidptr) int

fn C.mbedtls_ctr_drbg_init(&C.mbedtls_ctr_drbg_context)
fn C.mbedtls_ctr_drbg_seed(&C.mbedtls_ctr_drbg_context, fn (voidptr, &u8, usize), voidptr, &u8, usize) int
fn C.mbedtls_ctr_drbg_free(&C.mbedtls_ctr_drbg_context)
fn C.mbedtls_ctr_drbg_random(voidptr, &u8, usize) int

fn C.mbedtls_entropy_init(&C.mbedtls_entropy_context)
fn C.mbedtls_entropy_free(&C.mbedtls_entropy_context)
fn C.mbedtls_entropy_func(voidptr, &u8, usize)

fn C.mbedtls_x509_crt_init(&C.mbedtls_x509_crt)
fn C.mbedtls_x509_crt_free(&C.mbedtls_x509_crt)
fn C.mbedtls_x509_crt_parse(&C.mbedtls_x509_crt, &u8, usize) int
fn C.mbedtls_x509_crt_parse_file(&C.mbedtls_x509_crt, &char) int

fn C.mbedtls_high_level_strerr(int) &char

fn C.mbedtls_debug_set_threshold(level int)

fn C.mbedtls_ssl_conf_read_timeout(conf &C.mbedtls_ssl_config, timeout u32)
