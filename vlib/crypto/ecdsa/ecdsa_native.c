#include <openssl/ecdsa.h>
#include <openssl/obj_mac.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/x509.h>
#include <openssl/bio.h>
#include <openssl/pem.h>
#include <openssl/param_build.h>

void v_ecdsa_OPENSSL_free(void *address) {
	OPENSSL_free(address);
}

_Static_assert(NID_X9_62_prime256v1 == 415, "V NID_X9_62_prime256v1 value");
_Static_assert(NID_secp384r1 == 715, "V NID_secp384r1 value");
_Static_assert(NID_secp521r1 == 716, "V NID_secp521r1 value");
_Static_assert(NID_secp256k1 == 714, "V NID_secp256k1 value");
_Static_assert(NID_X9_62_id_ecPublicKey == 408, "V NID_X9_62_id_ecPublicKey value");
_Static_assert(EVP_PKEY_EC == 408, "V EVP_PKEY_EC value");
_Static_assert(EVP_PKEY_PUBLIC_KEY == 134, "V EVP_PKEY_PUBLIC_KEY value");
_Static_assert(EVP_PKEY_KEYPAIR == 135, "V EVP_PKEY_KEYPAIR value");
_Static_assert(OPENSSL_EC_NAMED_CURVE == 1, "V OPENSSL_EC_NAMED_CURVE value");
