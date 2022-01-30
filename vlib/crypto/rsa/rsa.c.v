module rsa

import crypto.rand

#pkgconfig openssl

#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/err.h>

pub struct RSAInstance {
	pair &C.RSA
pub:
	public_key  []byte
	private_key []byte
}

[typedef]
struct C.RSA {
	engine voidptr
	n      &C.BIGNUM
	e      &C.BIGNUM
	d      &C.BIGNUM
	p      &C.BIGNUM
	q      &C.BIGNUM
}

[typedef]
struct C.BIGNUM {}

[typedef]
struct C.BIO {}

fn C.RSA_generate_key_ex(&C.RSA, int, &C.BIGNUM, voidptr) int
fn C.RSA_generate_key(int, u64, voidptr, voidptr) &C.RSA
fn C.RSA_new() &C.RSA
fn C.RSA_size(&C.RSA) int

fn C.RSA_public_encrypt(int, voidptr, voidptr, &C.RSA, int) int
fn C.RSA_private_decrypt(int, voidptr, voidptr, &C.RSA, int) int

fn C.BN_new() &C.BIGNUM
fn C.BN_free(&C.BIGNUM)
fn C.BN_set_word(&C.BIGNUM, int)

fn C.BIO_new(voidptr) &C.BIO
fn C.BIO_s_mem()
fn C.BIO_pending(&C.BIO) int
fn C.BIO_read(&C.BIO, byteptr, int)

fn C.ENGINE_set_default(voidptr, u32)

fn C.RAND_seed(voidptr, int)
fn C.RAND_status() int

fn C.ERR_get_error() u64
fn C.ERR_error_string(u64, charptr) charptr

fn C.PEM_write_bio_RSAPrivateKey(&C.BIO, &C.RSA, voidptr, voidptr, int, voidptr, voidptr)
fn C.PEM_write_bio_RSAPublicKey(&C.BIO, &C.RSA)

pub fn gen_key_pair(len int, exp int) ?RSAInstance {
	rsa := C.RSA_new()
	bn := C.BN_new()

	C.BN_set_word(bn, exp)

	if C.RAND_status() != 1 {
		return error('Not seeded')
	}

	res := C.RSA_generate_key_ex(rsa, len, bn, voidptr(0))
	C.BN_free(bn)

	if res != 1 {
		err := []byte{len: 256}
		C.ERR_error_string(C.ERR_get_error(), err.data)
		return error(err.bytestr())
	}

	private := C.BIO_new(C.BIO_s_mem())
	public := C.BIO_new(C.BIO_s_mem())

	C.PEM_write_bio_RSAPrivateKey(private, rsa, voidptr(0), voidptr(0), 0, voidptr(0),
		voidptr(0))
	C.PEM_write_bio_RSAPublicKey(public, rsa)

	private_len := C.BIO_pending(private)
	public_len := C.BIO_pending(public)

	private_key := []byte{len: private_len}
	public_key := []byte{len: public_len}

	C.BIO_read(private, private_key.data, private_len)
	C.BIO_read(public, public_key.data, public_len)

	return RSAInstance{
		pair: rsa
		public_key: public_key
		private_key: private_key
	}
}

pub fn (rsa RSAInstance) encrypt(msg []byte) ?([]byte, int) {
	res := []byte{len: C.RSA_size(rsa.pair)}
	len := C.RSA_public_encrypt(msg.len, msg.data, res.data, rsa.pair, C.RSA_PKCS1_OAEP_PADDING)
	if len == -1 {
		err := []byte{len: 256}
		C.ERR_error_string(C.ERR_get_error(), err.data)
		return error(err.bytestr())
	}
	return res, len
}

pub fn (rsa RSAInstance) decrypt(len int, data []byte) ?[]byte {
	res := []byte{len: len}
	l := C.RSA_private_decrypt(len, data.data, res.data, rsa.pair, C.RSA_PKCS1_OAEP_PADDING)
	if l == -1 {
		err := []byte{len: 256}
		C.ERR_error_string(C.ERR_get_error(), err.data)
		return error(err.bytestr())
	}
	return res[0..l]
}
