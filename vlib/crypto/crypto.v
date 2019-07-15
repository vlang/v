    
// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package crypto collects common cryptographic constants.
module crypto

// import (
// 	"hash"
// 	"io"
// 	"strconv"
// )

// Hash identifies a cryptographic hash function that is implemented in another
// package.
type Hash uint

// Hashfn simply returns the value of h so that Hash implements SignerOpts.
fn (h Hash) HashFunc() Hash {
	return h
}

enum Hashes {
	MD4
	MD5
	SHA1
	SHA224
	SHA256
	SHA384
	SHA512
	MD5SHA1
	RIPEMD160
	SHA3_224
	SHA3_256
	SHA3_384
	SHA3_512
	SHA512_224
	SHA512_256
	BLAKE2s_256
	BLAKE2b_256
	BLAKE2b_384
	BLAKE2b_512
}

const (
	DigestSize_MD4 =         16
	DigestSize_MD5 =         16
	DigestSize_SHA1 =        20
	DigestSize_SHA224 =      28
	DigestSize_SHA256 =      32
	DigestSize_SHA384 =      48
	DigestSize_SHA512 =      64
	DigestSize_SHA512_224 =  28
	DigestSize_SHA512_256 =  32
	DigestSize_SHA3_224 =    28
	DigestSize_SHA3_256 =    32
	DigestSize_SHA3_384 =    48
	DigestSize_SHA3_512 =    64
	DigestSize_MD5SHA1 =     36
	DigestSize_RIPEMD160 =   20
	DigestSize_BLAKE2s_256 = 32
	DigestSize_BLAKE2b_256 = 32
	DigestSize_BLAKE2b_384 = 48
	DigestSize_BLAKE2b_512 = 64
}

// Size returns the length, in bytes, of a digest resulting from the given hash
// function. It doesn't require that the hash function in question be linked
// into the program.
fn (h Hash) Size() int {
	if h > 0 && h < maxHash {
		return int(digestSizes[h])
	}
	panic("crypto: Size of unknown hash function")
}

var hashes = make([]func() hash.Hash, maxHash)

// New returns a new hash.Hash calculating the given hash function. New panics
// if the hash function is not linked into the binary.
fn (h Hash) New() hash.Hash {
	if h > 0 && h < maxHash {
		f := hashes[h]
		if f != nil {
			return f()
		}
	}
	panic("crypto: requested hash function #" + strconv.Itoa(int(h)) + " is unavailable")
}

// Available reports whether the given hash function is linked into the binary.
fn (h Hash) available() bool {
	return h < maxHash && hashes[h] != nil
}

// RegisterHash registers a function that returns a new instance of the given
// hash function. This is intended to be called from the init function in
// packages that implement hash functions.
fn register_hash(h Hash, f func() hash.Hash) {
	if h >= maxHash {
		panic("crypto: RegisterHash of unknown hash function")
	}
	hashes[h] = f
}

// PublicKey represents a public key using an unspecified algorithm.
interface PublicKey {}

// PrivateKey represents a private key using an unspecified algorithm.
interface PrivateKey {}

// Signer is an interface for an opaque private key that can be used for
// signing operations. For example, an RSA key kept in a hardware module.
interface Signer {
	// Public returns the public key corresponding to the opaque,
	// private key.
	Public() PublicKey

	// Sign signs digest with the private key, possibly using entropy from
	// rand. For an RSA key, the resulting signature should be either a
	// PKCS#1 v1.5 or PSS signature (as indicated by opts). For an (EC)DSA
	// key, it should be a DER-serialised, ASN.1 signature structure.
	//
	// Hash implements the SignerOpts interface and, in most cases, one can
	// simply pass in the hash function used as opts. Sign may also attempt
	// to type assert opts to other types in order to obtain algorithm
	// specific values. See the documentation in each package for details.
	//
	// Note that when a signature of a hash of a larger message is needed,
	// the caller is responsible for hashing the larger message and passing
	// the hash (as digest) and the hash function (as opts) to Sign.
	Sign(rand io.Reader, digest []byte, opts SignerOpts) (signature []byte, err error)
}

// SignerOpts contains options for signing with a Signer.
interface SignerOpts {
	// Hashfn returns an identifier for the hash function used to produce
	// the message passed to Signer.Sign, or else zero to indicate that no
	// hashing was done.
	HashFunc() Hash
}

// Decrypter is an interface for an opaque private key that can be used for
// asymmetric decryption operations. An example would be an RSA key
// kept in a hardware module.
interface Decrypter {
	// Public returns the public key corresponding to the opaque,
	// private key.
	Public() PublicKey

	// Decrypt decrypts msg. The opts argument should be appropriate for
	// the primitive used. See the documentation in each implementation for
	// details.
	Decrypt(rand io.Reader, msg []byte, opts DecrypterOpts) (plaintext []byte, err error)
}

interface DecrypterOpts {}