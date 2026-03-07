// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

// s. 4, table 1
pub enum Kind {
	ml_dsa_44
	ml_dsa_65
	ml_dsa_87
}

fn (k Kind) params() Params {
	return match k {
		.ml_dsa_44 { params_44 }
		.ml_dsa_65 { params_65 }
		.ml_dsa_87 { params_87 }
	}
}

pub fn (k Kind) public_key_size() int {
	return pub_key_size(k.params())
}

pub fn (k Kind) private_key_size() int {
	return priv_key_size(k.params())
}

pub fn (k Kind) signature_size() int {
	return sig_size(k.params())
}

@[params]
pub struct SignerOpts {
pub:
	context       string
	deterministic bool
}

struct Params {
	k      int
	l      int
	eta    int
	gamma1 int
	gamma2 int
	lambda int
	tau    int
	omega  int
}

// s. 4, table 1
const params_44 = Params{
	k:      4
	l:      4
	eta:    2
	gamma1: 17
	gamma2: 88
	lambda: 128
	tau:    39
	omega:  80
}

const params_65 = Params{
	k:      6
	l:      5
	eta:    4
	gamma1: 19
	gamma2: 32
	lambda: 192
	tau:    49
	omega:  55
}

const params_87 = Params{
	k:      8
	l:      7
	eta:    2
	gamma1: 19
	gamma2: 32
	lambda: 256
	tau:    60
	omega:  75
}

pub const seed_size = 32

// s. 4, table 2
pub const public_key_size_44 = 32 + 4 * n * 10 / 8
pub const public_key_size_65 = 32 + 6 * n * 10 / 8
pub const public_key_size_87 = 32 + 8 * n * 10 / 8

// s. 4, table 2
pub const signature_size_44 = 128 / 4 + 4 * n * (17 + 1) / 8 + 80 + 4
pub const signature_size_65 = 192 / 4 + 5 * n * (19 + 1) / 8 + 55 + 6
pub const signature_size_87 = 256 / 4 + 7 * n * (19 + 1) / 8 + 75 + 8

fn pub_key_size(p Params) int {
	return 32 + p.k * n * 10 / 8
}

fn priv_key_size(p Params) int {
	eta_bitlen := bits_len(u32(p.eta * 2))
	// rho + K + tr + l*n*eta-bit s1 + k*n*eta-bit s2 + k*n*13-bit t0
	return 32 + 32 + 64 + p.l * n * eta_bitlen / 8 + p.k * n * eta_bitlen / 8 + p.k * n * 13 / 8
}

fn sig_size(p Params) int {
	return (p.lambda / 4) + p.l * n * (p.gamma1 + 1) / 8 + p.omega + p.k
}
