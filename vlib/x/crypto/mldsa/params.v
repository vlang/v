// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

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

pub const public_key_size_44 = 32 + 4 * n * 10 / 8
pub const public_key_size_65 = 32 + 6 * n * 10 / 8
pub const public_key_size_87 = 32 + 8 * n * 10 / 8

pub const signature_size_44 = 128 / 4 + 4 * n * (17 + 1) / 8 + 80 + 4
pub const signature_size_65 = 192 / 4 + 5 * n * (19 + 1) / 8 + 55 + 6
pub const signature_size_87 = 256 / 4 + 7 * n * (19 + 1) / 8 + 75 + 8

fn pub_key_size(p Params) int {
	return 32 + p.k * n * 10 / 8
}

fn sig_size(p Params) int {
	return (p.lambda / 4) + p.l * n * (p.gamma1 + 1) / 8 + p.omega + p.k
}
