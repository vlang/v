// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

fn test_validate_bucket_name_accepts_valid() {
	validate_bucket_name('my-bucket') or { assert false, 'should pass' }
	validate_bucket_name('a.b.c') or { assert false, 'should pass' }
	validate_bucket_name('abc') or { assert false, 'should pass' }
	validate_bucket_name('a' + 'b'.repeat(61) + 'c') or { assert false, 'should pass' }
}

fn test_validate_bucket_name_rejects_invalid() {
	if _ := validate_bucket_name('Bad-UPPER') {
		assert false, 'uppercase must be rejected'
	}
	if _ := validate_bucket_name('ab') {
		assert false, 'shorter than 3 chars must be rejected'
	}
	if _ := validate_bucket_name('-leading-hyphen') {
		assert false, 'leading hyphen must be rejected'
	}
	if _ := validate_bucket_name('trailing-hyphen-') {
		assert false, 'trailing hyphen must be rejected'
	}
	if _ := validate_bucket_name('192.168.0.1') {
		assert false, 'IPv4-shaped name must be rejected'
	}
	if _ := validate_bucket_name('a..b') {
		assert false, 'consecutive dots must be rejected'
	}
	if _ := validate_bucket_name('a.-b') {
		assert false, 'adjacent dot/hyphen must be rejected'
	}
	if _ := validate_bucket_name('a-.b') {
		assert false, 'adjacent hyphen/dot must be rejected'
	}
	if _ := validate_bucket_name('') {
		assert false, 'empty name must be rejected'
	}
	long := 'a'.repeat(64)
	if _ := validate_bucket_name(long) {
		assert false, 'longer than 63 chars must be rejected'
	}
}
