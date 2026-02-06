// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

fn test_is_key() {
	// Test that tokens with keyword kinds return true.
	assert Token{
		kind: .key_as
	}.is_key()
	assert Token{
		kind: .key_fn
	}.is_key()
	assert Token{
		kind: .key_mut
	}.is_key()
	assert !Token{
		kind: .name
	}.is_key()
}
