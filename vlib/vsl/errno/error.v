// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module errno

[inline]
pub fn vsl_error(reason string, errno Errno) ?string {
	estr := str_error(errno)
	return error('vsl: $estr: $reason')
}

[inline]
pub fn vsl_panic(reason string, errno Errno) {
	estr := str_error(errno)
	panic('vsl: $estr: $reason')
}
