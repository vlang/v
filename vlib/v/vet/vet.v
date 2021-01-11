// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module vet

import v.token

pub enum ErrorKind {
	error
	warning
}

pub enum FixKind {
	unknown
	doc
	vfmt
}

pub struct Error {
pub:
	// General message
	message   string         [required]
	details   string // Details about how to resolve or fix the situation
	file_path string // file where the error have origin
	pos       token.Position // position in the file
	kind      ErrorKind      [required]
	fix       FixKind        [required]
}
