// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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

// ErrorType is used to filter out false positive errors under specific conditions
pub enum ErrorType {
	default
	space_indent
	trailing_space
}

[minify]
pub struct Error {
pub mut:
	kind ErrorKind [required]
pub:
	// General message
	message   string    [required]
	details   string    // Details about how to resolve or fix the situation
	file_path string    // file where the error have origin
	pos       token.Pos // position in the file
	fix       FixKind   [required]
	typ       ErrorType [required]
}
