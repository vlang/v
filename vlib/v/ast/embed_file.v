// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import hash.fnv1a

@[minify]
pub struct EmbeddedFile {
pub:
	compression_type string
pub mut:
	rpath string // used in the source code, as an ID/key to the embed
	apath string // absolute path during compilation to the resource
	// these are set by gen_embed_file_init in v/gen/c/embed
	is_compressed bool
	bytes         []u8
	len           int
}

pub fn (e EmbeddedFile) hash() u64 {
	return fnv1a.sum64_string('${e.apath}, ${e.compression_type}, ${e.is_compressed}, ${e.len}')
}
