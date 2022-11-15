// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import hash.fnv1a

pub fn (e EmbeddedFile) hash() u64 {
	return fnv1a.sum64_string('${e.apath}, ${e.compression_type}, ${e.is_compressed}, ${e.len}')
}
