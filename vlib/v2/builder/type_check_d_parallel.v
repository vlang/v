// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.types

fn (mut b Builder) type_check_files_parallel() &types.Environment {
	// FlatAst-driven type check is sequential today (check_flat). A parallel
	// flat pipeline is future work.
	return b.type_check_files()
}
