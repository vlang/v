// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast

fn (mut b Builder) parse_files_parallel(files []string) []ast.File {
	// FlatAst mode is the only builder parse pipeline. Keep `-parallel`
	// parsing serial for now: token.FileSet shares position counters and file
	// slices that are not concurrency-safe, and selector_names are keyed by
	// those position ids.
	return b.parse_files(files)
}
