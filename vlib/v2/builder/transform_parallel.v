// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.transformer
import runtime

fn transform_worker(t &transformer.Transformer, files []ast.File) []ast.File {
	mut w := t.new_worker_clone()
	mut result := []ast.File{cap: files.len}
	for file in files {
		result << w.transform_file_pub(file)
	}
	return result
}

fn (mut b Builder) transform_files_parallel(mut trans transformer.Transformer) []ast.File {
	// Pre-pass: sequential (builds elided_fns and runtime const inits)
	trans.pre_pass(b.files)

	// Per-file transformation: parallel
	n_jobs := runtime.nr_jobs()
	n_files := b.files.len
	if n_files <= 1 || n_jobs <= 1 {
		// Fall back to sequential for trivial cases
		mut result := []ast.File{cap: n_files}
		for file in b.files {
			result << trans.transform_file_pub(file)
		}
		trans.post_pass(mut result)
		return result
	}

	// Create worker clones upfront so we can merge their accumulated state after
	chunk_size := (n_files + n_jobs - 1) / n_jobs // ceiling division
	mut workers := []&transformer.Transformer{cap: n_jobs}
	mut threads := []thread []ast.File{cap: n_jobs}
	mut i := 0
	for i < n_files {
		end := if i + chunk_size < n_files { i + chunk_size } else { n_files }
		chunk := b.files[i..end]
		mut w := trans.new_worker_clone()
		workers << w
		threads << spawn transform_worker_with(mut w, chunk)
		i = end
	}

	// Collect results, preserving file order
	file_results := threads.wait()
	mut result := []ast.File{cap: n_files}
	for files in file_results {
		result << files
	}
	// Merge accumulated state from all workers
	for w in workers {
		trans.merge_worker(w)
	}

	// Post-pass: sequential (injects generated functions, test main, etc.)
	trans.post_pass(mut result)
	return result
}

fn transform_worker_with(mut w transformer.Transformer, files []ast.File) []ast.File {
	mut result := []ast.File{cap: files.len}
	for file in files {
		result << w.transform_file_pub(file)
	}
	return result
}
