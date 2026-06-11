// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markused

import v2.ast
import v2.types

// mark_used_flat is the flat-input entry point for the markused pass. It
// accepts a FlatAst — the canonical post-parse representation — instead of
// a rehydrated []ast.File.
//
// The collect_defs phase reads each file's imports and top-level stmts via
// FlatAst.read_file_imports / FileCursor.stmts, so the per-file File struct
// (with its attribute/import/stmt slices and selector_names map) is never
// materialized. Function bodies are still decoded as legacy ast.Stmt and
// stored in FnInfo for walk_collected — that phase is the next slice to
// port. The differential harness in markused_diff_test.v guards
// bit-for-bit equality with the legacy walker at every step.
pub fn mark_used_flat(flat &ast.FlatAst, env &types.Environment) map[string]bool {
	return mark_used_flat_with_options(flat, env, MarkUsedOptions{})
}

pub fn mark_used_flat_with_options(flat &ast.FlatAst, env &types.Environment, opts MarkUsedOptions) map[string]bool {
	mut w := new_walker([]ast.File{}, env, opts)
	w.collect_defs_from_flat(flat)
	return w.walk_collected_from_flat(flat)
}
