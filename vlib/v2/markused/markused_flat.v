// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markused

import v2.ast
import v2.types

// mark_used_flat is the flat-input entry point for the markused pass. It
// accepts a FlatAst — the canonical post-parse representation — instead of
// a rehydrated []ast.File. Today this is a thin shim: it rehydrates the
// FlatAst via flat.to_files() and delegates to the legacy walker, so its
// output is byte-identical to mark_used(files, env).
//
// The shim exists so the builder can already call markused without holding
// a separate b.files slice. Subsequent PRs will progressively replace this
// body — first collect_defs, then walk_stmt/walk_expr, then the mark_*
// helpers — until no rehydration happens. The differential harness in
// markused_diff_test.v guards bit-for-bit equality at every step.
pub fn mark_used_flat(flat &ast.FlatAst, env &types.Environment) map[string]bool {
	return mark_used_flat_with_options(flat, env, MarkUsedOptions{})
}

pub fn mark_used_flat_with_options(flat &ast.FlatAst, env &types.Environment, opts MarkUsedOptions) map[string]bool {
	files := flat.to_files()
	mut w := new_walker(files, env, opts)
	return w.walk()
}
