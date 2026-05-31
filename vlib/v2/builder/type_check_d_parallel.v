// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.types

fn (mut b Builder) type_check_files_parallel() &types.Environment {
	// FlatAst-driven type check is sequential today (check_flat). When it is
	// enabled we delegate to the serial path so V2_CHECK_FLAT=1 isn't silently
	// ignored under `-d parallel`. A parallel flat pipeline is future work.
	if b.flat_check_enabled {
		return b.type_check_files()
	}

	env := types.Environment.new()
	mut checker := types.Checker.new(b.pref, b.file_set, env)

	// Phase 1: preregister scopes + types sequentially.
	// The previous parallel implementation raced on mod-scope map writes
	// (Scope.insert is not thread-safe; multiple workers wrote into the same
	// shared scope) and on c_scope: each worker had its own c_scope, so the
	// `C` Module stamped into mod_scopes pointed at one random worker's scope
	// and lookups for C decls registered by other workers failed. Until those
	// scopes are made concurrency-safe, run preregister on the main checker
	// so the rest of the pipeline sees a consistent type universe.
	for file in b.files {
		checker.preregister_scopes(file)
	}
	for file in b.files {
		checker.preregister_types(file)
	}
	checker.process_struct_deferred()

	for file in b.files {
		checker.preregister_fn_signatures(file)
	}

	// Phase 2: Full type checking - sequential for now
	// Because deferred closures capture the checker reference,
	// they can't be safely transferred between workers.
	// Future optimization: make closures capture data instead of references.
	for file in b.files {
		checker.check_file(file)
	}

	checker.process_all_deferred()
	checker.check_final_default_exprs(b.files)

	return env
}
