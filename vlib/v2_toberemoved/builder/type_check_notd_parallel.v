module builder

import v2.types

// Default build (no `-d parallel`): type-checking is sequential. The real
// parallel implementation lives in type_check_d_parallel.v and is only
// included when `-d parallel` is set.
fn (mut b Builder) type_check_files_parallel() &types.Environment {
	// Type checking is sequential: the bulk of the time (~90%) is in check_file
	// which has complex interdependencies (pending fn bodies, struct fields, etc.)
	// that prevent safe parallelization. The pre-registration phases (scopes, types)
	// are only ~6% of total time and share mutable Scope objects, making them
	// unsafe for concurrent access without major restructuring.
	return b.type_check_files()
}
