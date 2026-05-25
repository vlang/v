module builder

import v2.types

fn (mut b Builder) type_check_files() &types.Environment {
	env := types.Environment.new()
	mut checker := types.Checker.new(b.pref, b.file_set, env)
	if b.flat_check_enabled {
		checker.check_flat(&b.flat)
	} else {
		checker.check_files(b.files)
	}
	return env
}
