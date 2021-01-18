module recompilation

import os

// disabling_file returns the path to a file, which if present, will disable the automatic recompilation
// that V attempts, when it detects that itself is newer than a tool .v source file.
// That file is intended to be made by `touch cmd/tools/.disable_autorecompilation` by package managers,
// so that tools like `v up` and `v self` will not work anymore, instead they will direct users to install
// V from source.
pub fn disabling_file(vroot string) string {
	tools_folder := os.join_path(vroot, 'cmd', 'tools')
	res := os.join_path(tools_folder, '.disable_autorecompilation')
	return res
}

// must_be_enabled is intended to be used by tools like `v self` and `v up`, to abort them
// early, when they detect that the V installation is part of a distro package, that has disabled autorecompilation.
pub fn must_be_enabled(vroot string, error_message string) {
	file := disabling_file(vroot)
	is_recompilation_disabled := os.exists(file)
	if is_recompilation_disabled {
		eprintln('Recompilation is disabled, since there is a "$file" file present.')
		eprintln(error_message)
		exit(1)
	}
}
