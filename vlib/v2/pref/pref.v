// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os

pub struct Preferences {
pub mut:
	debug        bool
	verbose      bool
	skip_genv    bool
	skip_builtin bool
	skip_imports bool
	no_parallel  bool
pub:
	vroot         string = os.dir(@VEXE)
	vmodules_path string = os.vmodules_dir()
}

pub fn new_preferences() Preferences {
	return Preferences{}
}

pub fn new_preferences_using_options(options []string) Preferences {
	return Preferences{
		// config flags
		debug:        '--debug' in options || '-d' in options
		verbose:      '--verbose' in options || '-v' in options
		skip_genv:    '--skip-genv' in options
		skip_builtin: '--skip-builtin' in options
		skip_imports: '--skip-imports' in options
		no_parallel:  '--no-parallel' in options
	}
}
