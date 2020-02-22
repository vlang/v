// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

// This file contains the options specific to the JS backend of V.

import (
	internal.flag
	v.pref
)

[inline]
fn parse_js_options(flag string, f flag.Instance, prefs pref.Preferences) {
	// No notable flags for JS-only currently. Add them here when they are needed.
	// For now, we just fail as this is meant to be a fallback.
	println('V error: Unknown flag `-$flag` provided.')
	println('Use `--` to terminate flag list if necessary.')
	exit(1)
}
