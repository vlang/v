// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compile

// This file contains the options specific to the x64 backend of V and backends that generate executable files.
// To add a flag to all backends at once, please add the flag to `parse_options()` in `compile_options.v`.
// To add a flag to the C backend-only, please add the flag to `parse_c_options()` in `compile_c_options.v`.

import (
	internal.flag
	v.pref
)

[inline]
fn parse_x64_options(flag string, f mut flag.Instance, prefs mut pref.Preferences) {
	// No notable flags for x64-only currently. Add them here when they are needed.
	if flag == 'arch' {
		println('V error: The `-arch` flag is not supported on the x64 backend.')
		exit(1)
	}
	parse_executable_options(flag, mut f, mut prefs)
}

[inline]
fn parse_executable_options(flag string, f mut flag.Instance, prefs mut pref.Preferences) {
	match flag {
		'os', 'target-os' {
			f.is_equivalent_to(['os', 'target-os'])
			target_os := f.string() or {
				println('V error: Expected argument after `-$flag`.')
				exit(1)
			}
			if target_os == 'cross' {
				prefs.output_cross_c = true
				return
			}
			//TODO Remove `tmp` variable when it doesn't error out in C.
			tmp := pref.os_from_string(target_os) or {
				println('V error: Unknown operating system target `$target_os`.')
				exit(1)
			}
			prefs.os = tmp
		}
		'arch' {
			target_arch := f.string() or {
				println('V error: Expected argument after `-arch`.')
				exit(1)
			}
			match target_arch {
				'x86' {
					prefs.cflags += ' -m32'
				}
				'x64' {} // Default. Do nothing.
				else {
					println('V error: Unknown architecture type. Only x86 and x64 are supported currently.')
					exit(1)
				}
			}
		}
		'freestanding' {
			prefs.is_bare = f.bool()
		}
		'shared' {
			prefs.is_so = f.bool()
		}
		'live' {
			prefs.is_solive = f.bool()
		}
		'manual-free' {
			prefs.autofree = !f.bool()
		}
		'compress' {
			prefs.compress = f.bool()
		}
		else {
			// No more fallback. We don't recognize this flag.
			println('V error: Unknown flag `-$flag` provided.')
			println('Use `--` to terminate flag list if necessary.')
			exit(1)
		}
	}
}
