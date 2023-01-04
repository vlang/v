module builder

import os
import v.pref
import v.cflag

// get flags for current os
fn (mut v Builder) get_os_cflags() []cflag.CFlag {
	mut flags := []cflag.CFlag{}
	mut ctimedefines := []string{}
	if v.pref.compile_defines.len > 0 {
		ctimedefines << v.pref.compile_defines
	}
	for mut flag in v.table.cflags {
		if flag.value.ends_with('.o') {
			flag.cached = v.pref.cache_manager.mod_postfix_with_key2cpath(flag.mod, '.o',
				os.real_path(flag.value))
		}
		if flag.os == '' || flag.os in ctimedefines {
			flags << flag
			continue
		}
		fos := pref.os_from_string(flag.os) or { pref.OS.all }
		if fos != .all && fos == v.pref.os {
			flags << flag
			continue
		}
		if v.pref.os == .windows && flag.os == 'mingw' && v.pref.ccompiler != 'msvc' {
			flags << flag
			continue
		}
	}
	return flags
}

fn (mut v Builder) get_rest_of_module_cflags(c &cflag.CFlag) []cflag.CFlag {
	mut flags := []cflag.CFlag{}
	cflags := v.get_os_cflags()
	for flag in cflags {
		if c.mod == flag.mod {
			if c.name == flag.name && c.value == flag.value && c.os == flag.os {
				continue
			}
			flags << flag
		}
	}
	return flags
}
