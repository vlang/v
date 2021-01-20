module builder

import os
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
			flag.cached = v.pref.cache_manager.postfix_with_key2cpath('.o', os.real_path(flag.value))
		}
		if flag.os == '' ||
			(flag.os == 'linux' && v.pref.os == .linux) ||
			(flag.os == 'macos' && v.pref.os == .macos) ||
			(flag.os == 'darwin' && v.pref.os == .macos) ||
			(flag.os == 'freebsd' && v.pref.os == .freebsd) ||
			(flag.os == 'windows' && v.pref.os == .windows) ||
			(flag.os == 'mingw' && v.pref.os == .windows && v.pref.ccompiler != 'msvc') ||
			(flag.os == 'solaris' && v.pref.os == .solaris)
		{
			flags << flag
		}
		if flag.os in ctimedefines {
			flags << flag
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
