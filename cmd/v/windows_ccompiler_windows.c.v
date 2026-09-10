module main

import v.builder
import v.pref

fn validate_windows_c_compiler_for_unknown_command(prefs &pref.Preferences) {
	// An invalid `-cc` setting should take precedence over an unknown command,
	// but must not affect commands that do not compile code.
	if builder.should_find_windows_host_c_compiler(prefs) && prefs.ccompiler_set_by_flag
		&& prefs.ccompiler != 'msvc' {
		mut probe := builder.Builder{
			pref: unsafe { prefs }
		}
		probe.find_win_cc() or { builder.verror(err.msg()) }
	}
}
