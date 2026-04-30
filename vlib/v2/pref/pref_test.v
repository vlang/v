module pref

import os

fn test_target_os_flag_is_parsed() {
	prefs := new_preferences_from_args(['v2', '-b', 'x64', '-os', 'mac', 'main.v'])
	assert prefs.target_os == 'macos'
	assert prefs.get_effective_os() == 'macos'
}

fn test_effective_os_defaults_to_host_os() {
	prefs := new_preferences_from_args(['v2', '-b', 'x64', 'main.v'])
	assert prefs.target_os == ''
	assert prefs.get_effective_os() == normalize_os_name(os.user_os())
}
