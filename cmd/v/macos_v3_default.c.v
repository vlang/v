module main

import v.pref

fn maybe_delegate_to_macos_v3(command string, prefs &pref.Preferences) ?MacosV3CErrorReport {
	_ = command
	_ = prefs
	return none
}
