module pref

import os

pub fn get_build_facts_and_defines() ([]string, []string) {
	facts := os.getenv('VBUILD_FACTS').split_any(',')
	defines := os.getenv('VBUILD_DEFINES').split_any(',')
	return facts, defines
}

@[manualfree]
pub fn set_build_flags_and_defines(facts []string, defines []string) {
	sfacts := facts.join(',')
	sdefines := defines.join(',')
	os.setenv('VBUILD_FACTS', sfacts, true)
	os.setenv('VBUILD_DEFINES', sdefines, true)
	$if trace_vbuild ? {
		eprintln('> VBUILD_FACTS: ${sfacts}')
		eprintln('> VBUILD_DEFINES: ${sdefines}')
	}
	unsafe { sdefines.free() }
	unsafe { sfacts.free() }
}
