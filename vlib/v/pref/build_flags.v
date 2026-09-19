module pref

import os

// get_build_facts_and_defines returns the build facts and the compile time
// defines of the current build, as recorded in the `VBUILD_FACTS` and
// `VBUILD_DEFINES` environment variables. The V driver reads the same two
// variables when it evaluates the `// vtest build:` constraints of a file, so
// test runners use this to construct the very same environment.
pub fn get_build_facts_and_defines() ([]string, []string) {
	facts := os.getenv('VBUILD_FACTS').split_any(',')
	defines := os.getenv('VBUILD_DEFINES').split_any(',')
	return facts, defines
}

// set_build_flags_and_defines records the build facts and the compile time
// defines in the `VBUILD_FACTS` and `VBUILD_DEFINES` environment variables, for
// the test runners and compilers that this process starts.
pub fn set_build_flags_and_defines(facts []string, defines []string) {
	os.setenv('VBUILD_FACTS', facts.join(','), true)
	os.setenv('VBUILD_DEFINES', defines.join(','), true)
}
