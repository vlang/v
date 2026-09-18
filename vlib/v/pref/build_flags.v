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
