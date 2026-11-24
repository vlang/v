module driver

import os
import v.cmdexec

// Ask the selected toolchain for its arithmetic support library. The same
// target and final compile/link flags let Clang select compiler-rt,
// and let cross compilers locate their target's archive rather than the host's.
fn v3_freestanding_compiler_runtime(compiler string, environment_flags []string, target_args []string, source_flags []string, link_flags []string) ?string {
	mut args := []string{cap: environment_flags.len + source_flags.len + target_args.len + link_flags.len + 1}
	args << environment_flags
	args << target_args
	args << source_flags
	args << link_flags
	args << '-print-libgcc-file-name'
	result := cmdexec.run(compiler, args)
	if result.exit_code != 0 {
		return none
	}
	// Verbose toolchains can include diagnostics before the printed path.
	lines := result.output.split_into_lines()
	for index := lines.len - 1; index >= 0; index-- {
		path := lines[index].trim_space()
		if path.ends_with('.a') && os.is_file(path) {
			return os.real_path(path)
		}
	}
	// A driver that does not expose a concrete archive keeps its normal link
	// policy; callers can supply any required target runtime in their link flags.
	return none
}
