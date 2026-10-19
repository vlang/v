module main

import os

// The V3 unit-test runner, `vlib/v/test_all.vsh`, builds V3 from `vlib/v/v.v` once and names
// that compiler and its source in these variables. Hundreds of its tests build the same compiler
// again through `@VEXE`, which is this launcher, so the launcher is the only process that sees
// those requests: a wrapper named by VEXE cannot, because main() points VEXE back at this
// executable before the test compiler reads it.
const shared_test_compiler_env = 'V3_TEST_UNIT_SHARED_V3'
const shared_test_compiler_source_env = 'V3_TEST_UNIT_V3_SRC'

// reuse_shared_test_compiler serves a build of the unit-test runner's shared V3 compiler with a
// copy of it, when `args` asks for exactly the build that produced it. It reports whether it
// served the request; when it did not, the caller builds as usual. A copy, unlike a hard link,
// gives the output its own file identity, as a fresh build would: the module cache keys on it,
// and a test that rebuilds its compiler expects a different one.
fn reuse_shared_test_compiler(args []string) bool {
	shared := os.getenv(shared_test_compiler_env)
	source := os.getenv(shared_test_compiler_source_env)
	if shared == '' || source == '' || !os.is_executable(shared) {
		return false
	}
	output := shared_test_compiler_output(args, source) or { return false }
	if os.real_path(output) == os.real_path(shared) {
		return false
	}
	// Remove the old output first: copying onto it would write through any hard link to it.
	if os.exists(output) {
		os.rm(output) or { return false }
	}
	os.cp(shared, output) or { return false }
	os.chmod(output, 0o755) or { return false }
	// The runner counts these lines to report how many builds the shared compiler saved.
	mut log := os.open_append(shared + '.reused') or { return true }
	log.writeln(output) or {}
	log.close()
	return true
}

// shared_test_compiler_output returns the `-o` path of `args` when they build `source` with the
// options of the shared compiler and nothing else: `-gc none`, and `-path` entries that resolve
// modules from the vlib of `source`. Every other option can change the compiler that is built,
// and its request must build its own.
fn shared_test_compiler_output(args []string, source string) ?string {
	source_path := os.real_path(source)
	vlib := os.dir(os.dir(source_path))
	mut output := ''
	mut saw_source := false
	mut saw_gc_none := false
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg in ['-gc', '-path', '-o'] {
			if i + 1 >= args.len {
				return none
			}
			value := args[i + 1]
			if arg == '-gc' {
				if value != 'none' {
					return none
				}
				saw_gc_none = true
			} else if arg == '-path' {
				if !shared_test_compiler_path_is_vlib(value, vlib) {
					return none
				}
			} else {
				if output != '' {
					return none
				}
				output = value
			}
			i += 2
			continue
		}
		if arg.starts_with('-') || saw_source || os.real_path(arg) != source_path {
			return none
		}
		saw_source = true
		i++
	}
	if !saw_source || !saw_gc_none || output == '' {
		return none
	}
	// An extension can select another kind of output, such as C source.
	extension := os.file_ext(os.file_name(output))
	$if windows {
		if extension != '.exe' {
			return none
		}
	} $else {
		if extension != '' {
			return none
		}
	}
	return output
}

// shared_test_compiler_path_is_vlib reports whether the `-path` value `value` looks modules up
// in `vlib` first, as the `-path <vlib>` that built the shared compiler does. `@vmodules` may
// follow, since `vlib/v/v.v` imports only vlib modules.
fn shared_test_compiler_path_is_vlib(value string, vlib string) bool {
	entries := value.split('|')
	for i, entry in entries {
		if entry == '@vlib' || (entry != '' && os.real_path(entry) == vlib) {
			continue
		}
		if entry == '@vmodules' && i > 0 {
			continue
		}
		return false
	}
	return true
}
