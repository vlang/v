// vclean.v removes the executables that a default V build leaves behind.
//
// A V build writes its executable next to the sources it was built from, named
// after them, so a build tree accumulates binaries that no other tool knows
// about. `v clean` removes exactly the ones the compiler would write for the
// paths it is given, and nothing else.
module main

import os
import flag

// input_kind classifies what the user named, because the compiler derives a
// different output name for a directory and for a single source file.
enum InputKind {
	directory
	source_file
	unsupported
}

// Input is one cleaned path together with the executable that belongs to it.
struct Input {
	path     string
	kind     InputKind
	bin_file string
}

// classify decides what kind of input `path` is. A name the compiler would only
// produce through its sanitizer is refused here rather than reconstructed:
// this command deletes files, so an uncertain name has to stop it.
fn classify(path string) InputKind {
	if os.is_dir(path) {
		return .directory
	}
	resolved := os.real_path(path)
	if !resolved.ends_with('.v') {
		return .unsupported
	}
	stem := os.file_name(resolved).all_before_last('.')
	if stem == '' || stem != stem.trim_space() || stem in ['.', '..', '-'] || stem.ends_with('.c') || stem.ends_with('.js')
		|| stem.ends_with('.wasm') {
		return .unsupported
	}
	for ch in stem {
		if ch < ` ` || ch == 127 {
			return .unsupported
		}
	}
	return .source_file
}

// bin_file_for returns the executable a default build of `path` writes. This
// mirrors `default_bin_file_for_input` and the platform suffix the compiler
// appends in `cmd/v/driver/driver.v`.
fn bin_file_for(path string) string {
	base := if os.is_dir(path) {
		real := os.real_path(path)
		os.join_path_single(real, os.file_name(real))
	} else {
		real := os.real_path(path)
		os.join_path_single(os.dir(real), os.file_name(real).all_before_last('.'))
	}
	if os.user_os() == 'windows' && !base.ends_with('.exe') {
		return base + '.exe'
	}
	return base
}

// classify_all turns the command line arguments into inputs. A missing argument
// means the current directory, which is the one an in-place build writes a
// binary for.
fn classify_all(paths []string) []Input {
	targets := if paths.len == 0 { ['.'] } else { paths }
	mut inputs := []Input{}
	for path in targets {
		inputs << Input{
			path:     path
			kind:     classify(path)
			bin_file: bin_file_for(path)
		}
	}
	return inputs
}

// report_refused names every path whose output cannot be worked out. The rest of
// the arguments are still cleaned, but the caller has to learn that these were
// left alone.
fn report_refused(inputs []Input) bool {
	mut refused := false
	for input in inputs {
		if input.kind == .unsupported {
			eprintln('v clean: cannot tell which executable `${input.path}` would produce, skipping it')
			refused = true
		}
	}
	return refused
}

// report_nothing names the paths that have no build output, which is the common
// case for a project that was never built in place.
fn report_nothing(inputs []Input) {
	for input in inputs {
		if input.kind != .unsupported && !os.is_file(input.bin_file) {
			println('nothing to clean for `${input.path}`')
		}
	}
}

// remove_one deletes the executable of `input`. Only a regular file is removed,
// and only the name the compiler itself would have written.
fn remove_one(input Input, dry_run bool, verbose bool) bool {
	if input.kind == .unsupported || !os.is_file(input.bin_file) {
		return true
	}
	if dry_run || verbose {
		println('rm ${input.bin_file}')
	}
	if dry_run {
		return true
	}
	os.rm(input.bin_file) or {
		eprintln('v clean: cannot remove `${input.bin_file}`: ${err.msg()}')
		return false
	}
	if !verbose {
		println('removed `${input.bin_file}`')
	}
	return true
}

fn main() {
	args := os.args[1..]
	// `v clean ...` reaches this tool with the `clean` word still in the arguments.
	passed := if args.len > 0 && args[0] == 'clean' { args[1..] } else { args }
	mut fp := flag.new_flag_parser(passed)
	fp.application('v clean')
	fp.version('0.0.1')
	fp.description('Remove the executables that a default V build leaves behind.')
	fp.arguments_description('[PATH...]')
	dry_run := fp.bool('dry-run', `n`, false, 'Print what would be removed, change nothing.')
	verbose := fp.bool('verbose', `x`, false, 'Print each removal as a command.')
	paths := fp.finalize() or {
		eprintln('v clean: ${err.msg()}')
		println(fp.usage())
		exit(1)
	}
	inputs := classify_all(paths)
	mut failed := report_refused(inputs)
	report_nothing(inputs)
	for input in inputs {
		if !remove_one(input, dry_run, verbose) {
			failed = true
		}
	}
	if failed {
		exit(1)
	}
}
