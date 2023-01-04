// Copyright (c) 2019-2022 Subhomoy Haldar. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import flag
import os
import regex
import semver

const (
	tool_name        = os.file_name(os.executable())
	tool_version     = '0.0.1'
	tool_description = '\n  Bump the semantic version of the v.mod and/or specified files.

  The first instance of a version number is replaced with the new version.
  Additionally, the line affected must contain the word "version" in any
  form of capitalization. For instance, the following lines will be
  recognized by the heuristic:

    tool_version = \'1.2.1\'
    version: \'0.2.42\'
    VERSION = "1.23.8"

Examples:
  Bump the patch version in v.mod if it exists
    v bump --patch
  Bump the major version in v.mod and vls.v
    v bump --major v.mod vls.v
  Upgrade the minor version in sample.v only
    v bump --minor sample.v
'
	semver_query = r'((0)|([1-9]\d*)\.){2}(0)|([1-9]\d*)(\-[\w\d\.\-_]+)?(\+[\w\d\.\-_]+)?'
)

struct Options {
	show_help bool
	major     bool
	minor     bool
	patch     bool
}

type ReplacementFunction = fn (re regex.RE, input string, start int, end int) string

fn replace_with_increased_patch_version(re regex.RE, input string, start int, end int) string {
	version := semver.from(input[start..end]) or { return input }
	return version.increment(.patch).str()
}

fn replace_with_increased_minor_version(re regex.RE, input string, start int, end int) string {
	version := semver.from(input[start..end]) or { return input }
	return version.increment(.minor).str()
}

fn replace_with_increased_major_version(re regex.RE, input string, start int, end int) string {
	version := semver.from(input[start..end]) or { return input }
	return version.increment(.major).str()
}

fn get_replacement_function(options Options) ReplacementFunction {
	if options.patch {
		return replace_with_increased_patch_version
	} else if options.minor {
		return replace_with_increased_minor_version
	} else if options.major {
		return replace_with_increased_major_version
	}
	return replace_with_increased_patch_version
}

fn process_file(input_file string, options Options) {
	lines := os.read_lines(input_file) or { panic('Failed to read file: ${input_file}') }

	mut re := regex.regex_opt(semver_query) or { panic('Could not create a RegEx parser.') }

	repl_fn := get_replacement_function(options)

	mut new_lines := []string{cap: lines.len}
	mut replacement_complete := false

	for line in lines {
		// Copy over the remaining lines normally if the replacement is complete
		if replacement_complete {
			new_lines << line
			continue
		}

		// Check if replacement is necessary
		updated_line := if line.to_lower().contains('version') {
			replacement_complete = true
			re.replace_by_fn(line, repl_fn)
		} else {
			line
		}
		new_lines << updated_line
	}

	// Add a trailing newline
	new_lines << ''

	backup_file := input_file + '.cache'

	// Remove the backup file if it exists.
	os.rm(backup_file) or {}

	// Rename the original to the backup.
	os.mv(input_file, backup_file) or { panic('Failed to copy file: ${input_file}') }

	// Process the old file and write it back to the original.
	os.write_file(input_file, new_lines.join_lines()) or {
		panic('Failed to write file: ${input_file}')
	}

	// Remove the backup file.
	os.rm(backup_file) or {}

	if replacement_complete {
		println('Bumped version in ${input_file}')
	} else {
		println('No changes made in ${input_file}')
	}
}

fn main() {
	if os.args.len < 2 {
		println('Usage: ${tool_name} [options] [file1 file2 ...]
${tool_description}
Try ${tool_name} -h for more help...')
		exit(1)
	}

	mut fp := flag.new_flag_parser(os.args)

	fp.application(tool_name)
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('[file1 file2 ...]')
	fp.skip_executable()

	options := Options{
		show_help: fp.bool('help', `h`, false, 'Show this help text.')
		patch: fp.bool('patch', `p`, false, 'Bump the patch version.')
		minor: fp.bool('minor', `n`, false, 'Bump the minor version.')
		major: fp.bool('major', `m`, false, 'Bump the major version.')
	}

	if options.show_help {
		println(fp.usage())
		exit(0)
	}

	validate_options(options) or { panic(err) }

	files := os.args[3..]

	if files.len == 0 {
		if !os.exists('v.mod') {
			println('v.mod does not exist. You can create one using "v init".')
			exit(1)
		}
		process_file('v.mod', options)
	}

	for input_file in files {
		if !os.exists(input_file) {
			println('File not found: ${input_file}')
			exit(1)
		}
		process_file(input_file, options)
	}
}

fn validate_options(options Options) ? {
	if options.patch && options.major {
		return error('Cannot specify both --patch and --major.')
	}

	if options.patch && options.minor {
		return error('Cannot specify both --patch and --minor.')
	}

	if options.major && options.minor {
		return error('Cannot specify both --major and --minor.')
	}

	if !(options.patch || options.major || options.minor) {
		return error('Must specify one of --patch, --major, or --minor.')
	}
}
