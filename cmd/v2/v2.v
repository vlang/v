// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v2.pref
import v2.builder

fn main() {
	args := os.args[1..]

	// Check for 'ast' subcommand
	if args.len > 0 && args[0] == 'ast' {
		run_ast_command(args[1..])
		return
	}

	prefs := pref.new_preferences_from_args(args)

	files := get_files(args)
	if files.len == 0 {
		eprintln('At least 1 .v file expected')
		exit(1)
	}

	mut b := builder.new_builder(prefs)
	b.build(files)
}

fn run_ast_command(args []string) {
	if args.len == 0 {
		eprintln('Usage: v2 ast <file.v>')
		eprintln('Dumps AST to <file>_ast.json and <file>_ast_transformed.json')
		exit(1)
	}

	// Find the vast2 tool relative to vexe
	vroot := os.dir(@VEXE)
	vast2_path := os.join_path(vroot, 'cmd', 'tools', 'vast2', 'vast2')

	// Build vast2 if it doesn't exist
	if !os.exists(vast2_path) {
		eprintln('Building vast2 tool...')
		vast2_source := os.join_path(vroot, 'cmd', 'tools', 'vast2', 'vast2.v')
		build_result := os.execute('${@VEXE} ${vast2_source}')
		if build_result.exit_code != 0 {
			eprintln('Failed to build vast2 tool:')
			eprintln(build_result.output)
			exit(1)
		}
	}

	// Run vast2 with the provided arguments
	cmd := '${vast2_path} ${args.join(' ')}'
	result := os.execute(cmd)
	print(result.output)
	if result.exit_code != 0 {
		exit(result.exit_code)
	}
}

// get_files extracts source files from args, excluding options and their values
fn get_files(args []string) []string {
	options_with_values := ['-backend', '-b', '-o', '-output', '-arch', '-printfn', '-gc', '-d']
	mut files := []string{}
	mut skip_next := false
	for arg in args {
		if skip_next {
			skip_next = false
			continue
		}
		if arg.starts_with('-') {
			if arg in options_with_values {
				skip_next = true
			}
			continue
		}
		files << arg
	}
	return files
}
