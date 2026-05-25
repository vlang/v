// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
module types

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token

// Verifies Checker.check_flat (Phase 2 consumer entry point) produces a
// type-check environment functionally indistinguishable from check_files for
// the same source. While check_flat currently rehydrates internally, this
// regression guards the eventual direct-FlatAst implementation.

fn parse_to_flat(src string) (&ast.FlatAst, &pref.Preferences) {
	tmp := '/tmp/v2_checker_flat_${os.getpid()}.v'
	os.write_file(tmp, src) or { panic('write_file: ${err}') }
	defer {
		os.rm(tmp) or {}
	}
	p := &pref.Preferences{}
	mut fs := token.FileSet.new()
	mut par := parser.Parser.new(p)
	flat := par.parse_files_to_flat([tmp], mut fs)
	return &flat, p
}

fn check_via_files(src string) &Environment {
	tmp := '/tmp/v2_checker_flat_files_${os.getpid()}.v'
	os.write_file(tmp, src) or { panic('write_file: ${err}') }
	defer {
		os.rm(tmp) or {}
	}
	p := &pref.Preferences{}
	mut fs := token.FileSet.new()
	mut par := parser.Parser.new(p)
	files := par.parse_files([tmp], mut fs)
	env := Environment.new()
	mut c := Checker.new(p, fs, env)
	c.check_files(files)
	return env
}

fn check_via_flat(src string) &Environment {
	flat, prefs := parse_to_flat(src)
	mut fs := token.FileSet.new()
	env := Environment.new()
	mut c := Checker.new(prefs, fs, env)
	c.check_flat(flat)
	return env
}

fn test_check_flat_matches_check_files_simple_fn() {
	src := 'module main

fn add(a int, b int) int {
	return a + b
}
'
	env_files := check_via_files(src)
	env_flat := check_via_flat(src)
	// Both environments should have the same registered functions.
	fn_files := env_files.lookup_fn('main', 'add') or { panic('check_files missing main.add') }
	fn_flat := env_flat.lookup_fn('main', 'add') or { panic('check_flat missing main.add') }
	assert fn_files.params.len == fn_flat.params.len
	assert fn_files.params.len == 2
}

fn test_check_flat_matches_check_files_struct_method() {
	src := 'module main

pub struct Point {
pub mut:
	x int
	y int
}

fn (p Point) sum() int {
	return p.x + p.y
}
'
	env_files := check_via_files(src)
	env_flat := check_via_flat(src)
	m_files := env_files.lookup_method('Point', 'sum') or { panic('check_files missing sum') }
	m_flat := env_flat.lookup_method('Point', 'sum') or { panic('check_flat missing sum') }
	assert m_files.params.len == m_flat.params.len
}
