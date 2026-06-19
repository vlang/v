// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
module builder

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token

// Phase 2 regression: Parser.parse_files_to_flat must produce a FlatAst whose
// canonical signature is identical to ast.flatten_files(Parser.parse_files()).
// This guards the streaming path while downstream consumers are migrated.

fn write_tmp_file(name string, src string) string {
	path := '/tmp/v2_flat_stream_${os.getpid()}_${name}.v'
	os.write_file(path, src) or { panic('write_file: ${err}') }
	return path
}

fn assert_streaming_matches(srcs map[string]string) {
	mut paths := []string{cap: srcs.len}
	for name, src in srcs {
		paths << write_tmp_file(name, src)
	}
	defer {
		for p in paths {
			os.rm(p) or {}
		}
	}

	prefs := &pref.Preferences{}

	mut fs_batch := token.FileSet.new()
	mut p_batch := parser.Parser.new(prefs)
	legacy_files := p_batch.parse_files(paths, mut fs_batch)
	flat_batch := ast.flatten_files(legacy_files)

	mut fs_stream := token.FileSet.new()
	mut p_stream := parser.Parser.new(prefs)
	flat_stream := p_stream.parse_files_to_flat(paths, mut fs_stream)

	sig_batch := flat_batch.signature()
	sig_stream := flat_stream.signature()
	if sig_batch != sig_stream {
		mut diff_at := 0
		for diff_at < sig_batch.len && diff_at < sig_stream.len
			&& sig_batch[diff_at] == sig_stream[diff_at] {
			diff_at++
		}
		from := if diff_at > 80 { diff_at - 80 } else { 0 }
		end_b := if diff_at + 200 < sig_batch.len { diff_at + 200 } else { sig_batch.len }
		end_s := if diff_at + 200 < sig_stream.len { diff_at + 200 } else { sig_stream.len }
		eprintln('signature mismatch at offset ${diff_at}')
		eprintln('batch:  ${sig_batch[from..end_b]}')
		eprintln('stream: ${sig_stream[from..end_s]}')
	}
	assert sig_batch == sig_stream, 'streaming flat AST diverges from batch flatten'
}

fn test_streaming_single_file() {
	assert_streaming_matches({
		'single': 'module main

fn add(a int, b int) int {
	return a + b
}
'
	})
}

fn test_streaming_multi_file_order() {
	// Two files should produce node ids in parse order regardless of whether
	// the conversion is batched or streamed. Strings interned by file 0 must
	// be reused by file 1 in both cases.
	a := 'module main

pub const greeting = "hello"

fn say() string {
	return greeting
}
'
	b := 'module main

pub struct Point { x int y int }

fn (p Point) sum() int { return p.x + p.y }
'
	assert_streaming_matches({
		'a': a
		'b': b
	})
}

fn test_streaming_real_source_file() {
	path := os.real_path('vlib/v2_toberemoved/ast/ast.v')
	if !os.exists(path) {
		return
	}
	prefs := &pref.Preferences{}

	mut fs_batch := token.FileSet.new()
	mut p_batch := parser.Parser.new(prefs)
	legacy := p_batch.parse_files([path], mut fs_batch)
	flat_batch := ast.flatten_files(legacy)

	mut fs_stream := token.FileSet.new()
	mut p_stream := parser.Parser.new(prefs)
	flat_stream := p_stream.parse_files_to_flat([path], mut fs_stream)

	assert flat_batch.signature() == flat_stream.signature(), 'real-source streaming mismatch'
}

fn test_default_flat_parallel_transform_keeps_user_files() {
	path := write_tmp_file('parallel_transform_main', 'module main

fn main() {
	println("flat parallel ok")
}
')
	out_path := '/tmp/v2_flat_parallel_transform_${os.getpid()}.out'
	defer {
		os.rm(path) or {}
		os.rm(out_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -v2 -nocache -o ${os.quoted_path(out_path)} ${os.quoted_path(path)} 2>&1'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	run_res := os.execute(os.quoted_path(out_path))
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.trim_space() == 'flat parallel ok', run_res.output
}

fn test_default_parallel_transform_monomorphizes_generic_calls_for_native_backend() {
	path := write_tmp_file('parallel_transform_generic_main', 'module main

fn id[T](x T) T {
	return x
}

fn main() {
	_ = id(41)
}
')
	out_path := '/tmp/v2_parallel_generic_transform_${os.getpid()}.out'
	defer {
		os.rm(path) or {}
		os.rm(out_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} -v2 -nocache -backend arm64 -gc none -o ${os.quoted_path(out_path)} ${os.quoted_path(path)} 2>&1'
	res := os.execute(cmd)
	assert !res.output.contains('_id_T_int'), res.output
	assert res.exit_code == 0, res.output
}
