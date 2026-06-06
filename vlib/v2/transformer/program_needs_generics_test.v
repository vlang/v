// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Pins program_needs_generics_from_flat — the cursor-native predicate that
// gates needs_full_files_for_transform. It must classify a program as needing
// generics iff some FnDecl/StructDecl has a non-lifetime generic param, scanning
// the flat AST directly (FnDecl edge 1 = FnType, FnType edge 0 = generic_params;
// StructDecl edge 3 = generic_params). The edge offsets are the one real
// correctness risk, so they get a dedicated test before the predicate is wired.
module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token

// png_of parses+flattens `src` and runs the predicate against the flat AST.
// Self-contained (no checker needed: generic params are syntactic) so it
// compiles standalone the way the V test runner builds each _test.v file.
fn png_of(src string) bool {
	tmp := '/tmp/v2_png_${os.getpid()}_${src.len}.v'
	os.write_file(tmp, src) or { panic('write_file: ${err}') }
	defer {
		os.rm(tmp) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut fs := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp], mut fs)
	flat := ast.flatten_files(files)
	return program_needs_generics_from_flat(&flat)
}

fn test_plain_fn_has_no_generics() {
	assert !png_of('fn add(a int, b int) int { return a + b }\nfn main() { println(add(1, 2)) }')
}

fn test_plain_struct_has_no_generics() {
	assert !png_of('struct Point { x int y int }\nfn main() { p := Point{1, 2} println(p.x) }')
}

fn test_generic_fn_is_detected() {
	assert png_of('fn pick[T](a T, b T) T { return a }\nfn main() {}')
}

fn test_generic_struct_is_detected() {
	assert png_of('struct Box[T] { val T }\nfn main() {}')
}

// A lifetime-only param (`^a`) is not a generic — mirror has_non_lifetime_generic_params.
fn test_lifetime_only_fn_is_not_a_generic() {
	assert !png_of('fn make[^a]() int { return 0 }\nfn main() {}')
}

fn test_lifetime_only_struct_is_not_a_generic() {
	assert !png_of('struct Holder[^a] { x int }\nfn main() {}')
}

// A lifetime alongside a real generic param still needs monomorphization.
fn test_lifetime_plus_generic_is_detected() {
	assert png_of('struct Ref[^a, T] { val T }\nfn main() {}')
}
