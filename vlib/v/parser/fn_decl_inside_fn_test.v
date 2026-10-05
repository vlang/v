module parser

import os
import v.pref

fn parse_diagnostics(name string, src string, is_fmt bool) []Diagnostic {
	path := os.join_path(os.vtmp_dir(), 'v3_fn_decl_inside_fn_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.is_fmt = is_fmt
	mut p := Parser.new(prefs)
	p.parse_file(path)
	return p.diagnostics
}

// Anonymous functions used as statements start with `fn (` like a method
// declaration does; none of them may be taken for a declaration that is missing
// the `}` of the enclosing function.
fn test_anonymous_function_statements_are_not_declarations() {
	src := 'fn pair() (int, int) {
	return 1, 2
}

fn main() {
	fn () thread (int, int) {
		return spawn pair()
	}().wait()
	fn (x int) thread (int, int) {
		return spawn pair()
	}(1).wait()
	fn (x int) int {
		return x
	}(2)
	fn (x int) Box[int] {
		return Box[int]{x}
	}(3)
	fn (x int) (int, int) {
		return x, x
	}(4)
	y := 5
	fn [y] () {}()
}
'
	for is_fmt in [false, true] {
		diagnostics := parse_diagnostics('anon_${is_fmt}', src, is_fmt)
		assert diagnostics.len == 0, 'is_fmt: ${is_fmt}, ${diagnostics}'
	}
}

fn test_declarations_inside_a_function_report_the_missing_brace_once() {
	src := 'struct Foo {}

fn (f Foo) helper(x int) int {
	if x > 0 {
		return x
	return 0
}

fn (f Foo) other[T](v T) T {
	return v
}

pub fn third() int {
	return 3
}
'
	for is_fmt in [false, true] {
		diagnostics := parse_diagnostics('decl_${is_fmt}', src, is_fmt)
		assert diagnostics.len == 1, 'is_fmt: ${is_fmt}, ${diagnostics}'
		assert diagnostics[0].message == 'unexpected function declaration, expecting `}` to close function `Foo.helper`'
		assert diagnostics[0].line == 9
	}
}

fn test_operator_overload_after_a_missing_brace_is_reported_once() {
	src := 'struct V2 {
	x int
}

fn helper(x int) int {
	if x > 0 {
		return x
	return 0
}

fn (a V2) + (b V2) V2 {
	return V2{a.x + b.x}
}

fn main() {
	println(helper(3))
}
'
	for is_fmt in [false, true] {
		diagnostics := parse_diagnostics('op_${is_fmt}', src, is_fmt)
		assert diagnostics.len == 1, 'is_fmt: ${is_fmt}, ${diagnostics}'
		assert diagnostics[0].message == 'unexpected function declaration, expecting `}` to close function `helper`'
		assert diagnostics[0].line == 11
	}
}
