// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Differential test harness for the flat-native markused walker.
//
// Markused decides which functions/methods the backend must emit. A wrong
// answer is silent — too few keys → linker errors, too many keys → bloated
// binary and slow build. The cursor-based walker that will replace the
// recursive walker must produce a bit-identical key set on every program
// that exercises the dispatch surface.
//
// This file provides the comparison machinery and a growing fixture catalog.
// The fixtures are intentionally small, one per language construct, so that
// when the diff fails the failing fixture immediately localises the bug.
// Until mark_used_flat lands, the "differential" tests assert determinism
// (legacy == legacy across two runs) — they prove the harness itself works
// without depending on the new walker.
module markused

import os
import time
import v2.ast
import v2.parser
import v2.pref
import v2.token
import v2.types

// ParsedFixture bundles every artefact a markused walker (legacy or flat)
// needs to run on a source string. It is returned by parse_fixture so each
// test fixture writes the source once and gets everything else for free.
struct ParsedFixture {
	files []ast.File
	flat  ast.FlatAst
	env   &types.Environment
}

fn parse_fixture(src string) ParsedFixture {
	tmp := '/tmp/v2_markused_diff_${os.getpid()}_${rand_suffix()}.v'
	os.write_file(tmp, src) or { panic('write_file: ${err}') }
	defer {
		os.rm(tmp) or {}
	}
	prefs := &pref.Preferences{
		skip_builtin: true
	}
	mut fs := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp], mut fs)
	flat := ast.flatten_files(files)
	// Skip the type checker entirely — fixtures use println / etc. which
	// would require pulling in builtin. The walker treats a near-empty env
	// as "no type info available" and falls back to syntactic receiver
	// candidates. Determinism is preserved (env in == env out for both runs)
	// which is all the differential harness needs.
	env := types.Environment.new()
	return ParsedFixture{
		files: files
		flat:  flat
		env:   env
	}
}

// rand_suffix produces a short pseudo-unique tag so concurrent test runs
// don't trample each other's tempfiles. We avoid pulling in `rand` so the
// harness has no dependency on RNG seeding inside the test binary.
fn rand_suffix() string {
	t := u64(time.now().unix_milli()) & 0xFFFFFFFF
	return '${t:x}'
}

// DiffReport captures the symmetric difference between two used_keys maps.
// It is a struct (not a bool) so failing tests can print exactly which keys
// drifted, which is essential for debugging walker bugs in 100+-key fixtures.
struct DiffReport {
	only_in_a []string
	only_in_b []string
	equal     bool
}

fn diff_used_keys(a map[string]bool, b map[string]bool) DiffReport {
	mut only_a := []string{}
	mut only_b := []string{}
	for k, _ in a {
		if k !in b {
			only_a << k
		}
	}
	for k, _ in b {
		if k !in a {
			only_b << k
		}
	}
	only_a.sort()
	only_b.sort()
	return DiffReport{
		only_in_a: only_a
		only_in_b: only_b
		equal:     only_a.len == 0 && only_b.len == 0
	}
}

fn assert_used_keys_equal(label string, a map[string]bool, b map[string]bool) {
	d := diff_used_keys(a, b)
	if d.equal {
		return
	}
	eprintln('[${label}] used_keys diverged:')
	if d.only_in_a.len > 0 {
		eprintln('  only in A (${d.only_in_a.len}):')
		for k in d.only_in_a {
			eprintln('    ${k}')
		}
	}
	if d.only_in_b.len > 0 {
		eprintln('  only in B (${d.only_in_b.len}):')
		for k in d.only_in_b {
			eprintln('    ${k}')
		}
	}
	assert false, '${label}: used_keys differ (see above)'
}

// run_legacy is a thin wrapper around mark_used so individual fixtures read
// as "legacy = run_legacy(p); flat = run_flat(p); assert_used_keys_equal".
fn run_legacy(p ParsedFixture) map[string]bool {
	return mark_used(p.files, p.env)
}

fn run_flat(p ParsedFixture) map[string]bool {
	return mark_used_flat(&p.flat, p.env)
}

// fixtures — each test exercises one slice of the walker dispatch surface.
// Adding a new fixture means: (1) write the smallest V program that hits the
// dispatch arm, (2) assert legacy determinism here, (3) when flat lands,
// flip to assert_used_keys_equal(name, run_legacy(p), run_flat(p)).

const fixture_plain_fn = 'module main

fn add(a int, b int) int {
	return a + b
}

fn main() {
	println(add(1, 2))
}
'

const fixture_method_call = 'module main

struct Counter {
mut:
	n int
}

fn (mut c Counter) inc() {
	c.n++
}

fn (c Counter) value() int {
	return c.n
}

fn main() {
	mut c := Counter{}
	c.inc()
	println(c.value())
}
'

const fixture_infix_operator = 'module main

struct Vec {
	x int
	y int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

fn main() {
	v := Vec{1, 2} + Vec{3, 4}
	println(v.x)
}
'

const fixture_array_and_loop = 'module main

fn sum(xs []int) int {
	mut s := 0
	for x in xs {
		s += x
	}
	return s
}

fn main() {
	xs := [1, 2, 3]
	println(sum(xs))
}
'

const fixture_match_expr = 'module main

fn describe(n int) string {
	return match n {
		0 { "zero" }
		1 { "one" }
		else { "many" }
	}
}

fn main() {
	println(describe(1))
}
'

const fixture_if_guard = 'module main

fn maybe(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn main() {
	if v := maybe(3) {
		println(v)
	}
}
'

const fixture_global_init = 'module main

__global g_counter = 0

fn bump() {
	g_counter++
}

fn main() {
	bump()
	println(g_counter)
}
'

// fixture_generic_fn exercises walker dispatch on generic function calls.
// The walker must mark both the generic base and any concrete bindings
// reachable through env.generic_types — the closest analogue of a real
// codebase\'s "[]T → []int" specialization path.
const fixture_generic_fn = 'module main

fn identity[T](x T) T {
	return x
}

fn main() {
	a := identity[int](42)
	b := identity[string]("hi")
	println(a)
	println(b)
}
'

// fixture_closure exercises closure capture: the walker must not lose
// reachability when the called fn is the value of a local variable that
// itself was assigned a fn literal closing over outer state.
const fixture_closure = 'module main

fn make_counter() fn () int {
	mut n := 0
	return fn [mut n] () int {
		n++
		return n
	}
}

fn main() {
	c := make_counter()
	println(c())
	println(c())
}
'

// fixture_sumtype_is_as covers the .is / as / smart-cast dispatch arms
// in walk_expr that resolve variant-specific method calls.
const fixture_sumtype_is_as = 'module main

type Shape = Circle | Square

struct Circle {
	r f64
}

struct Square {
	side f64
}

fn area(s Shape) f64 {
	return match s {
		Circle { 3.14 * s.r * s.r }
		Square { s.side * s.side }
	}
}

fn main() {
	c := Shape(Circle{r: 2.0})
	if c is Circle {
		println(c.r)
	}
	println(area(c))
}
'

// fixture_defer_and_map exercises defer statement walking plus map literal
// init / index / "in" lookup — three distinct walk_stmt / walk_expr arms
// that share no helpers with the simpler fixtures.
const fixture_defer_and_map = 'module main

fn lookup() int {
	mut m := {"a": 1, "b": 2}
	defer {
		m["a"] = 9
	}
	if "b" in m {
		return m["b"]
	}
	return 0
}

fn main() {
	println(lookup())
}
'

// fixture_string_interp_and_array_slice covers string-interpolation
// expressions and array slicing — both have their own walk_expr arms
// with their own sub-expression edges. Uses a raw string (r'...') so
// the outer V parser does not try to interpolate the inner ${...}.
const fixture_string_interp_and_array_slice = r'module main

fn slice_sum(xs []int) string {
	tail := xs[1..]
	mut s := 0
	for x in tail {
		s += x
	}
	return "sum=${s} count=${tail.len}"
}

fn main() {
	println(slice_sum([10, 20, 30, 40]))
}
'

fn all_fixtures() []string {
	return [
		fixture_plain_fn,
		fixture_method_call,
		fixture_infix_operator,
		fixture_array_and_loop,
		fixture_match_expr,
		fixture_if_guard,
		fixture_global_init,
		fixture_generic_fn,
		fixture_closure,
		fixture_sumtype_is_as,
		fixture_defer_and_map,
		fixture_string_interp_and_array_slice,
	]
}

// --- flat-vs-legacy diff tests ---
// These are the real safety net for the markused migration. Each PR that
// rewrites part of mark_used_flat must keep every fixture green. While the
// shim still calls flat.to_files() + the legacy walker, both runs must
// trivially produce the same map — once the body diverges, these fixtures
// catch regressions immediately and the DiffReport pinpoints which key set
// drifted.

fn test_flat_matches_legacy_plain_fn() {
	p := parse_fixture(fixture_plain_fn)
	assert_used_keys_equal('plain_fn', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_method_call() {
	p := parse_fixture(fixture_method_call)
	assert_used_keys_equal('method_call', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_infix_operator() {
	p := parse_fixture(fixture_infix_operator)
	assert_used_keys_equal('infix_operator', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_array_and_loop() {
	p := parse_fixture(fixture_array_and_loop)
	assert_used_keys_equal('array_and_loop', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_match_expr() {
	p := parse_fixture(fixture_match_expr)
	assert_used_keys_equal('match_expr', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_if_guard() {
	p := parse_fixture(fixture_if_guard)
	assert_used_keys_equal('if_guard', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_global_init() {
	p := parse_fixture(fixture_global_init)
	assert_used_keys_equal('global_init', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_generic_fn() {
	p := parse_fixture(fixture_generic_fn)
	assert_used_keys_equal('generic_fn', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_closure() {
	p := parse_fixture(fixture_closure)
	assert_used_keys_equal('closure', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_sumtype_is_as() {
	p := parse_fixture(fixture_sumtype_is_as)
	assert_used_keys_equal('sumtype_is_as', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_defer_and_map() {
	p := parse_fixture(fixture_defer_and_map)
	assert_used_keys_equal('defer_and_map', run_legacy(p), run_flat(p))
}

fn test_flat_matches_legacy_string_interp_and_array_slice() {
	p := parse_fixture(fixture_string_interp_and_array_slice)
	assert_used_keys_equal('string_interp_and_array_slice', run_legacy(p), run_flat(p))
}

// test_flat_is_deterministic — two runs of mark_used_flat on the same fixture
// must produce the same map. Guards against accidental nondeterminism
// (e.g. iteration order over a map of types) creeping in as the body
// of the shim is progressively replaced.
fn test_flat_is_deterministic_all_fixtures() {
	for i, src in all_fixtures() {
		p := parse_fixture(src)
		assert_used_keys_equal('fixture-${i}-determinism', run_flat(p), run_flat(p))
	}
}

// test_fixtures_produce_nonempty_results guards against silently broken
// harness setup: every fixture has at least main() reachable, so a zero-key
// result means our parse/typecheck/walk pipeline broke before we even got
// to compare anything.
fn test_fixtures_produce_nonempty_results() {
	for i, src in all_fixtures() {
		p := parse_fixture(src)
		keys := run_legacy(p)
		assert keys.len > 0, 'fixture #${i} produced empty used_keys — pipeline broken before walker'
	}
}

// test_flat_ast_round_trips_every_fixture verifies the cursor-side machinery
// works end-to-end on the fixture surface: every fixture parses, flattens,
// and is reachable via the cursor API from each file root. This is what
// the future flat walker will consume, so it must not blow up here.
fn test_flat_ast_round_trips_every_fixture() {
	for i, src in all_fixtures() {
		p := parse_fixture(src)
		assert p.flat.files.len > 0, 'fixture #${i}: flat has no files'
		for fi in 0 .. p.flat.files.len {
			fc := p.flat.file_cursor(fi)
			assert fc.root().is_valid(), 'fixture #${i} file ${fi}: invalid root'
			assert fc.root().kind() == .file, 'fixture #${i} file ${fi}: root not .file'
			// Touch each top-level list so an aux_list miswiring trips here
			// rather than deep inside the future walker port.
			_ = fc.attrs().len()
			_ = fc.imports().len()
			_ = fc.stmts().len()
		}
	}
}
