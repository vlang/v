// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Differential test harness for the transformer migration to flat output.
//
// Goal: when the transformer eventually writes its result directly into a
// FlatAst (so the post-transform []ast.File is never allocated) the new
// path must produce bit-identical output to the legacy path on every
// program that exercises a non-trivial rewrite.
//
// Until that lands, the harness pins two invariants that already matter:
//
//   1. Determinism — transform_files run twice on the same parsed input
//      produces the same canonical signature. This guards against accidental
//      iteration-order or pointer-identity leakage as the body changes.
//
//   2. Flat-input parity — transform_files(files) and
//      transform_files_from_flat(&flat, []) must produce the same signature.
//      The from-flat streaming path was added 2026-05-26 and previously had
//      no per-construct regression test; it is the closest production path
//      to the future transform-writes-flat work, so any divergence here
//      surfaces a pre-pass / rehydration bug immediately.
//
// Adding a fixture: pick the smallest V program that hits the rewrite arm
// (operator overloading, if-guard, smartcast, ...) and add a test that calls
// assert_transform_signatures_equal. When a fixture fails, the signatures
// are dumped to /tmp so a `diff` localises the diverging node.
module transformer

import os
import time
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

// ParsedTransformerFixture bundles parse + check artefacts. The transformer
// mutates env / checker state internally; running both transformer paths
// against the *same* env is intentional — it's how the production pipeline
// behaves and any divergence the env causes is part of what we want to catch.
struct ParsedTransformerFixture {
	files []ast.File
	flat  ast.FlatAst
	env   &types.Environment
	prefs &vpref.Preferences
}

fn parse_transformer_fixture(src string) ParsedTransformerFixture {
	tmp := '/tmp/v2_transformer_diff_${os.getpid()}_${transformer_rand_suffix()}.v'
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
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, fs, env)
	checker.check_files(files)
	return ParsedTransformerFixture{
		files: files
		flat:  flat
		env:   env
		prefs: prefs
	}
}

fn transformer_rand_suffix() string {
	t := u64(time.now().unix_milli()) & 0xFFFFFFFF
	return '${t:x}'
}

// transform_signature is the canonical, comparable string form of a
// transformer output. It flattens the result and reuses ast.signature() so
// two runs producing structurally identical (but pointer-distinct) trees
// hash to the same string.
fn transform_signature(files []ast.File) string {
	flat := ast.flatten_files(files)
	return flat.signature()
}

// run_legacy_transform runs the canonical []ast.File-in, []ast.File-out path.
fn run_legacy_transform(p ParsedTransformerFixture) []ast.File {
	mut t := Transformer.new_with_pref(p.env, p.prefs)
	return t.transform_files(p.files)
}

// run_from_flat_transform runs the streaming-from-flat path, which feeds
// the same per-file transform but rehydrates each ast.File on demand from
// FlatAst instead of holding the full source array. Today both paths share
// transform_file; the test exists so when the path diverges further (e.g.
// transformer-writes-flat) any drift is caught.
fn run_from_flat_transform(p ParsedTransformerFixture) []ast.File {
	mut t := Transformer.new_with_pref(p.env, p.prefs)
	return t.transform_files_from_flat(&p.flat, [])
}

// dump_signature_pair writes the two signatures to /tmp so a `diff` can
// localise the diverging node when a fixture fails. Returns the two paths
// in the assert message.
fn dump_signature_pair(label string, a string, b string) (string, string) {
	suffix := transformer_rand_suffix()
	pa := '/tmp/v2_transform_sig_${label}_a_${suffix}.txt'
	pb := '/tmp/v2_transform_sig_${label}_b_${suffix}.txt'
	os.write_file(pa, a) or {}
	os.write_file(pb, b) or {}
	return pa, pb
}

fn assert_transform_signatures_equal(label string, a []ast.File, b []ast.File) {
	sa := transform_signature(a)
	sb := transform_signature(b)
	if sa == sb {
		return
	}
	pa, pb := dump_signature_pair(label, sa, sb)
	eprintln('[${label}] transformer signatures diverged.')
	eprintln('  A: ${pa}')
	eprintln('  B: ${pb}')
	eprintln('  diff with: diff -u ${pa} ${pb}')
	assert false, '${label}: transformer outputs differ (see /tmp dumps above)'
}

// --- fixture catalog ---
//
// Each fixture is the smallest V program that exercises one transformer
// rewrite path. The mix here is deliberately chosen to cover:
//   - plain top-level (no rewrites)        → fixture_plain_fn
//   - operator overloading                 → fixture_infix_operator
//   - array.contains / array methods       → fixture_array_contains
//   - if-guard (rewritten on every backend)→ fixture_if_guard
//   - or-block expression expansion        → fixture_or_block
//   - sumtype is / smartcast               → fixture_sumtype_is_as
//   - string interpolation                 → fixture_string_interp
//   - global init                          → fixture_global_init
//   - file-scope const decl                → fixture_const_decl
//   - paren-wrapped const value            → fixture_paren_expr
//   - prefix-op const value                 → fixture_prefix_expr
//   - modifier expr (mut arg)               → fixture_modifier_expr
//   - lambda expr (|y| y + 1 in call arg)   → fixture_lambda_expr
//   - fn literal (anonymous fn body)        → fixture_fn_literal
//   - postfix expr (a++ / a-- in stmt body) → fixture_postfix_expr
//   - cast expr (int(x), f64(y), u8(z))     → fixture_cast_expr
//   - field init expr (struct shorthand arg) → fixture_field_init
//   - as-cast expr (sumtype as Variant)      → fixture_as_cast_expr
//   - unsafe expr (unsafe { ... }, nil norm)  → fixture_unsafe_expr
//   - lock expr (lock x { body } value-form)   → fixture_lock_expr
//   - or expr (or-block nested in call args)    → fixture_or_expr
//   - selector expr (chained + non-Ident lhs)    → fixture_selector_expr
//   - index expr (default + gated + map + slice) → fixture_index_expr
//   - comptime expr ($if guarded body)           → fixture_comptime_expr
//   - init expr (struct literal w/ defaults)      → fixture_init_expr
//   - return stmt (multi-value + sumtype wrap)     → fixture_return_stmt
//   - ident expr (Ident in ported expr ancestors)    → fixture_ident
//   - keyword operator (sizeof/isreftype default)     → fixture_keyword_operator
//   - assert stmt (simple + cmp value-print branch)   → fixture_assert_stmt
//   - tuple if-assign (x, y := if c { a, b } else)    → fixture_tuple_if_assign
//   - tuple call-assign (a, b := pair())              → fixture_tuple_call_assign
//   - lock stmt (lock c { c.v = 1 } void-form)        → fixture_lock_stmt
//   - if-guard assign (x := if v := maybe() { v } else)→ fixture_if_guard_assign
//   - if-expr assign  (x = if cond { a } else { b })   → fixture_if_expr_assign
//   - return-if expr  (return if cond { a } else { b }) → fixture_return_if_expr
//   - return-match    (return match x { ... })          → fixture_return_match
//   - or stmt         (maybe() or { panic("") } stmt)   → fixture_or_stmt
//   - or assign       (a := maybe() or { 0 })            → fixture_or_assign
//   - or return       (return maybe() or { 0 })          → fixture_or_return
//   - for-in map      (for k, v in m { ... })             → fixture_for_in_map

// Fixtures intentionally avoid `module main`, `println`, and any builtin
// dependency — the harness skips the .vh cache load to stay light, so the
// checker only sees what's in the fixture source. Each fixture must still
// type-check standalone.

const fixture_plain_fn = '
fn add(a int, b int) int {
	return a + b
}

fn use_add() int {
	return add(1, 2)
}
'

const fixture_infix_operator = '
struct Vec {
	x int
	y int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

fn use_add() int {
	v := Vec{1, 2} + Vec{3, 4}
	return v.x
}
'

const fixture_array_contains = '
fn has(xs []int, n int) bool {
	return n in xs
}

fn use_has() bool {
	return has([1, 2, 3], 2)
}
'

const fixture_if_guard = '
fn maybe(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn use_guard() int {
	if v := maybe(3) {
		return v
	}
	return 0
}
'

const fixture_or_block = '
fn maybe(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn use_or() int {
	x := maybe(2) or { -1 }
	return x
}
'

const fixture_sumtype_is_as = '
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

fn use_shape() f64 {
	c := Shape(Circle{r: 2.0})
	if c is Circle {
		return c.r
	}
	return area(c)
}
'

const fixture_string_interp = r'
fn describe(xs []int) string {
	return "len=${xs.len} first=${xs[0]}"
}

fn use_describe() string {
	return describe([10, 20, 30])
}
'

const fixture_global_init = '
__global g_counter = 0

fn bump() {
	g_counter++
}

fn use_bump() int {
	bump()
	return g_counter
}
'

const fixture_const_decl = '
const x = 42
const greeting = "hello"
const sum = 1 + 2

fn use_const() int {
	return x + sum
}
'

const fixture_paren_expr = '
const paren_lit = (10)
const paren_nested = ((5))

fn use_paren() int {
	return paren_lit + paren_nested
}
'

const fixture_prefix_expr = '
const five = 5
const neg_five = -five
const inv_bits = ~0xFF
const not_flag = !true

fn use_prefix() int {
	if not_flag {
		return inv_bits
	}
	return neg_five
}
'

const fixture_modifier_expr = '
fn bump(mut x int) int {
	x = x + 1
	return x
}

fn use_modifier() int {
	mut a := 10
	r := bump(mut a)
	return r + a
}
'

const fixture_lambda_expr = '
fn apply(f fn (int) int, x int) int {
	return f(x)
}

fn use_lambda() int {
	return apply(|y| y + 1, 5)
}
'

const fixture_fn_literal = '
fn make_doubler() fn (int) int {
	return fn (x int) int {
		return x * 2
	}
}

fn use_fn_literal() int {
	doubler := fn (n int) int {
		return n + n
	}
	return doubler(21)
}
'

const fixture_postfix_expr = '
fn use_postfix() int {
	mut a := 5
	a++
	a++
	a--
	return a
}
'

const fixture_cast_expr = '
fn use_cast() int {
	a := f64(3.5)
	b := int(a)
	c := u8(b + 1)
	return int(c) + b
}
'

const fixture_field_init = '
struct Cfg {
	name string
	n    int
}

fn make(c Cfg) int {
	return c.n
}

fn use_field_init() int {
	return make(name: "hello", n: 5)
}
'

const fixture_or_expr = '
fn maybe(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn pair(a int, b int) int {
	return a + b
}

fn use_or_expr(n int) int {
	return pair(maybe(n) or { -1 }, 2)
}
'

const fixture_lock_expr = '
struct Counter {
mut:
	value int
}

fn use_lock() int {
	shared c := Counter{
		value: 0
	}
	x := lock c {
		c.value = 5
		c.value
	}
	return x
}
'

const fixture_unsafe_expr = '
fn use_unsafe() int {
	p := unsafe { nil }
	q := unsafe {
		a := 1
		a + 2
	}
	if p == unsafe { nil } {
		return q
	}
	return 0
}
'

const fixture_as_cast_expr = '
type Shape = Circle | Square

struct Circle {
	r f64
}

struct Square {
	side f64
}

fn pick(s Shape, k int) f64 {
	if k == 0 {
		return (s as Circle).r
	}
	return (s as Square).side
}
'

const fixture_selector_expr = '
struct Point {
	x int
	y int
}

struct Box {
mut:
	p    Point
	size int
}

fn use_selector(mut b Box, arr []Point) int {
	a := b.p.x
	c := b.size
	d := arr[0].y
	b.p.y = 42
	return a + c + d + b.p.y
}
'

const fixture_index_expr = "
fn use_index(arr []int, m map[string]int) int {
	a := arr[0] + arr[1]
	b := arr#[0]
	c := m['key']
	d := arr[1..3].len
	return a + b + c + d
}
"

const fixture_comptime_expr = '
fn use_comptime() int {
	\$if linux {
		return 1
	}
	return 2
}
'

const fixture_keyword_operator = '
const sz_int = sizeof(int)
const sz_bool = sizeof(bool)
const ref_int = isreftype(int)

fn use_keyword_operator() int {
	return int(sz_int) + int(sz_bool) + int(ref_int)
}
'

const fixture_ident = '
const base = 7
const neg_base = -base
const wrapped_base = (base)
const inv_base = ~base

fn use_ident(mut x int) int {
	mut a := base
	mut b := neg_base
	a = a + wrapped_base
	b = b - inv_base
	return a + b + x
}
'

const fixture_return_stmt = '
type Shape2 = Circle2 | Square2

struct Circle2 {
	r f64
}

struct Square2 {
	side f64
}

fn pair() (int, int) {
	return 1, 2
}

fn make_circle(r f64) Shape2 {
	return Circle2{r: r}
}

fn use_shape(s Shape2) Shape2 {
	if s is Circle2 {
		return s
	}
	return s
}
'

const fixture_init_expr = '
struct Item {
	name string
	qty  int
	tag  string = "default"
}

struct Crate {
mut:
	a Item
	b Item
	n int
}

fn use_init() int {
	x := Item{
		name: "apple"
		qty:  3
	}
	c := Crate{
		a: Item{
			name: "banana"
			qty:  5
		}
		b: Item{
			name: "cherry"
			qty:  7
			tag:  "ripe"
		}
		n: 11
	}
	return x.qty + c.a.qty + c.b.qty + c.n
}
'

const fixture_assert_stmt = '
fn use_assert(a int, b int) {
	assert a > 0
	assert a == b
	assert a > 0 && b > 0
}
'

const fixture_tuple_if_assign = '
fn use_tuple_if(cond bool) int {
	x, y := if cond { 10, 20 } else { 30, 40 }
	return x + y
}
'

const fixture_tuple_call_assign = '
fn pair() (int, int) {
	return 1, 2
}

fn use_tuple_call() int {
	a, b := pair()
	return a + b
}
'

const fixture_lock_stmt = '
struct LockBox {
mut:
	value int
}

fn use_lock_stmt() {
	shared c := LockBox{
		value: 0
	}
	lock c {
		c.value = 5
	}
}
'

const fixture_or_stmt = '
fn maybe3(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn act(n int) {
}

fn use_or_stmt(n int) {
	act(maybe3(n) or { -1 })
}
'

const fixture_or_assign = '
fn maybe4(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn use_or_assign(n int) {
	a := maybe4(n) or { 0 }
	b := maybe4(n) or { -1 }
	c := maybe4(a + b) or { 7 }
	_ = c
}
'

const fixture_or_return = '
fn maybe5(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn use_or_return(n int) int {
	return maybe5(n) or { -1 }
}

fn use_or_return_propagate(n int) ?int {
	return maybe5(n) or { return none }
}
'

const fixture_for_in_map = '
fn use_for_in_map() int {
	mut m := map[string]int{}
	m["a"] = 1
	m["b"] = 2
	m["c"] = 3
	mut total := 0
	for k, v in m {
		_ = k
		total += v
	}
	for _, v in m {
		total += v
	}
	for k, _ in m {
		_ = k
	}
	return total
}
'

const fixture_map_index_array_push = '
fn use_map_index_array_push() {
	mut m := map[int][]int{}
	m[1] << 2
}
'

const fixture_return_match = '
fn classify(n int) string {
	return match n {
		0 { "zero" }
		1, 2, 3 { "small" }
		else { "big" }
	}
}
'

const fixture_return_if_expr = '
fn use_return_if(cond bool) int {
	return if cond { 10 } else { 20 }
}

fn use_return_if_chained(a int) int {
	return if a > 10 { 1 } else if a > 5 { 2 } else { 3 }
}
'

const fixture_if_expr_assign = '
fn use_if_expr_assign(cond bool) int {
	mut x := 0
	x = if cond { 10 } else { 20 }
	return x
}

fn use_if_expr_assign_chained(a int) int {
	mut y := 0
	y = if a > 10 { 1 } else if a > 5 { 2 } else { 3 }
	return y
}
'

const fixture_if_guard_assign = '
fn maybe2(n int) ?int {
	if n > 0 {
		return n
	}
	return none
}

fn use_if_guard_assign() int {
	x := if v := maybe2(3) {
		v
	} else {
		0
	}
	return x
}

fn use_if_guard_assign_map() int {
	m := {
		"a": 1
		"b": 2
	}
	y := if r := m["a"] {
		r
	} else {
		-1
	}
	return y
}
'

const fixture_generic_fn = '
fn id[T](x T) T {
	return x
}

fn use_id() int {
	return id[int](3)
}
'

fn all_transformer_fixtures() []string {
	return [
		fixture_plain_fn,
		fixture_infix_operator,
		fixture_array_contains,
		fixture_if_guard,
		fixture_or_block,
		fixture_sumtype_is_as,
		fixture_string_interp,
		fixture_global_init,
		fixture_const_decl,
		fixture_paren_expr,
		fixture_prefix_expr,
		fixture_modifier_expr,
		fixture_lambda_expr,
		fixture_fn_literal,
		fixture_postfix_expr,
		fixture_cast_expr,
		fixture_field_init,
		fixture_as_cast_expr,
		fixture_unsafe_expr,
		fixture_lock_expr,
		fixture_or_expr,
		fixture_selector_expr,
		fixture_index_expr,
		fixture_comptime_expr,
		fixture_init_expr,
		fixture_return_stmt,
		fixture_ident,
		fixture_keyword_operator,
		fixture_assert_stmt,
		fixture_tuple_if_assign,
		fixture_tuple_call_assign,
		fixture_lock_stmt,
		fixture_if_guard_assign,
		fixture_if_expr_assign,
		fixture_return_if_expr,
		fixture_return_match,
		fixture_or_stmt,
		fixture_or_assign,
		fixture_or_return,
		fixture_for_in_map,
	]
}

// --- determinism: legacy × legacy on the same fixture ---
//
// These are the floor: if these ever fail the transformer has stateful
// drift that will break every other test in this file. Each fixture is
// re-parsed for each run so per-Transformer accumulator state never
// leaks between calls.

fn run_determinism(label string, src string) {
	p := parse_transformer_fixture(src)
	a := run_legacy_transform(p)
	b := run_legacy_transform(p)
	assert_transform_signatures_equal(label, a, b)
}

fn test_transform_is_deterministic_plain_fn() {
	run_determinism('det_plain_fn', fixture_plain_fn)
}

fn test_transform_is_deterministic_infix_operator() {
	run_determinism('det_infix_operator', fixture_infix_operator)
}

fn test_transform_is_deterministic_array_contains() {
	run_determinism('det_array_contains', fixture_array_contains)
}

fn test_transform_is_deterministic_if_guard() {
	run_determinism('det_if_guard', fixture_if_guard)
}

fn test_transform_is_deterministic_or_block() {
	run_determinism('det_or_block', fixture_or_block)
}

fn test_transform_is_deterministic_sumtype_is_as() {
	run_determinism('det_sumtype_is_as', fixture_sumtype_is_as)
}

fn test_transform_is_deterministic_string_interp() {
	run_determinism('det_string_interp', fixture_string_interp)
}

fn test_transform_is_deterministic_global_init() {
	run_determinism('det_global_init', fixture_global_init)
}

fn test_transform_is_deterministic_const_decl() {
	run_determinism('det_const_decl', fixture_const_decl)
}

fn test_transform_is_deterministic_paren_expr() {
	run_determinism('det_paren_expr', fixture_paren_expr)
}

fn test_transform_is_deterministic_prefix_expr() {
	run_determinism('det_prefix_expr', fixture_prefix_expr)
}

fn test_transform_is_deterministic_modifier_expr() {
	run_determinism('det_modifier_expr', fixture_modifier_expr)
}

fn test_transform_is_deterministic_lambda_expr() {
	run_determinism('det_lambda_expr', fixture_lambda_expr)
}

fn test_transform_is_deterministic_fn_literal() {
	run_determinism('det_fn_literal', fixture_fn_literal)
}

fn test_transform_is_deterministic_postfix_expr() {
	run_determinism('det_postfix_expr', fixture_postfix_expr)
}

fn test_transform_is_deterministic_cast_expr() {
	run_determinism('det_cast_expr', fixture_cast_expr)
}

fn test_transform_is_deterministic_field_init() {
	run_determinism('det_field_init', fixture_field_init)
}

fn test_transform_is_deterministic_as_cast_expr() {
	run_determinism('det_as_cast_expr', fixture_as_cast_expr)
}

fn test_transform_is_deterministic_unsafe_expr() {
	run_determinism('det_unsafe_expr', fixture_unsafe_expr)
}

fn test_transform_is_deterministic_lock_expr() {
	run_determinism('det_lock_expr', fixture_lock_expr)
}

fn test_transform_is_deterministic_or_expr() {
	run_determinism('det_or_expr', fixture_or_expr)
}

fn test_transform_is_deterministic_selector_expr() {
	run_determinism('det_selector_expr', fixture_selector_expr)
}

fn test_transform_is_deterministic_index_expr() {
	run_determinism('det_index_expr', fixture_index_expr)
}

fn test_transform_is_deterministic_comptime_expr() {
	run_determinism('det_comptime_expr', fixture_comptime_expr)
}

fn test_transform_is_deterministic_init_expr() {
	run_determinism('det_init_expr', fixture_init_expr)
}

fn test_transform_is_deterministic_return_stmt() {
	run_determinism('det_return_stmt', fixture_return_stmt)
}

fn test_transform_is_deterministic_ident() {
	run_determinism('det_ident', fixture_ident)
}

fn test_transform_is_deterministic_keyword_operator() {
	run_determinism('det_keyword_operator', fixture_keyword_operator)
}

fn test_transform_is_deterministic_assert_stmt() {
	run_determinism('det_assert_stmt', fixture_assert_stmt)
}

fn test_transform_is_deterministic_tuple_if_assign() {
	run_determinism('det_tuple_if_assign', fixture_tuple_if_assign)
}

fn test_transform_is_deterministic_tuple_call_assign() {
	run_determinism('det_tuple_call_assign', fixture_tuple_call_assign)
}

fn test_transform_is_deterministic_lock_stmt() {
	run_determinism('det_lock_stmt', fixture_lock_stmt)
}

fn test_transform_is_deterministic_if_guard_assign() {
	run_determinism('det_if_guard_assign', fixture_if_guard_assign)
}

fn test_transform_is_deterministic_if_expr_assign() {
	run_determinism('det_if_expr_assign', fixture_if_expr_assign)
}

fn test_transform_is_deterministic_return_if_expr() {
	run_determinism('det_return_if_expr', fixture_return_if_expr)
}

fn test_transform_is_deterministic_return_match() {
	run_determinism('det_return_match', fixture_return_match)
}

fn test_transform_is_deterministic_or_stmt() {
	run_determinism('det_or_stmt', fixture_or_stmt)
}

fn test_transform_is_deterministic_or_assign() {
	run_determinism('det_or_assign', fixture_or_assign)
}

fn test_transform_is_deterministic_or_return() {
	run_determinism('det_or_return', fixture_or_return)
}

fn test_transform_is_deterministic_for_in_map() {
	run_determinism('det_for_in_map', fixture_for_in_map)
}

// --- parity: transform_files vs transform_files_from_flat ---
//
// The streaming-from-flat path is the seed for the upcoming
// transformer-writes-flat work. Each fixture is parsed ONCE so the
// filenames embedded in pos info match; both transformer paths get
// independent Transformer instances over the same parsed input and shared
// env. A drift here indicates either a pre_pass_from_flat seeding bug or
// in-flight rehydration losing a node, both of which are exactly what
// this harness exists to catch.

fn run_parity(label string, src string) {
	p := parse_transformer_fixture(src)
	leg := run_legacy_transform(p)
	flt := run_from_flat_transform(p)
	assert_transform_signatures_equal(label, leg, flt)
}

fn test_flat_parity_plain_fn() {
	run_parity('parity_plain_fn', fixture_plain_fn)
}

fn test_flat_parity_infix_operator() {
	run_parity('parity_infix_operator', fixture_infix_operator)
}

fn test_flat_parity_array_contains() {
	run_parity('parity_array_contains', fixture_array_contains)
}

fn test_flat_parity_if_guard() {
	run_parity('parity_if_guard', fixture_if_guard)
}

fn test_flat_parity_or_block() {
	run_parity('parity_or_block', fixture_or_block)
}

fn test_flat_parity_sumtype_is_as() {
	run_parity('parity_sumtype_is_as', fixture_sumtype_is_as)
}

fn test_flat_parity_string_interp() {
	run_parity('parity_string_interp', fixture_string_interp)
}

fn test_flat_parity_global_init() {
	run_parity('parity_global_init', fixture_global_init)
}

fn test_flat_parity_const_decl() {
	run_parity('parity_const_decl', fixture_const_decl)
}

fn test_flat_parity_paren_expr() {
	run_parity('parity_paren_expr', fixture_paren_expr)
}

fn test_flat_parity_prefix_expr() {
	run_parity('parity_prefix_expr', fixture_prefix_expr)
}

fn test_flat_parity_modifier_expr() {
	run_parity('parity_modifier_expr', fixture_modifier_expr)
}

fn test_flat_parity_lambda_expr() {
	run_parity('parity_lambda_expr', fixture_lambda_expr)
}

fn test_flat_parity_fn_literal() {
	run_parity('parity_fn_literal', fixture_fn_literal)
}

fn test_flat_parity_postfix_expr() {
	run_parity('parity_postfix_expr', fixture_postfix_expr)
}

fn test_flat_parity_cast_expr() {
	run_parity('parity_cast_expr', fixture_cast_expr)
}

fn test_flat_parity_field_init() {
	run_parity('parity_field_init', fixture_field_init)
}

fn test_flat_parity_as_cast_expr() {
	run_parity('parity_as_cast_expr', fixture_as_cast_expr)
}

fn test_flat_parity_unsafe_expr() {
	run_parity('parity_unsafe_expr', fixture_unsafe_expr)
}

fn test_flat_parity_lock_expr() {
	run_parity('parity_lock_expr', fixture_lock_expr)
}

fn test_flat_parity_or_expr() {
	run_parity('parity_or_expr', fixture_or_expr)
}

fn test_flat_parity_selector_expr() {
	run_parity('parity_selector_expr', fixture_selector_expr)
}

fn test_flat_parity_index_expr() {
	run_parity('parity_index_expr', fixture_index_expr)
}

fn test_flat_parity_comptime_expr() {
	run_parity('parity_comptime_expr', fixture_comptime_expr)
}

fn test_flat_parity_init_expr() {
	run_parity('parity_init_expr', fixture_init_expr)
}

fn test_flat_parity_return_stmt() {
	run_parity('parity_return_stmt', fixture_return_stmt)
}

fn test_flat_parity_ident() {
	run_parity('parity_ident', fixture_ident)
}

fn test_flat_parity_keyword_operator() {
	run_parity('parity_keyword_operator', fixture_keyword_operator)
}

fn test_flat_parity_assert_stmt() {
	run_parity('parity_assert_stmt', fixture_assert_stmt)
}

fn test_flat_parity_tuple_if_assign() {
	run_parity('parity_tuple_if_assign', fixture_tuple_if_assign)
}

fn test_flat_parity_tuple_call_assign() {
	run_parity('parity_tuple_call_assign', fixture_tuple_call_assign)
}

fn test_flat_parity_lock_stmt() {
	run_parity('parity_lock_stmt', fixture_lock_stmt)
}

fn test_flat_parity_if_guard_assign() {
	run_parity('parity_if_guard_assign', fixture_if_guard_assign)
}

fn test_flat_parity_if_expr_assign() {
	run_parity('parity_if_expr_assign', fixture_if_expr_assign)
}

fn test_flat_parity_return_if_expr() {
	run_parity('parity_return_if_expr', fixture_return_if_expr)
}

fn test_flat_parity_return_match() {
	run_parity('parity_return_match', fixture_return_match)
}

fn test_flat_parity_or_stmt() {
	run_parity('parity_or_stmt', fixture_or_stmt)
}

fn test_flat_parity_or_assign() {
	run_parity('parity_or_assign', fixture_or_assign)
}

fn test_flat_parity_or_return() {
	run_parity('parity_or_return', fixture_or_return)
}

fn test_flat_parity_for_in_map() {
	run_parity('parity_for_in_map', fixture_for_in_map)
}

// --- parity: check_files vs check_flat upstream ---
//
// The flat-input checker (Checker.check_flat) is the next migration row in
// the parse → check → transform → markused → SSA chain. This family asserts
// that the env produced
// by check_flat is shape-compatible with the env from check_files — by
// running an identical transform over each and comparing signatures of
// the result.
//
// The transformer reads from env (type lookups, method tables) for many
// rewrites; any divergence in env state surfaces as a transformer-output
// drift here, with the diff narrowing the culprit to one fixture. This
// is the same pattern as the markused harness, but at the layer above.

fn run_check_flat_parity(label string, src string) {
	// Parse ONCE: same filename embedded in pos info, then run two
	// independent checkers (legacy / flat) against shared parser output
	// but separate env instances. Each env then feeds an independent
	// Transformer; outputs must produce identical signatures.
	tmp := '/tmp/v2_transformer_diff_${os.getpid()}_${transformer_rand_suffix()}.v'
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

	mut env_legacy := types.Environment.new()
	mut ck_legacy := types.Checker.new(prefs, fs, env_legacy)
	ck_legacy.check_files(files)

	mut env_flat := types.Environment.new()
	mut ck_flat := types.Checker.new(prefs, fs, env_flat)
	ck_flat.check_flat(&flat)

	mut t_a := Transformer.new_with_pref(env_legacy, prefs)
	leg := t_a.transform_files(files)
	mut t_b := Transformer.new_with_pref(env_flat, prefs)
	flt := t_b.transform_files(files)
	assert_transform_signatures_equal(label, leg, flt)
}

fn test_check_flat_parity_plain_fn() {
	run_check_flat_parity('check_flat_plain_fn', fixture_plain_fn)
}

fn test_check_flat_parity_infix_operator() {
	run_check_flat_parity('check_flat_infix_operator', fixture_infix_operator)
}

fn test_check_flat_parity_array_contains() {
	run_check_flat_parity('check_flat_array_contains', fixture_array_contains)
}

fn test_check_flat_parity_if_guard() {
	run_check_flat_parity('check_flat_if_guard', fixture_if_guard)
}

fn test_check_flat_parity_or_block() {
	run_check_flat_parity('check_flat_or_block', fixture_or_block)
}

fn test_check_flat_parity_sumtype_is_as() {
	run_check_flat_parity('check_flat_sumtype_is_as', fixture_sumtype_is_as)
}

fn test_check_flat_parity_string_interp() {
	run_check_flat_parity('check_flat_string_interp', fixture_string_interp)
}

fn test_check_flat_parity_global_init() {
	run_check_flat_parity('check_flat_global_init', fixture_global_init)
}

fn test_check_flat_parity_const_decl() {
	run_check_flat_parity('check_flat_const_decl', fixture_const_decl)
}

fn test_check_flat_parity_paren_expr() {
	run_check_flat_parity('check_flat_paren_expr', fixture_paren_expr)
}

fn test_check_flat_parity_prefix_expr() {
	run_check_flat_parity('check_flat_prefix_expr', fixture_prefix_expr)
}

fn test_check_flat_parity_modifier_expr() {
	run_check_flat_parity('check_flat_modifier_expr', fixture_modifier_expr)
}

fn test_check_flat_parity_lambda_expr() {
	run_check_flat_parity('check_flat_lambda_expr', fixture_lambda_expr)
}

fn test_check_flat_parity_fn_literal() {
	run_check_flat_parity('check_flat_fn_literal', fixture_fn_literal)
}

fn test_check_flat_parity_postfix_expr() {
	run_check_flat_parity('check_flat_postfix_expr', fixture_postfix_expr)
}

fn test_check_flat_parity_cast_expr() {
	run_check_flat_parity('check_flat_cast_expr', fixture_cast_expr)
}

fn test_check_flat_parity_field_init() {
	run_check_flat_parity('check_flat_field_init', fixture_field_init)
}

fn test_check_flat_parity_as_cast_expr() {
	run_check_flat_parity('check_flat_as_cast_expr', fixture_as_cast_expr)
}

fn test_check_flat_parity_unsafe_expr() {
	run_check_flat_parity('check_flat_unsafe_expr', fixture_unsafe_expr)
}

fn test_check_flat_parity_lock_expr() {
	run_check_flat_parity('check_flat_lock_expr', fixture_lock_expr)
}

fn test_check_flat_parity_or_expr() {
	run_check_flat_parity('check_flat_or_expr', fixture_or_expr)
}

fn test_check_flat_parity_selector_expr() {
	run_check_flat_parity('check_flat_selector_expr', fixture_selector_expr)
}

fn test_check_flat_parity_index_expr() {
	run_check_flat_parity('check_flat_index_expr', fixture_index_expr)
}

fn test_check_flat_parity_comptime_expr() {
	run_check_flat_parity('check_flat_comptime_expr', fixture_comptime_expr)
}

fn test_check_flat_parity_init_expr() {
	run_check_flat_parity('check_flat_init_expr', fixture_init_expr)
}

fn test_check_flat_parity_return_stmt() {
	run_check_flat_parity('check_flat_return_stmt', fixture_return_stmt)
}

fn test_check_flat_parity_ident() {
	run_check_flat_parity('check_flat_ident', fixture_ident)
}

fn test_check_flat_parity_keyword_operator() {
	run_check_flat_parity('check_flat_keyword_operator', fixture_keyword_operator)
}

fn test_check_flat_parity_assert_stmt() {
	run_check_flat_parity('check_flat_assert_stmt', fixture_assert_stmt)
}

fn test_check_flat_parity_tuple_if_assign() {
	run_check_flat_parity('check_flat_tuple_if_assign', fixture_tuple_if_assign)
}

fn test_check_flat_parity_tuple_call_assign() {
	run_check_flat_parity('check_flat_tuple_call_assign', fixture_tuple_call_assign)
}

fn test_check_flat_parity_lock_stmt() {
	run_check_flat_parity('check_flat_lock_stmt', fixture_lock_stmt)
}

fn test_check_flat_parity_if_guard_assign() {
	run_check_flat_parity('check_flat_if_guard_assign', fixture_if_guard_assign)
}

fn test_check_flat_parity_if_expr_assign() {
	run_check_flat_parity('check_flat_if_expr_assign', fixture_if_expr_assign)
}

fn test_check_flat_parity_return_if_expr() {
	run_check_flat_parity('check_flat_return_if_expr', fixture_return_if_expr)
}

fn test_check_flat_parity_return_match() {
	run_check_flat_parity('check_flat_return_match', fixture_return_match)
}

fn test_check_flat_parity_or_stmt() {
	run_check_flat_parity('check_flat_or_stmt', fixture_or_stmt)
}

fn test_check_flat_parity_or_assign() {
	run_check_flat_parity('check_flat_or_assign', fixture_or_assign)
}

fn test_check_flat_parity_or_return() {
	run_check_flat_parity('check_flat_or_return', fixture_or_return)
}

fn test_check_flat_parity_for_in_map() {
	run_check_flat_parity('check_flat_for_in_map', fixture_for_in_map)
}

// --- parity: transform_files vs transform_files_to_flat ---
//
// transform_files_to_flat is the API wedge for the future
// "transformer writes directly into a FlatBuilder" port. Today it
// composes transform_files_from_flat with a boundary flatten_files();
// callers that only need flat output route through here and will get the
// eventual peak-memory win
// without further changes.
//
// This row pins the contract: the FlatAst returned by
// transform_files_to_flat must have the same signature as
// flatten_files(transform_files(files)). When the internals are rewritten
// to skip the boundary flatten, this is the regression net.

fn run_to_flat_parity(label string, src string) {
	p := parse_transformer_fixture(src)
	leg := run_legacy_transform(p)
	leg_sig := transform_signature(leg)

	mut t := Transformer.new_with_pref(p.env, p.prefs)
	new_flat, _ := t.transform_files_to_flat(&p.flat, [])
	flt_sig := new_flat.signature()

	if leg_sig == flt_sig {
		return
	}
	pa, pb := dump_signature_pair(label, leg_sig, flt_sig)
	eprintln('[${label}] transform_files_to_flat signature diverged from legacy.')
	eprintln('  legacy: ${pa}')
	eprintln('  to_flat: ${pb}')
	eprintln('  diff with: diff -u ${pa} ${pb}')
	assert false, '${label}: transform_files_to_flat output diverged (see /tmp dumps above)'
}

fn test_to_flat_parity_plain_fn() {
	run_to_flat_parity('to_flat_plain_fn', fixture_plain_fn)
}

fn test_to_flat_parity_infix_operator() {
	run_to_flat_parity('to_flat_infix_operator', fixture_infix_operator)
}

fn test_to_flat_parity_array_contains() {
	run_to_flat_parity('to_flat_array_contains', fixture_array_contains)
}

fn test_to_flat_parity_if_guard() {
	run_to_flat_parity('to_flat_if_guard', fixture_if_guard)
}

fn test_to_flat_parity_or_block() {
	run_to_flat_parity('to_flat_or_block', fixture_or_block)
}

fn test_to_flat_parity_sumtype_is_as() {
	run_to_flat_parity('to_flat_sumtype_is_as', fixture_sumtype_is_as)
}

fn test_to_flat_parity_string_interp() {
	run_to_flat_parity('to_flat_string_interp', fixture_string_interp)
}

fn test_to_flat_parity_global_init() {
	run_to_flat_parity('to_flat_global_init', fixture_global_init)
}

fn test_to_flat_parity_const_decl() {
	run_to_flat_parity('to_flat_const_decl', fixture_const_decl)
}

fn test_to_flat_parity_paren_expr() {
	run_to_flat_parity('to_flat_paren_expr', fixture_paren_expr)
}

fn test_to_flat_parity_prefix_expr() {
	run_to_flat_parity('to_flat_prefix_expr', fixture_prefix_expr)
}

fn test_to_flat_parity_modifier_expr() {
	run_to_flat_parity('to_flat_modifier_expr', fixture_modifier_expr)
}

fn test_to_flat_parity_lambda_expr() {
	run_to_flat_parity('to_flat_lambda_expr', fixture_lambda_expr)
}

fn test_to_flat_parity_fn_literal() {
	run_to_flat_parity('to_flat_fn_literal', fixture_fn_literal)
}

fn test_to_flat_parity_postfix_expr() {
	run_to_flat_parity('to_flat_postfix_expr', fixture_postfix_expr)
}

fn test_to_flat_parity_cast_expr() {
	run_to_flat_parity('to_flat_cast_expr', fixture_cast_expr)
}

fn test_to_flat_parity_field_init() {
	run_to_flat_parity('to_flat_field_init', fixture_field_init)
}

fn test_to_flat_parity_as_cast_expr() {
	run_to_flat_parity('to_flat_as_cast_expr', fixture_as_cast_expr)
}

fn test_to_flat_parity_unsafe_expr() {
	run_to_flat_parity('to_flat_unsafe_expr', fixture_unsafe_expr)
}

fn test_to_flat_parity_lock_expr() {
	run_to_flat_parity('to_flat_lock_expr', fixture_lock_expr)
}

fn test_to_flat_parity_or_expr() {
	run_to_flat_parity('to_flat_or_expr', fixture_or_expr)
}

fn test_to_flat_parity_selector_expr() {
	run_to_flat_parity('to_flat_selector_expr', fixture_selector_expr)
}

fn test_to_flat_parity_index_expr() {
	run_to_flat_parity('to_flat_index_expr', fixture_index_expr)
}

fn test_to_flat_parity_comptime_expr() {
	run_to_flat_parity('to_flat_comptime_expr', fixture_comptime_expr)
}

fn test_to_flat_parity_init_expr() {
	run_to_flat_parity('to_flat_init_expr', fixture_init_expr)
}

fn test_to_flat_parity_return_stmt() {
	run_to_flat_parity('to_flat_return_stmt', fixture_return_stmt)
}

fn test_to_flat_parity_ident() {
	run_to_flat_parity('to_flat_ident', fixture_ident)
}

fn test_to_flat_parity_keyword_operator() {
	run_to_flat_parity('to_flat_keyword_operator', fixture_keyword_operator)
}

fn test_to_flat_parity_assert_stmt() {
	run_to_flat_parity('to_flat_assert_stmt', fixture_assert_stmt)
}

fn test_to_flat_parity_tuple_if_assign() {
	run_to_flat_parity('to_flat_tuple_if_assign', fixture_tuple_if_assign)
}

fn test_to_flat_parity_tuple_call_assign() {
	run_to_flat_parity('to_flat_tuple_call_assign', fixture_tuple_call_assign)
}

fn test_to_flat_parity_lock_stmt() {
	run_to_flat_parity('to_flat_lock_stmt', fixture_lock_stmt)
}

fn test_to_flat_parity_if_guard_assign() {
	run_to_flat_parity('to_flat_if_guard_assign', fixture_if_guard_assign)
}

fn test_to_flat_parity_if_expr_assign() {
	run_to_flat_parity('to_flat_if_expr_assign', fixture_if_expr_assign)
}

fn test_to_flat_parity_return_if_expr() {
	run_to_flat_parity('to_flat_return_if_expr', fixture_return_if_expr)
}

fn test_to_flat_parity_return_match() {
	run_to_flat_parity('to_flat_return_match', fixture_return_match)
}

fn test_to_flat_parity_or_stmt() {
	run_to_flat_parity('to_flat_or_stmt', fixture_or_stmt)
}

fn test_to_flat_parity_or_assign() {
	run_to_flat_parity('to_flat_or_assign', fixture_or_assign)
}

fn test_to_flat_parity_or_return() {
	run_to_flat_parity('to_flat_or_return', fixture_or_return)
}

fn test_to_flat_parity_for_in_map() {
	run_to_flat_parity('to_flat_for_in_map', fixture_for_in_map)
}

// --- parity: per-file flat-write API vs reference rehydrate+transform+append ---
//
// `transform_file_index_to_flat` (flat_write.v) is the per-file entry point
// for the multi-session port that progressively rewrites the transformer to
// emit flat nodes directly into a FlatBuilder. Today the body is a behaviour-
// preserving decomposition of the loop inside `transform_files_from_flat`;
// sessions 2..N replace one rewrite site at a time inside the body.
//
// This row pins the per-file invariant: driving the new API in a loop must
// produce a FlatBuilder bit-equal to manually doing rehydrate + transform +
// append_file for each file index. Each future session must keep this
// invariant green after porting its rewrite site.
//
// The 4th row (to_flat_*) covers the wedge-level invariant against legacy.
// This row is one layer below: it isolates the per-file API so a regression
// inside the new body fails here independently of any post_pass change.

fn run_per_file_parity(label string, src string) {
	p := parse_transformer_fixture(src)

	// Reference: manual rehydrate + transform + append per file index.
	mut t_ref := Transformer.new_with_pref(p.env, p.prefs)
	t_ref.pre_pass_from_flat(&p.flat)
	mut ref_builder := ast.new_flat_builder()
	for fi in 0 .. p.flat.files.len {
		src_arr := p.flat.to_files_range(fi, fi + 1)
		if src_arr.len == 0 {
			continue
		}
		transformed := t_ref.transform_file_pub(src_arr[0])
		ref_builder.append_file(transformed)
	}
	ref_sig := ref_builder.flat.signature()

	// New per-file API driven in the same loop, fresh Transformer instance so
	// no internal counter state leaks between the two paths.
	mut t_api := Transformer.new_with_pref(p.env, p.prefs)
	t_api.pre_pass_from_flat(&p.flat)
	mut api_builder := ast.new_flat_builder()
	for fi in 0 .. p.flat.files.len {
		t_api.transform_file_index_to_flat(&p.flat, fi, mut api_builder)
	}
	api_sig := api_builder.flat.signature()

	if ref_sig == api_sig {
		return
	}
	pa, pb := dump_signature_pair(label, ref_sig, api_sig)
	eprintln('[${label}] per-file API signature diverged from reference loop.')
	eprintln('  ref: ${pa}')
	eprintln('  api: ${pb}')
	eprintln('  diff with: diff -u ${pa} ${pb}')
	assert false, '${label}: transform_file_index_to_flat output diverged (see /tmp dumps above)'
}

fn test_per_file_parity_plain_fn() {
	run_per_file_parity('per_file_plain_fn', fixture_plain_fn)
}

fn test_per_file_parity_infix_operator() {
	run_per_file_parity('per_file_infix_operator', fixture_infix_operator)
}

fn test_per_file_parity_array_contains() {
	run_per_file_parity('per_file_array_contains', fixture_array_contains)
}

fn test_per_file_parity_if_guard() {
	run_per_file_parity('per_file_if_guard', fixture_if_guard)
}

fn test_per_file_parity_or_block() {
	run_per_file_parity('per_file_or_block', fixture_or_block)
}

fn test_per_file_parity_sumtype_is_as() {
	run_per_file_parity('per_file_sumtype_is_as', fixture_sumtype_is_as)
}

fn test_per_file_parity_string_interp() {
	run_per_file_parity('per_file_string_interp', fixture_string_interp)
}

fn test_per_file_parity_global_init() {
	run_per_file_parity('per_file_global_init', fixture_global_init)
}

fn test_per_file_parity_const_decl() {
	run_per_file_parity('per_file_const_decl', fixture_const_decl)
}

fn test_per_file_parity_paren_expr() {
	run_per_file_parity('per_file_paren_expr', fixture_paren_expr)
}

fn test_per_file_parity_prefix_expr() {
	run_per_file_parity('per_file_prefix_expr', fixture_prefix_expr)
}

fn test_per_file_parity_modifier_expr() {
	run_per_file_parity('per_file_modifier_expr', fixture_modifier_expr)
}

fn test_per_file_parity_lambda_expr() {
	run_per_file_parity('per_file_lambda_expr', fixture_lambda_expr)
}

fn test_per_file_parity_fn_literal() {
	run_per_file_parity('per_file_fn_literal', fixture_fn_literal)
}

fn test_per_file_parity_postfix_expr() {
	run_per_file_parity('per_file_postfix_expr', fixture_postfix_expr)
}

fn test_per_file_parity_cast_expr() {
	run_per_file_parity('per_file_cast_expr', fixture_cast_expr)
}

fn test_per_file_parity_field_init() {
	run_per_file_parity('per_file_field_init', fixture_field_init)
}

fn test_per_file_parity_as_cast_expr() {
	run_per_file_parity('per_file_as_cast_expr', fixture_as_cast_expr)
}

fn test_per_file_parity_unsafe_expr() {
	run_per_file_parity('per_file_unsafe_expr', fixture_unsafe_expr)
}

fn test_per_file_parity_lock_expr() {
	run_per_file_parity('per_file_lock_expr', fixture_lock_expr)
}

fn test_per_file_parity_or_expr() {
	run_per_file_parity('per_file_or_expr', fixture_or_expr)
}

fn test_per_file_parity_selector_expr() {
	run_per_file_parity('per_file_selector_expr', fixture_selector_expr)
}

fn test_per_file_parity_index_expr() {
	run_per_file_parity('per_file_index_expr', fixture_index_expr)
}

fn test_per_file_parity_comptime_expr() {
	run_per_file_parity('per_file_comptime_expr', fixture_comptime_expr)
}

fn test_per_file_parity_init_expr() {
	run_per_file_parity('per_file_init_expr', fixture_init_expr)
}

fn test_per_file_parity_return_stmt() {
	run_per_file_parity('per_file_return_stmt', fixture_return_stmt)
}

fn test_per_file_parity_ident() {
	run_per_file_parity('per_file_ident', fixture_ident)
}

fn test_per_file_parity_keyword_operator() {
	run_per_file_parity('per_file_keyword_operator', fixture_keyword_operator)
}

fn test_per_file_parity_assert_stmt() {
	run_per_file_parity('per_file_assert_stmt', fixture_assert_stmt)
}

fn test_per_file_parity_tuple_if_assign() {
	run_per_file_parity('per_file_tuple_if_assign', fixture_tuple_if_assign)
}

fn test_per_file_parity_tuple_call_assign() {
	run_per_file_parity('per_file_tuple_call_assign', fixture_tuple_call_assign)
}

fn test_per_file_parity_lock_stmt() {
	run_per_file_parity('per_file_lock_stmt', fixture_lock_stmt)
}

fn test_per_file_parity_if_guard_assign() {
	run_per_file_parity('per_file_if_guard_assign', fixture_if_guard_assign)
}

fn test_per_file_parity_if_expr_assign() {
	run_per_file_parity('per_file_if_expr_assign', fixture_if_expr_assign)
}

fn test_per_file_parity_return_if_expr() {
	run_per_file_parity('per_file_return_if_expr', fixture_return_if_expr)
}

fn test_per_file_parity_return_match() {
	run_per_file_parity('per_file_return_match', fixture_return_match)
}

fn test_per_file_parity_or_stmt() {
	run_per_file_parity('per_file_or_stmt', fixture_or_stmt)
}

fn test_per_file_parity_or_assign() {
	run_per_file_parity('per_file_or_assign', fixture_or_assign)
}

fn test_per_file_parity_or_return() {
	run_per_file_parity('per_file_or_return', fixture_or_return)
}

fn test_per_file_parity_for_in_map() {
	run_per_file_parity('per_file_for_in_map', fixture_for_in_map)
}

// --- parity: transform_files_to_flat_direct per-file subtree vs legacy ---
//
// `transform_files_to_flat_direct` is the native low-memory path: it keeps the
// existing whole-program prepare/monomorphize step, but emits each transformed
// file directly into a FlatBuilder instead of collecting a transformed
// []ast.File result. Its observable tree must match legacy transform output.
fn run_to_flat_direct_parity(label string, src string) {
	p := parse_transformer_fixture(src)
	leg := run_legacy_transform(p)
	leg_flat := ast.flatten_files(leg)

	mut t := Transformer.new_with_pref(p.env, p.prefs)
	new_flat := t.transform_files_to_flat_direct(p.files)

	if leg_flat.files.len != new_flat.files.len {
		assert false, '${label}: file count mismatch: legacy=${leg_flat.files.len} direct=${new_flat.files.len}'
		return
	}

	for i in 0 .. leg_flat.files.len {
		leg_stmts := leg_flat.child_at(leg_flat.files[i].file_id, 2)
		new_stmts := new_flat.child_at(new_flat.files[i].file_id, 2)
		leg_sub_sig := leg_flat.subtree_signature(leg_stmts)
		new_sub_sig := new_flat.subtree_signature(new_stmts)
		if leg_sub_sig == new_sub_sig {
			continue
		}
		pa, pb := dump_signature_pair('${label}_file${i}', leg_sub_sig, new_sub_sig)
		eprintln('[${label}] transform_files_to_flat_direct file ${i} subtree diverged from legacy.')
		eprintln('  legacy: ${pa}')
		eprintln('  direct: ${pb}')
		eprintln('  diff with: diff -u ${pa} ${pb}')
		assert false, '${label}: transform_files_to_flat_direct output diverged at file ${i} (see /tmp dumps above)'
	}
}

fn test_to_flat_direct_parity_plain_fn() {
	run_to_flat_direct_parity('to_flat_direct_plain_fn', fixture_plain_fn)
}

fn test_to_flat_direct_parity_if_guard_assign() {
	run_to_flat_direct_parity('to_flat_direct_if_guard_assign', fixture_if_guard_assign)
}

fn run_flat_input_to_flat_direct_matches_file_input_direct(label string, src string) {
	p_files := parse_transformer_fixture(src)
	p_flat := parse_transformer_fixture(src)

	mut t_files := Transformer.new_with_pref(p_files.env, p_files.prefs)
	files_direct := t_files.transform_files_to_flat_direct(p_files.files)

	mut t_flat := Transformer.new_with_pref(p_flat.env, p_flat.prefs)
	flat_direct := t_flat.transform_flat_to_flat_direct(&p_flat.flat, [])

	if files_direct.files.len != flat_direct.files.len {
		assert false, '${label}: file count mismatch: files=${files_direct.files.len} flat=${flat_direct.files.len}'
		return
	}
	for i in 0 .. files_direct.files.len {
		files_stmts := files_direct.child_at(files_direct.files[i].file_id, 2)
		flat_stmts := flat_direct.child_at(flat_direct.files[i].file_id, 2)
		files_sig := files_direct.subtree_signature(files_stmts)
		flat_sig := flat_direct.subtree_signature(flat_stmts)
		if files_sig == flat_sig {
			continue
		}
		pa, pb := dump_signature_pair('${label}_file${i}', files_sig, flat_sig)
		eprintln('[${label}] file ${i} subtree diverged.')
		eprintln('  files input: ${pa}')
		eprintln('  flat input: ${pb}')
		eprintln('  diff with: diff -u ${pa} ${pb}')
		assert false, '${label}: output diverged at file ${i} (see /tmp dumps above)'
	}
}

fn test_flat_input_to_flat_direct_matches_file_input_direct() {
	run_flat_input_to_flat_direct_matches_file_input_direct('flat_input_direct_if_guard_assign',
		fixture_if_guard_assign)
}

fn test_flat_input_to_flat_direct_monomorphizes_generics_like_file_input() {
	run_flat_input_to_flat_direct_matches_file_input_direct('flat_input_direct_generic_fn',
		fixture_generic_fn)
}

fn test_flat_input_to_flat_direct_map_index_array_push() {
	run_flat_input_to_flat_direct_matches_file_input_direct('flat_input_direct_map_index_array_push',
		fixture_map_index_array_push)
}

fn test_to_flat_direct_parity_for_in_map() {
	run_to_flat_direct_parity('to_flat_direct_for_in_map', fixture_for_in_map)
}

// --- parity: transform_files_to_flat_via_driver per-file subtree vs legacy ---
//
// `transform_files_to_flat_via_driver` is the s162 wedge that routes through
// the s161 `post_pass_to_flat` driver instead of the legacy `post_pass +
// flatten_files` boundary. Structurally equivalent to legacy on every file's
// stmts list, but NOT bit-equal in full `signature()` because intern order of
// post_pass-added strings differs between the two paths: the legacy path
// interns those strings DURING `flatten_files` walk (so later files' mod
// strings end up at higher indices), the new path interns all bare files
// first and post-pass-added strings AFTER (so later files' mod strings end
// up at lower indices). The `.file.extra` slot stores `intern(mod)` as a raw
// int, so the leak shows up in `signature()` even though the trees are
// content-identical.
//
// Workaround: compare per-file `subtree_signature(child_at(file_id, 2))`.
// File root edge 2 is the stmts list; its subtree never touches `.file.extra`
// so intern-order differences vanish. Every file's stmts list — including
// post_pass-mutated parts like prepended init calls or appended init fns —
// must match the legacy result.
//
// When SSA migrates to consume flat AST and `transform_files_to_flat` can
// drop its `[]ast.File` return, this row pins the alternative driver path so
// the swap doesn't regress.
fn run_to_flat_via_driver_parity(label string, src string) {
	p := parse_transformer_fixture(src)
	leg := run_legacy_transform(p)
	leg_flat := ast.flatten_files(leg)

	mut t := Transformer.new_with_pref(p.env, p.prefs)
	new_flat, _ := t.transform_files_to_flat_via_driver(&p.flat, [])

	if leg_flat.files.len != new_flat.files.len {
		assert false, '${label}: file count mismatch: legacy=${leg_flat.files.len} driver=${new_flat.files.len}'
		return
	}

	for i in 0 .. leg_flat.files.len {
		leg_stmts := leg_flat.child_at(leg_flat.files[i].file_id, 2)
		new_stmts := new_flat.child_at(new_flat.files[i].file_id, 2)
		leg_sub_sig := leg_flat.subtree_signature(leg_stmts)
		new_sub_sig := new_flat.subtree_signature(new_stmts)
		if leg_sub_sig == new_sub_sig {
			continue
		}
		pa, pb := dump_signature_pair('${label}_file${i}', leg_sub_sig, new_sub_sig)
		eprintln('[${label}] transform_files_to_flat_via_driver file ${i} subtree diverged from legacy.')
		eprintln('  legacy: ${pa}')
		eprintln('  driver: ${pb}')
		eprintln('  diff with: diff -u ${pa} ${pb}')
		assert false, '${label}: transform_files_to_flat_via_driver output diverged at file ${i} (see /tmp dumps above)'
	}
}

fn test_to_flat_via_driver_parity_plain_fn() {
	run_to_flat_via_driver_parity('to_flat_via_driver_plain_fn', fixture_plain_fn)
}

fn test_to_flat_via_driver_parity_infix_operator() {
	run_to_flat_via_driver_parity('to_flat_via_driver_infix_operator', fixture_infix_operator)
}

fn test_to_flat_via_driver_parity_array_contains() {
	run_to_flat_via_driver_parity('to_flat_via_driver_array_contains', fixture_array_contains)
}

fn test_to_flat_via_driver_parity_if_guard() {
	run_to_flat_via_driver_parity('to_flat_via_driver_if_guard', fixture_if_guard)
}

fn test_to_flat_via_driver_parity_or_block() {
	run_to_flat_via_driver_parity('to_flat_via_driver_or_block', fixture_or_block)
}

fn test_to_flat_via_driver_parity_global_init() {
	run_to_flat_via_driver_parity('to_flat_via_driver_global_init', fixture_global_init)
}

fn test_to_flat_via_driver_parity_const_decl() {
	run_to_flat_via_driver_parity('to_flat_via_driver_const_decl', fixture_const_decl)
}

fn test_to_flat_via_driver_parity_string_interp() {
	run_to_flat_via_driver_parity('to_flat_via_driver_string_interp', fixture_string_interp)
}

fn test_to_flat_via_driver_parity_assert_stmt() {
	run_to_flat_via_driver_parity('to_flat_via_driver_assert_stmt', fixture_assert_stmt)
}

fn test_to_flat_via_driver_parity_return_stmt() {
	run_to_flat_via_driver_parity('to_flat_via_driver_return_stmt', fixture_return_stmt)
}

// test_all_fixtures_produce_nonempty_signature guards against silent harness
// breakage: every fixture has at least main() so the signature must contain
// at least one FILE / fn body. A zero-length signature means parse / check /
// transform broke before we even compared.
fn test_all_fixtures_produce_nonempty_signature() {
	for i, src in all_transformer_fixtures() {
		p := parse_transformer_fixture(src)
		sig := transform_signature(run_legacy_transform(p))
		assert sig.len > 0, 'fixture #${i}: empty signature — pipeline broken before transformer'
	}
}
