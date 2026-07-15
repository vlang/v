import os
import v3.flat
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// parse_checked_source reads parse checked source input for v3 tests.
fn parse_checked_source(name string, source string) (&flat.FlatAst, &types.TypeChecker) {
	src := os.join_path(os.temp_dir(), 'v3_markused_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn parse_checked_project(name string, files map[string]string, main_file string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	os.rmdir_all(root) or {}
	for rel, source in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
	mut paths := []string{}
	if main_file.len > 0 {
		paths << os.join_path(root, main_file)
	}
	mut rel_paths := []string{}
	for rel, _ in files {
		if rel != main_file {
			rel_paths << rel
		}
	}
	rel_paths.sort()
	for rel in rel_paths {
		paths << os.join_path(root, rel)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn parse_checked_project_in_order(name string, rels []string, sources []string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	os.rmdir_all(root) or {}
	mut paths := []string{}
	for i, rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, sources[i]) or { panic(err) }
		paths << path
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

// mark_used_source updates mark used source state for v3 tests.
fn mark_used_source(name string, source string) map[string]bool {
	a, tc := parse_checked_source(name, source)
	return markused.mark_used(a, tc)
}

// build_v3_bin builds v3 bin data for v3 tests.
fn build_v3_bin(name string) string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// test_map_literals_seed_new_map_runtime_helper validates this v3 regression case.
fn test_map_literals_seed_new_map_runtime_helper() {
	used := mark_used_source('map_literal_new_map', '
fn make_map() map[string]int {
	return map[string]int{}
}

fn main() {
	_ := make_map()
}
')
	assert used['new_map']
}

// test_optional_map_or_seeds_new_map_runtime_helper validates this v3 regression case.
fn test_optional_map_or_seeds_new_map_runtime_helper() {
	used := mark_used_source('option_map_or_new_map', '
fn maybe_map() ?map[string]int {
	return none
}

fn main() {
	m := maybe_map() or { return }
	_ := m
}
')
	assert used['new_map']
}

// test_string_membership_seeds_contains_runtime_helpers validates this v3 regression case.
fn test_string_membership_seeds_contains_runtime_helpers() {
	used := mark_used_source('string_membership_contains', '
fn has_needle() bool {
	return "bc" in "abcd"
}

fn main() {
	_ := has_needle()
}
')
	assert used['string__contains']
	assert used['string__contains_u8']
}

// test_string_interpolation_seeds_string_plus_and_formatter_helpers
// validates this v3 regression case.
fn test_string_interpolation_seeds_string_plus_and_formatter_helpers() {
	used := mark_used_source('string_interp_plus_formatter', '
fn message(name string) string {
	return "hello \${name} \${true}"
}

fn main() {
	_ := message("v")
}
')
	assert used['string__plus']
	assert used['bool.str']
}

// test_print_bool_seeds_formatter_runtime_helper validates this v3 regression case.
fn test_print_bool_seeds_formatter_runtime_helper() {
	used := mark_used_source('print_bool_formatter', '
fn println(s string) {}

fn main() {
	println(true)
}
')
	assert used['bool.str']
}

// test_string_compound_assign_seeds_string_plus_runtime_helper validates this v3 regression case.
fn test_string_compound_assign_seeds_string_plus_runtime_helper() {
	used := mark_used_source('string_plus_assign', '
fn main() {
	mut s := "a"
	s += "b"
	_ := s
}
')
	assert used['string__plus']
}

fn test_generic_struct_operator_roots_operator_dependencies() {
	used := mark_used_source('generic_struct_operator_dependencies', '
struct Time {
	seconds int
}

fn (t Time) unix() int {
	return t.seconds
}

fn (a Time) < (b Time) bool {
	return a.unix() < b.unix()
}

fn min[T](a T, b T) T {
	if a < b {
		return a
	}
	return b
}

fn main() {
	a := Time{
		seconds: 1
	}
	b := Time{
		seconds: 2
	}
	_ := min(a, b)
}
')
	assert used['Time.<']
	assert used['Time.unix']
}

fn test_for_in_const_array_roots_receiver_method() {
	used := mark_used_source('for_in_const_array_receiver_method', '
enum FlagId {
	after_context
}

struct Doc {}

const flags = [FlagId.after_context]

fn (id FlagId) doc_short() Doc {
	_ := id
	return Doc{}
}

fn (d Doc) replace(from string, to string) string {
	_ := d
	_ := from
	_ := to
	return "Show n lines after each match."
}

fn main() {
	for flag in flags {
		_ := flag.doc_short().replace("NUM", "n")
	}
}
')
	assert used['FlagId.doc_short']
	assert used['Doc.replace']
}

fn test_error_argument_roots_nested_receiver_and_static_methods() {
	used := mark_used_source('error_arg_nested_receiver_static_methods', '
struct ErrorKind {}

struct GlobError {
	kind ErrorKind
}

struct Parser {}

fn ErrorKind.unopened_alternates() ErrorKind {
	return ErrorKind{}
}

fn (e GlobError) msg() string {
	_ := e
	return "unopened alternates"
}

fn (p Parser) mk_error(kind ErrorKind) GlobError {
	return GlobError{
		kind: kind
	}
}

fn (mut p Parser) pop_alternate() ! {
	return error(p.mk_error(ErrorKind.unopened_alternates()).msg())
}

fn main() {
	mut p := Parser{}
	p.pop_alternate() or { return }
}
')
	assert used['Parser.mk_error']
	assert used['GlobError.msg']
	assert used['ErrorKind.unopened_alternates']
}

fn test_nested_fn_literal_roots_callback_helper_dependencies() {
	used := mark_used_source('nested_fn_literal_callback_helpers', '
struct Match {}

fn Match.new() Match {
	return Match{}
}

struct Caps {}

fn Caps.overall(m Match) Caps {
	_ := m
	return Caps{}
}

fn (c Caps) get(i int) ?Match {
	_ := c
	_ := i
	return Match{}
}

fn no_index(name string) ?int {
	_ := name
	return none
}

fn append_match(mut dst []u8, m Match) {
	_ := m
	dst << u8(1)
}

fn interpolate(append fn (int, mut []u8), name_to_index fn (string) ?int, mut dst []u8) {
	_ := name_to_index
	append(0, mut dst)
}

fn find_iter(matched fn (Match) bool) {
	_ := matched(Match.new())
}

fn replace(mut dst []u8) {
	find_iter(fn [mut dst] (m Match) bool {
		caps := Caps.overall(m)
		interpolate(fn [caps] (i int, mut out []u8) {
			cap_match := caps.get(i) or {
				return
			}
			append_match(mut out, cap_match)
		}, no_index, mut dst)
		return true
	})
}

fn main() {
	mut dst := []u8{}
	replace(mut dst)
}
')
	assert used['Caps.overall']
	assert used['Caps.get']
	assert used['append_match']
	assert used['interpolate']
	assert used['no_index']
	assert used['Match.new']
}

fn test_imported_nested_fn_literal_roots_private_callback_helpers() {
	a, tc := parse_checked_project('imported_nested_fn_literal_callback_helpers', {
		'main.v':     'module main

import worker

fn main() {
	mut dst := []u8{}
	worker.replace(mut dst)
}
'
		'worker/w.v': 'module worker

struct Match {}

fn Match.new() Match {
	return Match{}
}

struct Caps {}

fn Caps.overall(m Match) Caps {
	_ := m
	return Caps{}
}

fn (c Caps) get(i int) ?Match {
	_ := c
	_ := i
	return Match{}
}

fn no_index(name string) ?int {
	_ := name
	return none
}

fn append_match(mut dst []u8, m Match) {
	_ := m
	dst << u8(1)
}

fn interpolate(append fn (int, mut []u8), name_to_index fn (string) ?int, mut dst []u8) {
	_ := name_to_index
	append(0, mut dst)
}

fn find_iter(matched fn (Match) bool) {
	_ := matched(Match.new())
}

pub fn replace(mut dst []u8) {
	find_iter(fn [mut dst] (m Match) bool {
		caps := Caps.overall(m)
		interpolate(fn [caps] (i int, mut out []u8) {
			cap_match := caps.get(i) or {
				return
			}
			append_match(mut out, cap_match)
		}, no_index, mut dst)
		return true
	})
}
'
	}, 'main.v')
	used := markused.mark_used(a, tc)
	assert used['worker.Caps.overall']
	assert used['worker.Caps.get']
	assert used['worker.append_match']
	assert used['worker.interpolate']
	assert used['worker.no_index']
	assert used['worker.Match.new']
}

fn test_map_str_seeds_string_plus_runtime_helper() {
	used := mark_used_source('map_str_string_plus', '
fn render(m map[string]int) string {
	return m.str()
}

fn main() {
	_ := render(map[string]int{})
}
')
	assert used['string__plus']
}

fn test_print_map_seeds_string_plus_runtime_helper() {
	used := mark_used_source('print_map_string_plus', '
fn main() {
	m := {
		"a": 1
	}
	println(m)
}
')
	assert used['string__plus']
}

fn test_moduleless_export_after_module_file_is_rooted() {
	a, tc := parse_checked_project_in_order('moduleless_export_after_module', [
		'helper/helper.v',
		'lonely.v',
	], ['module helper

pub fn helper_marker() int {
	return 1
}
', "@[export: 'raw_lonely']
fn lonely() int {
	return 7
}
"])
	used := markused.mark_used(a, tc)
	assert used['lonely']
}

fn test_receiver_method_call_in_selector_assign_rhs_is_used() {
	used := mark_used_source('receiver_method_selector_assign_rhs', '
struct Builder {
mut:
	name string
}

fn (b &Builder) main_module_name() string {
	return "main"
}

fn build_with_options() string {
	mut b := Builder{}
	b.name = b.main_module_name()
	return b.name
}

fn main() {
	_ := build_with_options()
}
')
	assert used['Builder.main_module_name']
}

fn test_unreachable_interface_implementer_method_is_not_rooted() {
	used := mark_used_source('unreachable_interface_implementer', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

fn main() {}
')
	assert !used['File.read']
}

fn test_reachable_interface_dispatch_keeps_implementer_method() {
	used := mark_used_source('reachable_interface_dispatch', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

fn call_reader(r Reader) int {
	return r.read()
}

fn main() {
	_ := call_reader(File{})
}
')
	assert used['File.read']
}

fn test_global_interface_dispatch_keeps_implementer_method() {
	used := mark_used_source('global_interface_dispatch', '
interface Reader {
	read() int
}

struct File {}

__global default_reader &Reader

fn (f File) read() int {
	return 1
}

fn call_default_reader() int {
	return default_reader.read()
}

fn main() {
	_ := call_default_reader()
}
')
	assert used['Reader.read']
	assert used['File.read']
}

fn test_module_global_interface_dispatch_keeps_dispatch_stub() {
	mut a, mut tc := parse_checked_project('module_global_interface_dispatch', {
		'main.v':               'module main

import m

fn main() {
	m.error("x")
}
'
		'm/default.c.v':        'module m

__global default_logger &Logger

pub fn error(s string) {
	default_logger.error(s)
}
'
		'm/logger_interface.v': 'module m

pub enum Level {
	info
}

pub interface Logger {
	get_level() Level
mut:
	fatal(s string)
	error(s string)
	warn(s string)
	info(s string)
	debug(s string)
	set_level(level Level)
	set_always_flush(should_flush bool)
	free()
}
'
		'm/safe_log.v':         'module m

pub struct BaseLog {}

pub struct ThreadSafeLog {
	BaseLog
}

pub fn (l BaseLog) get_level() Level {
	_ := l
	return .info
}

pub fn (mut l ThreadSafeLog) fatal(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) error(s string) {}

pub fn (mut l ThreadSafeLog) warn(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) info(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) debug(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) set_level(level Level) {
	_ := l
	_ := level
}

pub fn (mut l ThreadSafeLog) set_always_flush(should_flush bool) {
	_ := l
	_ := should_flush
}

pub fn (mut l ThreadSafeLog) free() {
	_ := l
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['m.Logger.error']
	assert used['m.ThreadSafeLog.error']
	assert used['m.BaseLog.get_level'] == false
	used = transform.transform_with_used(mut a, tc, used)
	assert used['m.Logger.error']
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('m__Logger__error(')
	assert c_code.contains(': m__ThreadSafeLog__error')
}

fn test_unreachable_interface_dispatch_stub_is_not_emitted_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('unreachable_interface_dispatch_cgen', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

fn main() {}
')
	mut used := markused.mark_used(a, tc)
	assert !used['Reader.read']
	assert !used['File.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('Reader__read(')
}

fn test_short_interface_dispatch_does_not_emit_imported_name_collision_stub() {
	mut a, mut tc := parse_checked_project('interface_dispatch_name_collision', {
		'main.v':      'module main

import moda

interface Reader {
	read() int
}

struct Local {}

fn (l Local) read() int {
	return 1
}

fn call_reader(r Reader) int {
	return r.read()
}

fn main() {
	_ := call_reader(Local{})
}
'
		'moda/moda.v': 'module moda

interface Reader {
	read(path string) string
}

struct Remote {}

fn (r Remote) read(path string) string {
	return path
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['Reader.read']
	assert !used['moda.Reader.read']
	assert !used['moda.Remote.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('Reader__read(')
	assert !c_code.contains('moda__Reader__read(')
}

fn test_local_receiver_method_does_not_root_imported_interface_by_short_name() {
	mut a, mut tc := parse_checked_project('local_receiver_imported_interface_short_name', {
		'main.v':      'module main

import moda

struct Reader {}

fn (r Reader) read() int {
	return 1
}

fn main() {
	_ := Reader{}.read()
}
'
		'moda/moda.v': 'module moda

pub interface Reader {
	read() int
}

pub struct Remote {}

pub fn (r Remote) read() int {
	return 2
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['Reader.read']
	assert !used['moda.Reader.read']
	assert !used['moda.Remote.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('Reader__read(')
	assert !c_code.contains('moda__Reader__read(')
	assert !c_code.contains('moda__Remote__read(')
}

fn test_imported_interface_dispatch_is_emitted_when_exactly_used() {
	mut a, mut tc := parse_checked_project('imported_interface_dispatch_used', {
		'main.v':      'module main

import moda

fn call_reader(r moda.Reader) string {
	return r.read("ok")
}

fn main() {
	_ := call_reader(moda.Remote{})
}
'
		'moda/moda.v': 'module moda

pub interface Reader {
	read(path string) string
}

pub struct Remote {}

pub fn (r Remote) read(path string) string {
	return path
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['moda.Reader.read']
	assert used['moda.Remote.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('moda__Reader__read(')
	assert c_code.contains('moda__Remote__read(')
}

fn test_interface_dispatch_target_does_not_use_bare_method_key_for_imported_homonym() {
	mut a, mut tc := parse_checked_project('interface_dispatch_target_homonym', {
		'main.v':      'module main

import moda

interface Reader {
	read() int
}

struct Local {}

fn (l Local) read() int {
	return 1
}

fn read() int {
	return 9
}

fn call_reader(r Reader) int {
	return r.read()
}

fn main() {
	_ := read()
	_ := call_reader(Local{})
}
'
		'moda/moda.v': 'module moda

pub struct Remote {}

pub fn (r Remote) read() int {
	return 2
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['read']
	assert used['Reader.read']
	used.delete('moda.Remote.read')
	used.delete('Remote.read')
	used.delete('moda__Remote__read')
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('Reader__read(')
	assert !c_code.contains('moda__Remote__read(')
}

fn test_unused_main_method_with_interface_dispatch_is_pruned_with_stub() {
	mut a, mut tc := parse_checked_source('unused_main_method_interface_dispatch_cgen', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

struct X {}

fn (x X) unused(r Reader) int {
	return r.read()
}

fn main() {}
')
	mut used := markused.mark_used(a, tc)
	assert !used['X.unused']
	assert !used['Reader.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('X__unused(')
	assert !c_code.contains('Reader__read(')
}

fn test_unused_main_helper_with_method_call_is_pruned_with_method() {
	mut a, mut tc := parse_checked_source('unused_main_helper_method_call_cgen',
		'module main\n\nstruct X {}\n\nfn (x X) m() int {\n\treturn 1\n}\n\nfn helper() int {\n\treturn X{}.m()\n}\n\nfn main() {}\n')
	mut used := markused.mark_used(a, tc)
	assert !used['helper']
	assert !used['X.m']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('helper(')
	assert !c_code.contains('X__m(')
}

fn test_reachable_main_fn_literal_is_emitted_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('reachable_main_fn_literal_cgen',
		'module main\n\nfn callback_value(cb fn () int) int {\n\treturn cb()\n}\n\nfn main() {\n\t_ := callback_value(fn () int {\n\t\treturn 7\n\t})\n}\n')
	mut used := markused.mark_used(a, tc)
	assert used['callback_value']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('int __anon_fn_')
	assert c_code.contains('callback_value(__anon_fn_')
}

fn test_reachable_imported_fn_literal_roots_private_callback_helpers() {
	mut a, mut tc := parse_checked_project('reachable_imported_fn_literal_helpers', {
		'printer/printer.v': 'module printer\n\nfn helper() int {\n\treturn 41\n}\n\nfn named(name string) int {\n\treturn name.len\n}\n\nfn consume(cb fn () int, name_to_index fn (string) int) int {\n\treturn cb() + name_to_index("x")\n}\n\npub fn run() int {\n\treturn consume(fn () int {\n\t\treturn helper()\n\t}, named)\n}\n'
		'main.v':            'module main\n\nimport printer\n\nfn main() {\n\t_ := printer.run()\n}\n'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('printer__helper('), c_code
	assert c_code.contains('printer__named('), c_code
	assert c_code.contains('printer__consume(printer____anon_fn_'), c_code
}

fn test_top_level_fn_value_roots_helper() {
	mut a, mut tc := parse_checked_source('top_level_fn_value_helper_cgen',
		'module main\n\nfn helper() int {\n\treturn 41\n}\n\nf := helper\nprintln(int_str(f() + 1))\n')
	mut used := markused.mark_used(a, tc)
	assert used['helper']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('helper(')
}

fn test_top_level_fn_value_compile_keeps_helper() {
	v3_bin := build_v3_bin('top_level_fn_value_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_top_level_fn_value_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_top_level_fn_value_input')
	os.write_file(src, '
fn helper() int {
	return 41
}

f := helper
println(int_str(f() + 1))
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('helper('), c_code
}

fn test_same_module_unqualified_homonym_call_uses_qualified_key() {
	a, tc := parse_checked_project('same_module_unqualified_homonym_call', {
		'main.v': 'module main

import a

fn main() {
	println(a.glob_match(false))
}
'
		'a/a.v':  'module a

fn helper() string {
	return "a"
}

pub fn glob_match(flag bool) string {
	if !flag {
		return helper()
	}
	return "other"
}
'
		'b/b.v':  'module b

fn helper() string {
	return "b"
}

pub fn glob_match(flag bool) string {
	if !flag {
		return helper()
	}
	return "other"
}
'
	}, 'main.v')
	used := markused.mark_used(a, tc)
	assert used['a.glob_match']
	assert used['a.helper']
	assert !used['b.glob_match']
	assert !used['b.helper']
}

fn test_top_level_fn_value_respects_prior_local_shadow() {
	used := mark_used_source('top_level_fn_value_prior_shadow', '
fn helper() int {
	return 1
}

helper := 10
f := helper
println(int_str(f))
')
	assert !used['helper']
}

fn test_local_fn_value_keeps_helper_before_future_local_shadow() {
	used := mark_used_source('local_fn_value_future_shadow', '
fn cb() int {
	return 1
}

fn takes(f fn () int) int {
	return f()
}

fn main() {
	takes(cb)
	cb := 0
	_ = cb
}
')
	assert used['cb']
}

fn test_fn_value_call_callee_call_roots_inner_factory() {
	used := mark_used_source('fn_value_call_callee_call', '
fn cb() int {
	return 7
}

fn make_cb() fn () int {
	return cb
}

fn main() {
	_ := make_cb()()
}
')
	assert used['make_cb']
	assert used['cb']
}

fn test_local_ident_reference_does_not_root_dead_function() {
	mut a, mut tc := parse_checked_source('local_ident_shadow_dead_fn_cgen', '
fn C.v3_dead_local_shadow_should_not_link() int

fn unused() int {
	return C.v3_dead_local_shadow_should_not_link()
}

fn echo(unused int) int {
	println(unused)
	return unused
}

fn main() {
	unused := 1
	println(unused)
	_ := echo(unused)
}
')
	mut used := markused.mark_used(a, tc)
	assert !used['unused']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('unused('), c_code
	assert !c_code.contains('v3_dead_local_shadow_should_not_link'), c_code
}

fn test_local_fn_value_call_does_not_root_shadowed_dead_function() {
	mut a, mut tc := parse_checked_source('local_fn_value_shadow_dead_fn_cgen', '
fn C.v3_dead_local_fn_value_shadow_should_not_link() int

fn unused() int {
	return C.v3_dead_local_fn_value_shadow_should_not_link()
}

fn used() int {
	return 7
}

fn main() {
	unused := used
	println(unused())
}
')
	mut used := markused.mark_used(a, tc)
	assert used['used']
	assert !used['unused']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('used('), c_code
	assert !c_code.contains('int unused(void)'), c_code
	assert !c_code.contains('v3_dead_local_fn_value_shadow_should_not_link'), c_code
}

fn test_flag_default_value_lowering_keeps_escape_helper() {
	mut a, mut tc := parse_checked_source('flag_default_value_escape_helper_cgen',
		'module main\n\nfn escape_default_string(value string) string {\n\treturn value\n}\n\nfn flag_default_value(value string) string {\n\treturn value\n}\n\nfn main() {\n\t_ := flag_default_value("abc")\n}\n')
	mut used := markused.mark_used(a, tc)
	assert used['escape_default_string']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('escape_default_string(')
}

// test_return_local_address_seeds_memdup_runtime_helper validates this v3 regression case.
fn test_return_local_address_seeds_memdup_runtime_helper() {
	used := mark_used_source('return_local_address_memdup', '
struct Box {
	x int
}

fn make_box() &Box {
	b := Box{
		x: 1
	}
	return &b
}

fn main() {
	_ := make_box()
}
')
	assert used['memdup']
}

// test_map_literals_lower_to_new_map_after_used_filter_transform validates this v3 regression case.
fn test_map_literals_lower_to_new_map_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('map_literal_new_map_cgen', '
fn make_map() map[string]int {
	return map[string]int{}
}

fn main() {
	_ := make_map()
}
')
	mut used := markused.mark_used(a, tc)
	assert used['new_map']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('new_map(sizeof(string), sizeof(int)')
}

// test_optional_map_or_lowers_to_new_map_after_used_filter_transform
// validates this v3 regression case.
fn test_optional_map_or_lowers_to_new_map_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('option_map_or_new_map_cgen', '
fn maybe_map() ?map[string]int {
	return none
}

fn main() {
	m := maybe_map() or { return }
	_ := m
}
')
	mut used := markused.mark_used(a, tc)
	assert used['new_map']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('new_map(sizeof(string), sizeof(int)')
}

// test_string_membership_lowers_to_contains_after_used_filter_transform
// validates this v3 regression case.
fn test_string_membership_lowers_to_contains_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('string_membership_contains_cgen', '
fn has_needle() bool {
	return "bc" in "abcd"
}

fn main() {
	_ := has_needle()
}
')
	mut used := markused.mark_used(a, tc)
	assert used['string__contains']
	assert used['string__contains_u8']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('string__contains(')
}

// test_string_compound_assign_lowers_to_plus_after_used_filter_transform
// validates this v3 regression case.
fn test_string_compound_assign_lowers_to_plus_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('string_plus_assign_cgen', '
fn main() {
	mut s := "a"
	s += "b"
	_ := s
}
')
	mut used := markused.mark_used(a, tc)
	assert used['string__plus']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('string__plus(')
}

// test_string_interpolation_lowers_to_formatter_after_used_filter_transform
// validates this v3 regression case.
fn test_string_interpolation_lowers_to_formatter_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('string_interp_formatter_cgen', '
fn main() {
	_ := "\${true}"
}
')
	mut used := markused.mark_used(a, tc)
	assert used['bool.str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('bool__str(')
}

// test_f32_interpolation_lowers_to_formatter_after_used_filter_transform
// validates this v3 regression case.
fn test_f32_interpolation_lowers_to_formatter_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('f32_interp_formatter_cgen', '
fn main() {
	value := f32(1.25)
	_ := "\${value}"
}
')
	mut used := markused.mark_used(a, tc)
	assert used['f32.str']
	assert used['f32__str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('f32__str(')
}

// test_print_bool_lowers_to_formatter_after_used_filter_transform
// validates this v3 regression case.
fn test_print_bool_lowers_to_formatter_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('print_bool_formatter_cgen', '
fn println(s string) {}

fn main() {
	println(true)
}
')
	mut used := markused.mark_used(a, tc)
	assert used['bool.str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('bool__str(')
}

// test_return_local_address_lowers_to_memdup_after_used_filter_transform
// validates this v3 regression case.
fn test_return_local_address_lowers_to_memdup_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('return_local_address_memdup_cgen', '
struct Box {
	x int
}

fn make_box() &Box {
	b := Box{
		x: 1
	}
	return &b
}

fn main() {
	_ := make_box()
}
')
	mut used := markused.mark_used(a, tc)
	assert used['memdup']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('memdup(')
}

// test_map_literal_compile_keeps_new_map_runtime_helper validates this v3 regression case.
fn test_map_literal_compile_keeps_new_map_runtime_helper() {
	v3_bin := build_v3_bin('map_literal_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_map_literal_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_map_literal_input')
	os.write_file(src, '
fn make_map() map[string]int {
	return map[string]int{}
}

fn main() {
	_ := make_map()
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_optional_map_or_compile_keeps_new_map_runtime_helper validates this v3 regression case.
fn test_optional_map_or_compile_keeps_new_map_runtime_helper() {
	v3_bin := build_v3_bin('option_map_or_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_option_map_or_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_option_map_or_input')
	os.write_file(src, '
fn maybe_map() ?map[string]int {
	return none
}

fn main() {
	m := maybe_map() or { return }
	_ := m
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_return_local_address_compile_keeps_memdup_runtime_helper validates this v3 regression case.
fn test_return_local_address_compile_keeps_memdup_runtime_helper() {
	v3_bin := build_v3_bin('return_local_address_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_return_local_address_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_return_local_address_input')
	os.write_file(src, '
struct Box {
	x int
}

fn make_box() &Box {
	b := Box{
		x: 1
	}
	return &b
}

fn main() {
	_ := make_box()
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_print_bool_compile_keeps_formatter_runtime_helper validates this v3 regression case.
fn test_print_bool_compile_keeps_formatter_runtime_helper() {
	v3_bin := build_v3_bin('print_bool_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_print_bool_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_print_bool_input')
	os.write_file(src, '
fn main() {
	println(true)
}
') or { panic(err) }
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_print_signed_width_compile_keeps_str_runtime_helpers validates this v3 regression case.
fn test_print_signed_width_compile_keeps_str_runtime_helpers() {
	v3_bin := build_v3_bin('print_signed_width_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_print_signed_width_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_print_signed_width_input')
	os.write_file(src, "
fn main() {
	println(i8(-5))
	println(i16(-300))
	println(i32(-70000))
	println(i64(-5000000000))
	println('\${i64(42)}')
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '-5\n-300\n-70000\n-5000000000\n42', run.output
}

// test_string_plus_compile_keeps_plus_runtime_helper validates this v3 regression case.
fn test_string_plus_compile_keeps_plus_runtime_helper() {
	v3_bin := build_v3_bin('string_plus_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_string_plus_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_string_plus_input')
	os.write_file(src, '
fn main() {
	flag := true
	mut s := "a"
	s += "\${flag}"
	_ := s
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_string_membership_compile_keeps_contains_runtime_helpers validates this v3 regression case.
fn test_string_membership_compile_keeps_contains_runtime_helpers() {
	v3_bin := build_v3_bin('string_membership_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_string_membership_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_string_membership_input')
	os.write_file(src, '
fn has_needle() bool {
	return "bc" in "abcd"
}

fn main() {
	_ := has_needle()
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}
