// vtest ownership
import os
import v.cmdexec
import v.tempname

const array_accessor_borrow_vexe = @VEXE
const array_accessor_borrow_tests_dir = os.dir(@FILE)
const array_accessor_borrow_v3_dir = os.dir(array_accessor_borrow_tests_dir)
const array_accessor_borrow_vlib_dir = os.dir(array_accessor_borrow_v3_dir)
const array_accessor_borrow_v3_src = os.join_path(array_accessor_borrow_v3_dir, 'v.v')

const array_accessor_borrow_tmp_dir = os.join_path(os.vtmp_dir(), 'v3_array_accessor_borrow_${tempname.unique_token()}')

fn testsuite_begin() {
	os.mkdir(array_accessor_borrow_tmp_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(array_accessor_borrow_tmp_dir) or {}
}

fn tmp_array_accessor_borrow_path(name string) string {
	return os.join_path(array_accessor_borrow_tmp_dir, 'v3_${name}')
}

fn tmp_array_accessor_borrow_executable_path(name string) string {
	return tmp_array_accessor_borrow_path(name) + $if windows { '.exe' } $else { '' }
}

fn assert_clean_borrow_build(compile os.Result) {
	assert compile.exit_code == 0, compile.output
	// The regressed transform lowered `arr.last().field` to an empty placeholder that
	// the C backend printed as `(0)`, so the borrow read reached cc as `(0).field`
	// ("member reference base type 'int' is not a structure or union"). In a directly
	// compiled module the ownership checker instead rejected the accessor outright.
	assert !compile.output.contains('unsupported node kind'), compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn run_v3_array_accessor_borrow_module(name string, module_src string, main_src string) string {
	proj_dir := tmp_array_accessor_borrow_path(name)
	mod_dir := os.join_path(proj_dir, 'mymod')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'mymod.v'), module_src) or { panic(err) }
	os.write_file(os.join_path(proj_dir, 'main.v'), main_src) or { panic(err) }
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	compile := cmdexec.run_in_merged_with_env(array_accessor_borrow_vexe, [proj_dir, '-b', 'c',
		'-o', bin_path], '', {
		'VFLAGS': ''
	})
	assert_clean_borrow_build(compile)
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// Reading a field of `arr.first()`/`arr.last()` is a borrow of the stored element,
// not an independent copy, so it must lower to an in-place `arr[..]` access even when
// the element type owns heap data and has no `clone()` method. This mirrors the HPACK
// dynamic table (`net.http` / `net.quic`) that `v new --web` pulls in via `veb`.
fn test_first_last_field_borrow_in_main_and_imported_module() {
	output := run_v3_array_accessor_borrow_module('field_borrows', 'module mymod\n\nstruct Entry {\n\tname  string\n\tvalue string\n\tsize  int\n}\n\npub struct Tbl {\nmut:\n\tentries []Entry\n\ttotal   int\n}\n\npub fn (mut t Tbl) add(name string, value string) {\n\tsz := name.len + value.len + 32\n\tfor t.entries.len > 0 && t.total + sz > 100 {\n\t\tt.total -= t.entries.last().size\n\t\tt.entries.delete_last()\n\t}\n\tt.entries.insert(0, Entry{ name: name, value: value, size: sz })\n\tt.total += sz\n}\n\npub fn (t &Tbl) total() int {\n\treturn t.total\n}\n', "module main

import mymod

struct Entry {
	name  string
	value string
	size  int
}

struct Tbl {
mut:
	entries []Entry
	total   int
}

fn (mut t Tbl) shrink() {
	// Read the last element before evicting it.
	for t.entries.len > 0 {
		t.total -= t.entries.last().size
		t.entries.delete_last()
	}
}

struct ProbeEntry {
	name string
	n    int
}

struct Box {
mut:
	entries []ProbeEntry
}

fn (b &Box) probe() int {
	// Include a chained field read through a reference receiver.
	return b.entries.first().n + b.entries.last().n + b.entries.last().name.len
}

fn check_main_module_borrows() {
	mut t := Tbl{}
	t.entries << Entry{ name: 'a', value: 'b', size: 5 }
	t.entries << Entry{ name: 'c', value: 'd', size: 7 }
	t.total = 12
	t.shrink()
	println(int_str(t.total))

	mut b := Box{}
	b.entries << ProbeEntry{ name: 'ab', n: 10 }
	b.entries << ProbeEntry{ name: 'cdef', n: 20 }
	println(int_str(b.probe()))
}

// The same borrow read inside an imported module: the ownership checker does not
// diagnose non-user modules, so the regressed transform silently produced `(0)` there,
// which is precisely why `v .` on a fresh `--web` project failed to compile.
fn check_imported_module_borrow() {
	mut t := mymod.Tbl{}
	t.add('a', 'b')
	t.add('c', 'd')
	println(int_str(t.total()))
}

fn main() {
	check_main_module_borrows()
	check_imported_module_borrow()
}
")
	assert output.split_into_lines() == ['0', '34', '68'], output
}

// The in-place borrow only fires with the ownership checker (`-d ownership`); it is where
// the accessor would otherwise take its independent-clone path. These cases exercise that
// path directly. The build compiler must itself embed the ownership checker.
fn build_v3_array_accessor_borrow_ownership() ?string {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(array_accessor_borrow_v3_src) {
		return os.real_path(shared)
	}
	v3_bin := tmp_array_accessor_borrow_executable_path('array_accessor_borrow_ownership')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build :=
		os.exec([array_accessor_borrow_vexe, '-gc', 'none', '-d', 'ownership', '-path',
			'${array_accessor_borrow_vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			array_accessor_borrow_v3_src])
	if build.exit_code != 0 {
		os.rm(v3_bin) or {}
	}
	if build.output.contains('ownership support is not compiled into this v3 executable') {
		// The bootstrap compiler running this test lacks the ownership checker, so it
		// cannot build an ownership-enabled v. Skip rather than fail on such a host.
		return none
	}
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn compile_v3_ownership_program(v3_bin string, name string, src string, extra_args string) os.Result {
	src_path := '${tmp_array_accessor_borrow_path(name)}.v'
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	return os.exec([v3_bin, '-ownership', '-d', 'ownership', '-no-parallel',
		...(os.split_args(extra_args) or { panic(err) }), '-o', bin_path, src_path])
}

fn compile_v3_uncloneable_array_ownership_program(v3_bin string, name string, src string, extra_args string) os.Result {
	e_decl := 'struct E {'
	assert src.count(e_decl) == 1
	uncloneable_src := 'interface Drop {\nmut:\n\tdrop()\n}\n\n' + src.replace_once(e_decl, 'struct E implements Drop {') + '\nfn (mut value E) drop() {}\n'
	return compile_v3_ownership_program(v3_bin, name, uncloneable_src, extra_args)
}

// These consumers can borrow the element for the string field read.
const array_accessor_borrow_string_consumers_src = "struct T {
mut:
	entries []E
}

// The string clone can outlive the borrowed element.
fn (t &T) last_name() string {
	return t.entries.last().name.clone()
}

struct StringSizedTable {
mut:
	entries []SizedEntry
}

// Only the length leaves this field chain.
fn (t &StringSizedTable) last_name_len() int {
	return t.entries.last().name.len
}

fn (t &T) has_name(name string) bool {
	return t.entries.last().name == name
}

fn has_last_name(entries []E, names []string) bool {
	return entries.last().name in names
}

fn check_clone() {
	mut t := T{}
	t.entries << E{ name: 'hello' }
	name := t.last_name()
	t.entries.delete_last()
	println(name)
}

fn check_length() {
	mut t := StringSizedTable{}
	t.entries << SizedEntry{ name: 'hello', size: 5 }
	println(int_str(t.last_name_len()))
}

fn check_comparison() {
	mut t := T{}
	t.entries << E{ name: 'hello' }
	println(t.has_name('hello'))
}

fn check_membership() {
	entries := [E{ name: 'hello' }]
	println(has_last_name(entries, ['hello']))
}

// Concatenation copies the borrowed string into the result.
fn decorated_last_name() string {
	arr := [E{ name: 'hello' }]
	return arr.last().name + '!'
}

// A multi-part interpolation also copies the borrowed string.
fn interpolated_last_name() string {
	arr := [E{ name: 'hello' }]
	return '<\${arr.last().name}>'
}

// Padding may return its input when the requested width is already met.
fn formatted_last_name() string {
	arr := [E{ name: 'hello' }]
	return '\${arr.last().name:3s}'
}

// The formatted string can also appear next to a formatted integer.
fn formatted_last_name_with_sibling() string {
	arr := [E{ name: 'hello' }]
	return '\${arr.last().name:3s}\${1:3d}'
}

fn has_known_last_name(entries []E) bool {
	return entries.last().name in ['hello', 'other']
}

fn check_allocating_consumers() {
	println(decorated_last_name())
	println(interpolated_last_name())
	println(formatted_last_name())
	println(formatted_last_name_with_sibling())
}

fn check_literal_membership() {
	entries := [E{ name: 'hello' }]
	println(has_known_last_name(entries))
}

// A plain `string` field uses the builtin stringifier, so the borrow stays safe.
fn last_label(entries []E) string {
	return '\${entries.last().name}\${0}'
}

fn last_name_matches(entries []E) bool {
	return entries.last().name == ('he' + 'llo')
}

fn check_plain_interpolation() {
	entries := [E{ name: 'hello' }]
	println(last_label(entries))
}

fn check_builtin_comparison() {
	entries := [E{ name: 'hello' }]
	println(last_name_matches(entries))
}

struct Stable {
	n int
}

fn last_name_then_stringify(entries []E, stable Stable) string {
	return '\${entries.last().name}\${stable}'
}

fn check_auto_stringifier() {
	entries := [E{ name: 'hello' }]
	println(last_name_then_stringify(entries, Stable{ n: 1 }))
}

fn check_string_consumers() {
	_ = check_auto_stringifier // Compile the control without running it.
	check_clone()
	check_length()
	check_comparison()
	check_membership()
	check_allocating_consumers()
	check_literal_membership()
	check_plain_interpolation()
	check_builtin_comparison()
}
"

// A bound method value needs a copy of its receiver. Returning a string,
// a pointer, or a nested pointer would let the borrow outlive the array.
// The nested pointer case uses an aggregate with no destructor.
fn test_owned_array_element_escapes_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_array_element_escapes', "struct E {
	name string
	size int
}

fn (e E) describe() string {
	return e.name
}

struct T {
mut:
	entries []E
}

fn (t &T) last_name() string {
	return t.entries.last().name
}

struct PointerEntry implements Drop {
	name string
}

fn last_name_ptr() &u8 {
	arr := [PointerEntry{ name: 'hello' }]
	return arr.last().name.str
}

fn (mut value PointerEntry) drop() {}

struct View {
	ptr &u8
}

struct Wrapper {
	view View
}

struct WrapperEntry implements Drop {
	name    string
	wrapper Wrapper
}

fn last_wrapper() Wrapper {
	arr := [WrapperEntry{
		name: 'hello'
		wrapper: Wrapper{
			view: View{
				ptr: unsafe { nil }
			}
		}
	}]
	return arr.last().wrapper
}

fn (mut value WrapperEntry) drop() {}

fn check_method_value() {
	arr := [E{ name: 'a', size: 1 }, E{ name: 'b', size: 2 }]
	f := arr.last().describe
	println(f())
}

fn check_string_escape() {
	mut t := T{}
	t.entries << E{ name: 'hello', size: 5 }
	println(t.last_name())
}

fn check_pointer_escape() {
	println(unsafe { cstring_to_vstring(last_name_ptr()) })
}

fn check_nested_pointer_escape() {
	_ = last_wrapper()
}

fn main() {
	check_method_value()
	check_string_escape()
	check_pointer_escape()
	check_nested_pointer_escape()
}
", '-nocolor')
	// Locations include the prepended Drop interface.
	assert ownership_error_lines(compile) == [
		independent_array_element_error_at('21:9', 'E'),
		independent_array_element_error_at('30:9', 'PointerEntry'),
		independent_array_element_error_at('57:9', 'WrapperEntry'),
		independent_array_element_error_at('64:7', 'E'),
	], compile.output
}

fn ownership_error_lines(compile os.Result) []string {
	assert compile.exit_code == 1, compile.output
	return compile.output.split_into_lines().filter(it.contains(': error: ')).map(it.all_after('.v:'))
}

fn independent_array_element_error_at(location string, element string) string {
	return '${location}: error: cannot return an independent array element: `${element}` requires ownership destruction but has no `clone()` method'
}

fn assert_independent_array_element_errors(compile os.Result, locations []string) {
	assert ownership_error_lines(compile) == locations.map(independent_array_element_error_at(it, 'E')), compile.output
}

fn test_owned_string_consumers_reject_mutating_overloads() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_string_consumers_with_mutating_overloads', "struct E {
	name string
}

struct Mutator {}

__global entries []E

fn (Mutator) str() string {
	entries.delete_last()
	return '!'
}

fn last_name_then_stringify(mutator Mutator) string {
	return '\${entries.last().name}\${mutator}'
}

struct Wrapper {
	mutator Mutator
}

fn last_name_then_stringify_wrapper(wrapper Wrapper) string {
	return '\${entries.last().name}\${wrapper}'
}

fn check_stringifier() {
	entries = [E{ name: 'hello' }]
	println(last_name_then_stringify(Mutator{}))
}

fn check_nested_stringifier() {
	entries = [E{ name: 'hello' }]
	println(last_name_then_stringify_wrapper(Wrapper{}))
}

type Name = string

struct AliasEntry implements Drop {
	name Name
}

__global alias_entries []AliasEntry

fn (name Name) str() string {
	alias_entries.delete_last()
	return string(name)
}

// The borrowed interpolation part is itself a string alias with a custom `str()`. The
// interpolation dispatches through that method (not the builtin stringifier), and it mutates
// the source array before the borrowed shallow receiver is read, so the borrow is unsafe.
fn last_label() string {
	return '\${alias_entries.last().name}\${0}'
}

fn (mut value AliasEntry) drop() {}

fn check_alias_stringifier() {
	alias_entries = [AliasEntry{ name: Name('hello') }]
	println(last_label())
}

fn (left Mutator) + (right Mutator) string {
	_ = left
	_ = right
	entries.delete_last()
	return 'hello'
}

fn last_name_matches_with_infix(left Mutator, right Mutator) bool {
	return entries.last().name == (left + right)
}

fn (Mutator) [] (index int) string {
	_ = index
	entries.delete_last()
	return 'hello'
}

fn last_name_matches_with_index(mutator Mutator) bool {
	return entries.last().name == mutator[0]
}

fn check_infix() {
	entries = [E{ name: 'hello' }]
	println(last_name_matches_with_infix(Mutator{}, Mutator{}))
}

fn check_index() {
	entries = [E{ name: 'hello' }]
	println(last_name_matches_with_index(Mutator{}))
}

fn main() {
	check_stringifier()
	check_nested_stringifier()
	check_alias_stringifier()
	check_infix()
	check_index()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert ownership_error_lines(compile) == [
		independent_array_element_error_at('20:12', 'E'),
		independent_array_element_error_at('28:12', 'E'),
		independent_array_element_error_at('58:12', 'AliasEntry'),
		independent_array_element_error_at('76:9', 'E'),
		independent_array_element_error_at('86:9', 'E'),
	], compile.output
}

fn test_owned_borrows_with_explicit_mutating_calls_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	src := "struct E {
	name string
}

fn delete_last_for_index(mut arr []E) int {
	arr.delete_last()
	return 0
}

fn last_initial_then_delete(mut arr []E) u8 {
	return arr.last().name[delete_last_for_index(mut arr)]
}

struct Pair {
	n int
	s string
}

fn delete_last_for_field(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn last_length_in_pair(mut arr []E) Pair {
	return Pair{ n: arr.last().name.len, s: delete_last_for_field(mut arr) }
}

fn delete_last_for_return(mut entries []E) string {
	entries.delete_last()
	return 'deleted'
}

fn last_length_in_multi_return(mut entries []E) (int, string) {
	return entries.last().name.len, delete_last_for_return(mut entries)
}

fn check_index() {
	mut arr := [E{ name: 'hello' }]
	println(last_initial_then_delete(mut arr))
}

fn check_field() {
	mut arr := [E{ name: 'hello' }]
	println(last_length_in_pair(mut arr).n)
}

fn check_return() {
	mut entries := [E{ name: 'hello' }]
	length, action := last_length_in_multi_return(mut entries)
	println(int_str(length) + ':' + action)
}

fn delete_last_for_condition(mut entries []E) bool {
	entries.delete_last()
	return true
}

fn if_length(mut entries []E) int {
	return if delete_last_for_condition(mut entries) { entries.last().name.len } else { 0 }
}

fn match_subject_length(mut entries []E) int {
	return match delete_last_for_condition(mut entries) {
		true { entries.last().name.len }
		else { 0 }
	}
}

fn delete_last_or_error(mut entries []E) !int {
	entries.delete_last()
	return error('deleted')
}

fn or_length(mut entries []E) int {
	return delete_last_or_error(mut entries) or { entries.last().name.len }
}

fn delete_last_and_return_one(mut entries []E) int {
	entries.delete_last()
	return 1
}

fn match_branch_length(mut entries []E) int {
	return match 1 {
		delete_last_and_return_one(mut entries) { entries.last().name.len }
		else { 0 }
	}
}

fn check_if() {
	mut entries := [E{ name: 'hello' }]
	println(if_length(mut entries))
}

fn check_match_subject() {
	mut entries := [E{ name: 'hello' }]
	println(match_subject_length(mut entries))
}

fn check_or() {
	mut entries := [E{ name: 'hello' }]
	println(or_length(mut entries))
}

fn check_match_branch() {
	mut entries := [E{ name: 'hello' }]
	println(match_branch_length(mut entries))
}

fn main() {
	check_index()
	check_field()
	check_return()
	check_if()
	check_match_subject()
	check_or()
	check_match_branch()
	check_mutating_siblings()
}

" + array_accessor_borrow_mutating_siblings_src
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_borrows_with_mutating_calls', src, '-nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['16:9', '30:18', '39:9', '64:53', '69:10',
		'80:48', '90:45', '148:10', '152:24', '156:28', '160:22', '164:24', '168:21', '172:31',
		'176:23', '180:33', '234:9', '238:12', '247:9'])
}

fn test_owned_field_overloaded_comparison_and_index_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_field_overloaded_operators', "struct Name {
	text string
}

struct E {
	name Name
}

fn (left Name) == (right Name) bool {
	return left.text == right.text
}

fn last_name_matches(arr []E) bool {
	return arr.last().name == Name{ text: 'hello' }
}

fn (name Name) [] (index int) u8 {
	return name.text[index]
}

fn last_initial(arr []E) u8 {
	return arr.last().name[0]
}

fn check_comparison() {
	arr := [E{ name: Name{ text: 'hello' } }]
	println(last_name_matches(arr))
}

fn check_index() {
	arr := [E{ name: Name{ text: 'hello' } }]
	println(last_initial(arr))
}

fn main() {
	check_comparison()
	check_index()
}
", '-nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['19:9', '27:9'])
}

fn test_owned_comparison_and_membership_reject_mutating_equality() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_mutating_equality_overloads', "struct Name {
	text string
}

struct E {
	names []Name
}

__global entries []E

fn (left Name) == (right Name) bool {
	entries.delete_last()
	return left.text == right.text
}

fn last_names_match(other []Name) bool {
	return entries.last().names == other
}

fn last_names_differ(other []Name) bool {
	return entries.last().names != other
}

interface Named {
	name() string
}

struct InterfaceName {
	text string
}

struct InterfaceEntry implements Drop {
	value Named
}

__global interface_entries []InterfaceEntry

fn (name InterfaceName) name() string {
	return name.text
}

fn (left InterfaceName) == (right InterfaceName) bool {
	interface_entries.delete_last()
	return left.text == right.text
}

fn last_matches(other Named) bool {
	return interface_entries.last().value == other
}

fn (mut value InterfaceEntry) drop() {}

fn check_aggregate_equality() {
	entries = [E{ names: [Name{ text: 'hello' }] }]
	println(last_names_match([Name{ text: 'hello' }]))
}

fn check_aggregate_inequality() {
	entries = [E{ names: [Name{ text: 'hello' }] }]
	println(last_names_differ([Name{ text: 'world' }]))
}

fn check_interface_equality() {
	interface_entries = [InterfaceEntry{ value: Named(InterfaceName{ text: 'hello' }) }]
	println(last_matches(Named(InterfaceName{ text: 'hello' })))
}

struct Needle {
	text string
}

struct NeedleEntry implements Drop {
	value Needle
}

__global needle_entries []NeedleEntry

fn (left Needle) == (right Needle) bool {
	needle_entries.delete_last()
	return left.text == right.text
}

fn last_value_is_in(haystack []Needle) bool {
	return needle_entries.last().value in haystack
}

struct MembershipEntry implements Drop {
	name string
}

struct MembershipNeedle {
	value int
}

__global membership_entries []MembershipEntry

fn (left MembershipNeedle) == (right MembershipNeedle) bool {
	membership_entries.delete_last()
	return left.value == right.value
}

fn consume(length int, found bool) int {
	return if found { length } else { 0 }
}

fn last_length_with_membership(needle MembershipNeedle, haystack []MembershipNeedle) int {
	return consume(membership_entries.last().name.len, needle in haystack)
}

fn (mut value NeedleEntry) drop() {}

fn (mut value MembershipEntry) drop() {}

fn check_direct_membership() {
	needle_entries = [NeedleEntry{ value: Needle{ text: 'hello' } }]
	println(last_value_is_in([Needle{ text: 'hello' }]))
}

fn check_membership_sibling() {
	membership_entries = [MembershipEntry{ name: 'hello' }]
	println(last_length_with_membership(MembershipNeedle{ value: 1 }, [MembershipNeedle{ value: 1 }]))
}

fn main() {
	check_aggregate_equality()
	check_aggregate_inequality()
	check_interface_equality()
	check_direct_membership()
	check_membership_sibling()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert ownership_error_lines(compile) == [
		independent_array_element_error_at('22:9', 'E'),
		independent_array_element_error_at('26:9', 'E'),
		independent_array_element_error_at('53:9', 'InterfaceEntry'),
		independent_array_element_error_at('89:9', 'NeedleEntry'),
		independent_array_element_error_at('112:17', 'MembershipEntry'),
	], compile.output
}

// An allocating consumer is borrow-safe only when evaluating its other operands cannot mutate
// the source array first. Keep the accessor on its independent-copy path when a sibling call can
// delete the selected element before string concatenation consumes the field.
const array_accessor_borrow_mutating_siblings_src = "fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume_string(value string, suffix string) string {
	return value + suffix
}

fn consume_bool(value bool, suffix string) bool {
	return value
}

fn consume_int(value int, suffix string) int {
	return value
}

fn identity(value string) string {
	return value
}

fn nested_concat_then_delete(mut arr []E) string {
	return (arr.last().name + '!') + delete_last(mut arr)
}

fn concat_arg_then_delete(mut arr []E) string {
	return consume_string(arr.last().name + '!', delete_last(mut arr))
}

fn interpolation_arg_then_delete(mut arr []E) string {
	return consume_string('<\${arr.last().name}>', delete_last(mut arr))
}

fn comparison_arg_then_delete(mut arr []E) bool {
	return consume_bool(arr.last().name == 'hello', delete_last(mut arr))
}

fn negated_comparison_arg_then_delete(mut arr []E) bool {
	return consume_bool(!(arr.last().name == 'hello'), delete_last(mut arr))
}

fn length_arg_then_delete(mut arr []E) int {
	return consume_int(arr.last().name.len, delete_last(mut arr))
}

fn conditional_arg_then_delete(mut arr []E, flag bool) int {
	return consume_int(if flag { arr.last().name.len } else { 0 }, delete_last(mut arr))
}

fn logical_arg_then_delete(mut arr []E) bool {
	return consume_bool((arr.last().name == 'hello') && true, delete_last(mut arr))
}

fn nested_arg_then_delete(mut arr []E) string {
	return consume_string(identity(arr.last().name + '!'), delete_last(mut arr))
}

fn check_nested_concat() {
	mut arr := [E{ name: 'hello' }]
	println(nested_concat_then_delete(mut arr))
}

fn check_concat_arg() {
	mut arr := [E{ name: 'hello' }]
	println(concat_arg_then_delete(mut arr))
}

fn check_interpolation_arg() {
	mut arr := [E{ name: 'hello' }]
	println(interpolation_arg_then_delete(mut arr))
}

fn check_comparison_arg() {
	mut arr := [E{ name: 'hello' }]
	println(comparison_arg_then_delete(mut arr))
}

fn check_negated_comparison_arg() {
	mut arr := [E{ name: 'hello' }]
	println(negated_comparison_arg_then_delete(mut arr))
}

fn check_length_arg() {
	mut arr := [E{ name: 'hello' }]
	println(length_arg_then_delete(mut arr))
}

fn check_conditional_arg() {
	mut arr := [E{ name: 'hello' }]
	println(conditional_arg_then_delete(mut arr, true))
}

fn check_logical_arg() {
	mut arr := [E{ name: 'hello' }]
	println(logical_arg_then_delete(mut arr))
}

fn check_nested_arg() {
	mut arr := [E{ name: 'hello' }]
	println(nested_arg_then_delete(mut arr))
}

fn delete_last_for_string(mut arr []E) string {
	arr.delete_last()
	return '!'
}

fn last_name_then_delete(mut arr []E) string {
	return arr.last().name + delete_last_for_string(mut arr)
}

fn interpolate_then_delete(mut arr []E) string {
	return '\${arr.last().name}\${delete_last_for_string(mut arr)}'
}

fn delete_for_comparison(mut arr []E) string {
	arr.delete_last()
	return 'hello'
}

fn compare_then_delete(mut arr []E) bool {
	return arr.last().name == delete_for_comparison(mut arr)
}

fn check_concat() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}

fn check_interpolation() {
	mut arr := [E{ name: 'hello' }]
	println(interpolate_then_delete(mut arr))
}

fn check_comparison() {
	mut arr := [E{ name: 'hello' }]
	println(compare_then_delete(mut arr))
}

fn check_mutating_siblings() {
	check_nested_concat()
	check_concat_arg()
	check_interpolation_arg()
	check_comparison_arg()
	check_negated_comparison_arg()
	check_length_arg()
	check_conditional_arg()
	check_logical_arg()
	check_nested_arg()
	check_concat()
	check_interpolation()
	check_comparison()
}
"

// Indexing and chained length reads can borrow the original element. A length
// read can also share an initializer or call with stable fields or arguments.
fn test_owned_array_accessor_consumers_run() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	src := array_accessor_borrow_string_consumers_src + "
struct E {
	name string
}

fn last_initial() u8 {
	arr := [E{ name: 'hello' }]
	return arr.last().name[0]
}

struct Pair {
	n int
	s string
}

fn last_length(arr []E) Pair {
	return Pair{ n: arr.last().name.len, s: 'ok' }
}

struct ArgumentPair {
	n int
}

fn consume_struct(length int, pair ArgumentPair) int {
	_ = pair
	return length
}

fn consume_map(length int, values map[string]int) int {
	_ = values
	return length
}

fn consume_array(length int, values []int) int {
	_ = values
	return length
}

fn last_struct_length(entries []E) int {
	return consume_struct(entries.last().name.len, ArgumentPair{ n: 1 })
}

fn last_map_length(entries []E) int {
	return consume_map(entries.last().name.len, { 'n': 1 })
}

fn last_array_length(entries []E) int {
	return consume_array(entries.last().name.len, []int{len: 1, init: 1})
}

fn check_scalar_index() {
	println(int_str(last_initial()))
}

fn check_aggregate_field() {
	arr := [E{ name: 'hello' }]
	println(last_length(arr).n)
}

fn check_aggregate_arguments() {
	entries1 := [E{ name: 'hello' }]
	entries2 := [E{ name: 'hello' }]
	entries3 := [E{ name: 'hello' }]
	println(last_struct_length(entries1) + last_map_length(entries2) + last_array_length(entries3))
}

struct SizedEntry {
	name string
	size int
}

struct SizedTable {
mut:
	entries []SizedEntry
	total   int
}

// Concern: (arr.last()).field must lower to the same in-place borrow as arr.last().field.
// The checker recognizes the borrow through parentheses, and the transformer must unwrap them.
fn (mut t SizedTable) shrink() {
	for t.entries.len > 0 {
		t.total -= (t.entries.last()).size
		t.entries.delete_last()
	}
}

// A literal branch condition cannot mutate the source, so the tail still borrows in place.
fn last_length_in_match(entries []E) int {
	return match 1 {
		1 { entries.last().name.len }
		else { 0 }
	}
}

fn check_parenthesized() {
	mut t := SizedTable{}
	t.entries << SizedEntry{ name: 'a', size: 5 }
	t.entries << SizedEntry{ name: 'b', size: 7 }
	t.total = 12
	t.shrink()
	println(int_str(t.total))
}

fn check_match_branch() {
	entries := [E{ name: 'hello' }]
	println(last_length_in_match(entries))
}

__global (
	make_calls = 0
)

fn make_entries() []SizedEntry {
	make_calls++
	return [SizedEntry{ name: 'a', size: 5 }, SizedEntry{ name: 'b', size: 9 }]
}

// Concern: `last()` reads its receiver twice (`arr[arr.len - 1]`), so a non-lvalue receiver
// such as `make_entries()` must be evaluated exactly once. The borrow lowering binds it to a
// temp instead of duplicating the call.
fn check_receiver_evaluated_once() {
	s := make_entries().last().size
	// size == 9 (last element), make_entries() invoked once (not twice).
	println(int_str(s) + ':' + int_str(make_calls))
}

struct CloneableEntry {
	name string
	size int
}

fn (e CloneableEntry) clone() CloneableEntry {
	return CloneableEntry{ name: e.name.clone(), size: e.size }
}

fn (e CloneableEntry) describe() string {
	return e.name
}

struct Reader implements IClone {
	label string
}

fn (reader Reader) run() string {
	return reader.label
}

fn consume(length int, callback fn () string) int {
	_ = callback()
	return length
}

fn last_length_with_bound_method(arr []E, reader Reader) int {
	return consume(arr.last().name.len, reader.run)
}

// A method value whose element type does provide `clone()` keeps working: the receiver is
// cloned (an independent copy), never an alias of the live array element.
fn check_entry_clone() {
	arr := [CloneableEntry{ name: 'a', size: 1 }, CloneableEntry{ name: 'bb', size: 2 }]
	f := arr.last().describe
	println(f())
}

fn check_default_clone() {
	arr := [E{ name: 'hello' }]
	println(last_length_with_bound_method(arr, Reader{ label: 'run' }))
}

fn main() {
	check_scalar_index()
	check_aggregate_field()
	check_aggregate_arguments()
	check_parenthesized()
	check_match_branch()
	check_receiver_evaluated_once()
	check_entry_clone()
	check_default_clone()
	check_string_consumers()
}
"
	compile := compile_v3_ownership_program(v3_bin, 'owned_array_accessor_consumers', src, '-enable-globals')
	assert_clean_borrow_build(compile)
	bin_path := tmp_array_accessor_borrow_path('owned_array_accessor_consumers_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output == '104\n5\n15\n0\n5\n9:1\nbb\n5\nhello\n5\ntrue\ntrue\nhello!\n<hello>\nhello\nhello  1\ntrue\nhello0\ntrue\n', run.output
}

fn test_owned_scalar_borrows_with_implicit_mutations_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_scalar_implicit_mutations', "struct E {
	name string
}

struct Mutator {
	label string
}

__global entries []E

fn (mutator Mutator) clone() Mutator {
	entries.delete_last()
	return Mutator{ label: mutator.label.clone() }
}

fn (mutator Mutator) run() string {
	return mutator.label
}

fn consume_method(length int, callback fn () string) int {
	_ = callback()
	return length
}

fn last_length_with_bound_method(mutator Mutator) int {
	return consume_method(entries.last().name.len, mutator.run)
}

type Reader = string

fn (reader Reader) clone() Reader {
	entries.delete_last()
	return Reader(string(reader).clone())
}

fn (reader Reader) run() string {
	return reader
}

fn last_length_with_alias_method(reader Reader) int {
	return consume_method(entries.last().name.len, reader.run)
}

fn consume_spread(length int, mutators []Mutator) int {
	_ = mutators
	return length
}

fn last_length_with_spread(mutators []Mutator) int {
	return consume_spread(entries.last().name.len, [...mutators])
}

fn check_struct_method() {
	entries = [E{ name: 'hello' }]
	println(last_length_with_bound_method(Mutator{ label: 'run' }))
}

fn check_alias_method() {
	entries = [E{ name: 'hello' }]
	reader := Reader('run')
	println(last_length_with_alias_method(reader))
}

fn check_array_spread() {
	entries = [E{ name: 'hello' }]
	println(last_length_with_spread([Mutator{ label: 'delete' }]))
}

struct Pair {
	n int = delete_last()
}

fn delete_last() int {
	entries.delete_last()
	return 0
}

fn consume_pair(length int, pair Pair) int {
	_ = pair
	return length
}

fn last_length_with_pair_default() int {
	return consume_pair(entries.last().name.len, Pair{})
}

fn check_pair_default() {
	entries = [E{ name: 'hello' }]
	println(last_length_with_pair_default())
}

fn main() {
	check_struct_method()
	check_alias_method()
	check_array_spread()
	check_pair_default()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['31:24', '46:24', '55:24', '89:22'])
}
