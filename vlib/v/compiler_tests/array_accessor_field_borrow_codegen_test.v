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

fn run_v3_array_accessor_borrow_program(name string, src string) string {
	src_path := '${tmp_array_accessor_borrow_path(name)}.v'
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile := cmdexec.run_in_merged_with_env(array_accessor_borrow_vexe, [src_path, '-b', 'c',
		'-o', bin_path], '', {
		'VFLAGS': ''
	})
	assert_clean_borrow_build(compile)
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
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
fn test_first_last_field_borrow_on_owned_elements() {
	output := run_v3_array_accessor_borrow_program('field_borrows', "struct Entry {
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

fn main() {
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
")
	assert output.split_into_lines() == ['0', '34'], output
}

// The same borrow read inside an imported module: the ownership checker does not
// diagnose non-user modules, so the regressed transform silently produced `(0)` there,
// which is precisely why `v .` on a fresh `--web` project failed to compile.
fn test_first_last_field_borrow_in_imported_module() {
	out := run_v3_array_accessor_borrow_module('borrow_mod', 'module mymod\n\nstruct Entry {\n\tname  string\n\tvalue string\n\tsize  int\n}\n\npub struct Tbl {\nmut:\n\tentries []Entry\n\ttotal   int\n}\n\npub fn (mut t Tbl) add(name string, value string) {\n\tsz := name.len + value.len + 32\n\tfor t.entries.len > 0 && t.total + sz > 100 {\n\t\tt.total -= t.entries.last().size\n\t\tt.entries.delete_last()\n\t}\n\tt.entries.insert(0, Entry{ name: name, value: value, size: sz })\n\tt.total += sz\n}\n\npub fn (t &Tbl) total() int {\n\treturn t.total\n}\n', "module main\n\nimport mymod\n\nfn main() {\n\tmut t := mymod.Tbl{}\n\tt.add('a', 'b')\n\tt.add('c', 'd')\n\tprintln(int_str(t.total()))\n}\n")
	assert out == '68'
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

// Concern: `last()` reads its receiver twice (`arr[arr.len - 1]`), so a non-lvalue receiver
// such as `make_entries()` must be evaluated exactly once. The borrow lowering binds it to a
// temp instead of duplicating the call.
fn test_owned_first_last_receiver_evaluated_once() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_eval_once', "struct E {\n\tname string\n\tsize int\n}\n\n__global (\n\tmake_calls = 0\n)\n\nfn make_entries() []E {\n\tmake_calls++\n\treturn [E{ name: 'a', size: 5 }, E{ name: 'b', size: 9 }]\n}\n\nfn main() {\n\ts := make_entries().last().size\n\tprintln(int_str(s) + ':' + int_str(make_calls))\n}\n", '-enable-globals')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unsupported node kind'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_eval_once_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	// size == 9 (last element), make_entries() invoked once (not twice).
	assert run.output.trim_space() == '9:1', run.output
}

// Concern: `(arr.last()).field` must lower to the same in-place borrow as `arr.last().field`.
// The checker's borrowed-field predicate sees through transparent parentheses to suppress the
// diagnostic, so the transformer must unwrap them too; otherwise the accessor follows its
// independent-copy path and emits an empty `(0)` placeholder that fails C compilation.
fn test_owned_parenthesized_field_borrow() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_paren_borrow', "struct E {\n\tname string\n\tsize int\n}\n\nstruct T {\nmut:\n\tentries []E\n\ttotal   int\n}\n\nfn (mut t T) shrink() {\n\tfor t.entries.len > 0 {\n\t\tt.total -= (t.entries.last()).size\n\t\tt.entries.delete_last()\n\t}\n}\n\nfn main() {\n\tmut t := T{}\n\tt.entries << E{ name: 'a', size: 5 }\n\tt.entries << E{ name: 'b', size: 7 }\n\tt.total = 12\n\tt.shrink()\n\tprintln(int_str(t.total))\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unsupported node kind'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_paren_borrow_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '0', run.output
}

// A method value whose element type does provide `clone()` keeps working: the receiver is
// cloned (an independent copy), never an alias of the live array element.
fn test_owned_method_value_with_clone_runs() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_method_value_clone', "struct E {\n\tname string\n\tsize int\n}\n\nfn (e E) clone() E {\n\treturn E{ name: e.name.clone(), size: e.size }\n}\n\nfn (e E) describe() string {\n\treturn e.name\n}\n\nfn main() {\n\tarr := [E{ name: 'a', size: 1 }, E{ name: 'bb', size: 2 }]\n\tf := arr.last().describe\n\tprintln(f())\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unsupported node kind'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_method_value_clone_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'bb', run.output
}

// These consumers can borrow the element for the string field read.
fn test_owned_string_consumers_borrow_array_elements() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_string_field_reads', "struct E {
	name string
}

struct T {
mut:
	entries []E
}

// The string clone can outlive the borrowed element.
fn (t &T) last_name() string {
	return t.entries.last().name.clone()
}

struct SizedEntry {
	name string
	size int
}

struct SizedTable {
mut:
	entries []SizedEntry
}

// Only the length leaves this field chain.
fn (t &SizedTable) last_name_len() int {
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
	mut t := SizedTable{}
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

fn main() {
	check_clone()
	check_length()
	check_comparison()
	check_membership()
}
", '')
	assert_clean_borrow_build(compile)
	bin_path := tmp_array_accessor_borrow_path('owned_string_field_reads_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['hello', '5', 'true', 'true'], run.output
}

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

fn test_owned_allocating_string_consumers_borrow_array_elements() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_allocating_string_consumers', "struct E {
	name string
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

fn main() {
	println(decorated_last_name())
	println(interpolated_last_name())
	println(formatted_last_name())
	println(formatted_last_name_with_sibling())
}
", '')
	assert_clean_borrow_build(compile)
	bin_path := tmp_array_accessor_borrow_path('owned_allocating_string_consumers_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output == 'hello!\n<hello>\nhello\nhello  1\n', run.output
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

// An allocating consumer is borrow-safe only when evaluating its other operands cannot mutate
// the source array first. Keep the accessor on its independent-copy path when a sibling call can
// delete the selected element before string concatenation consumes the field.
fn test_owned_string_consumer_with_mutating_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	// Diagnostic locations include the Drop interface prepended to each fixture.
	direct := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_string_mutating_siblings', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '!'
}

fn last_name_then_delete(mut arr []E) string {
	return arr.last().name + delete_last(mut arr)
}

fn interpolate_then_delete(mut arr []E) string {
	return '\${arr.last().name}\${delete_last(mut arr)}'
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

fn main() {
	check_concat()
	check_interpolation()
	check_comparison()
}
", '-nocolor')
	assert_independent_array_element_errors(direct, ['16:9', '20:12', '29:9'])

	stringifiers := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_string_mutating_stringifiers', "struct E {
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

fn main() {
	check_stringifier()
	check_nested_stringifier()
}
", '-enable-globals -nocolor')
	assert_independent_array_element_errors(stringifiers, ['20:12', '28:12'])

	stable_stringifier := compile_v3_ownership_program(v3_bin, 'owned_string_interp_stable_auto_stringifier', "struct E {
	name string
}

struct Stable {
	n int
}

fn last_name_then_stringify(entries []E, stable Stable) string {
	return '\${entries.last().name}\${stable}'
}

fn main() {
	entries := [E{ name: 'hello' }]
	println(last_name_then_stringify(entries, Stable{ n: 1 }))
}
", '')
	assert stable_stringifier.exit_code == 0, stable_stringifier.output
	assert !stable_stringifier.output.contains('cannot return an independent array element'), stable_stringifier.output
}

fn test_owned_string_interpolation_with_custom_stringifier_part_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	// The borrowed interpolation part is itself a string alias with a custom `str()`. The
	// interpolation dispatches through that method (not the builtin stringifier), and it mutates
	// the source array before the borrowed shallow receiver is read, so the borrow is unsafe.
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_string_interp_self_stringifier', "type Name = string

struct E {
	name Name
}

__global entries []E

fn (name Name) str() string {
	entries.delete_last()
	return string(name)
}

fn last_label() string {
	return '\${entries.last().name}\${0}'
}

fn main() {
	entries = [E{ name: Name('hello') }]
	println(last_label())
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output

	// A plain `string` field uses the builtin stringifier, so the borrow stays safe.
	safe := compile_v3_ownership_program(v3_bin, 'owned_string_interp_plain_part', "struct E {
	name string
}

fn last_label(entries []E) string {
	return '\${entries.last().name}\${0}'
}

fn main() {
	entries := [E{ name: 'hello' }]
	println(last_label(entries))
}
", '')
	assert safe.exit_code == 0, safe.output
	safe_bin_path := tmp_array_accessor_borrow_path('owned_string_interp_plain_part_bin')
	safe_run := os.exec([safe_bin_path])
	assert safe_run.exit_code == 0, safe_run.output
	assert safe_run.output.trim_space() == 'hello0', safe_run.output
}

fn test_owned_scalar_borrows_with_mutating_siblings_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_scalar_mutating_siblings', "struct E {
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

fn main() {
	check_index()
	check_field()
	check_return()
}
", '-nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['16:9', '30:18', '39:9'])
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

fn test_owned_string_comparison_checks_overloaded_siblings() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	builtin := compile_v3_ownership_program(v3_bin, 'owned_builtin_infix_sibling', "struct E {
	name string
}

fn last_name_matches(entries []E) bool {
	return entries.last().name == ('he' + 'llo')
}

fn main() {
	entries := [E{ name: 'hello' }]
	println(last_name_matches(entries))
}
", '')
	assert builtin.exit_code == 0, builtin.output
	assert !builtin.output.contains('cannot return an independent array element'), builtin.output
	builtin_bin_path := tmp_array_accessor_borrow_path('owned_builtin_infix_sibling_bin')
	builtin_run := os.exec([builtin_bin_path])
	assert builtin_run.exit_code == 0, builtin_run.output
	assert builtin_run.output.trim_space() == 'true', builtin_run.output

	overloaded := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_comparison_mutating_operator_siblings', "struct E {
	name string
}

struct Mutator {}

__global entries []E

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
	check_infix()
	check_index()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(overloaded, ['22:9', '32:9'])
}

fn test_owned_aggregate_and_interface_comparisons_reject_mutating_overloads() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_mutating_comparison_overloads', "struct Name {
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

fn main() {
	check_aggregate_equality()
	check_aggregate_inequality()
	check_interface_equality()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert ownership_error_lines(compile) == [
		independent_array_element_error_at('22:9', 'E'),
		independent_array_element_error_at('26:9', 'E'),
		independent_array_element_error_at('53:9', 'InterfaceEntry'),
	], compile.output
}

fn test_owned_membership_rejects_mutating_overloads() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_mutating_membership_overloads', "struct Needle {
	text string
}

struct E {
	value Needle
}

__global entries []E

fn (left Needle) == (right Needle) bool {
	entries.delete_last()
	return left.text == right.text
}

fn last_value_is_in(haystack []Needle) bool {
	return entries.last().value in haystack
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

fn (mut value MembershipEntry) drop() {}

fn check_direct_membership() {
	entries = [E{ value: Needle{ text: 'hello' } }]
	println(last_value_is_in([Needle{ text: 'hello' }]))
}

fn check_membership_sibling() {
	membership_entries = [MembershipEntry{ name: 'hello' }]
	println(last_length_with_membership(MembershipNeedle{ value: 1 }, [MembershipNeedle{ value: 1 }]))
}

fn main() {
	check_direct_membership()
	check_membership_sibling()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert ownership_error_lines(compile) == [
		independent_array_element_error_at('22:9', 'E'),
		independent_array_element_error_at('45:17', 'MembershipEntry'),
	], compile.output
}

fn test_owned_nested_consumers_with_mutating_siblings_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_nested_consumers_mutating_siblings', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
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

fn main() {
	check_nested_concat()
	check_concat_arg()
	check_interpolation_arg()
	check_comparison_arg()
	check_negated_comparison_arg()
	check_length_arg()
	check_conditional_arg()
	check_logical_arg()
	check_nested_arg()
}
", '-nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['32:10', '36:24', '40:28', '44:22', '48:24',
		'52:21', '56:31', '60:23', '64:33'])
}

// Indexing and chained length reads can borrow the original element. A length
// read can also share an initializer or call with stable fields or arguments.
fn test_owned_scalar_consumers_borrow_array_elements() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_consumers', "struct E {
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

fn main() {
	check_scalar_index()
	check_aggregate_field()
	check_aggregate_arguments()
}
", '')
	assert_clean_borrow_build(compile)
	bin_path := tmp_array_accessor_borrow_path('owned_scalar_consumers_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['104', '5', '15'], run.output
}

fn test_owned_conditional_prerequisites_that_mutate_source_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_mutating_prerequisites', "struct E {
	name string
}

fn delete_last(mut entries []E) bool {
	entries.delete_last()
	return true
}

fn if_length(mut entries []E) int {
	return if delete_last(mut entries) { entries.last().name.len } else { 0 }
}

fn match_subject_length(mut entries []E) int {
	return match delete_last(mut entries) {
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
	check_if()
	check_match_subject()
	check_or()
	check_match_branch()
}
", '-nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['16:39', '21:10', '32:48', '42:45'])

	// A literal branch condition cannot mutate the source, so the tail still borrows in place.
	safe := compile_v3_ownership_program(v3_bin, 'owned_match_branch_condition_stable', "struct E {
	name string
}

fn last_length(entries []E) int {
	return match 1 {
		1 { entries.last().name.len }
		else { 0 }
	}
}

fn main() {
	entries := [E{ name: 'hello' }]
	println(last_length(entries))
}
", '')
	assert safe.exit_code == 0, safe.output
	safe_bin_path := tmp_array_accessor_borrow_path('owned_match_branch_condition_stable_bin')
	safe_run := os.exec([safe_bin_path])
	assert safe_run.exit_code == 0, safe_run.output
	assert safe_run.output.trim_space() == '5', safe_run.output
}

fn test_owned_scalar_consumer_with_mutating_struct_default_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_scalar_mutating_struct_default_sibling', "struct E {
	name string
}

struct Pair {
	n int = delete_last()
}

__global entries []E

fn delete_last() int {
	entries.delete_last()
	return 0
}

fn consume(length int, pair Pair) int {
	_ = pair
	return length
}

fn last_length() int {
	return consume(entries.last().name.len, Pair{})
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_length())
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_scalar_consumers_with_cloning_siblings() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_uncloneable_array_ownership_program(v3_bin, 'owned_scalar_cloning_siblings', "struct E {
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

fn main() {
	check_struct_method()
	check_alias_method()
	check_array_spread()
}
", '-enable-globals -nocolor')
	// Locations include the prepended Drop interface.
	assert_independent_array_element_errors(compile, ['31:24', '46:24', '55:24'])

	safe := compile_v3_ownership_program(v3_bin, 'owned_scalar_default_clone_method_value_sibling', "struct E {
	name string
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

fn main() {
	arr := [E{ name: 'hello' }]
	println(last_length_with_bound_method(arr, Reader{ label: 'run' }))
}
", '')
	assert safe.exit_code == 0, safe.output
	safe_bin_path := tmp_array_accessor_borrow_path('owned_scalar_default_clone_method_value_sibling_bin')
	safe_run := os.exec([safe_bin_path])
	assert safe_run.exit_code == 0, safe_run.output
	assert safe_run.output.trim_space() == '5', safe_run.output
}

fn test_owned_field_membership_with_stable_array_literal_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_stable_array_membership', "struct E {
	name string
}

fn has_known_last_name(entries []E) bool {
	return entries.last().name in ['hello', 'other']
}

fn main() {
	entries := [E{ name: 'hello' }]
	println(has_known_last_name(entries))
}
", '')
	assert compile.exit_code == 0, compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_field_stable_array_membership_bin')
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true', run.output
}
