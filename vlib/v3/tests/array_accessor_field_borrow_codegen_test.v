import os

const array_accessor_borrow_vexe = @VEXE
const array_accessor_borrow_tests_dir = os.dir(@FILE)
const array_accessor_borrow_v3_dir = os.dir(array_accessor_borrow_tests_dir)
const array_accessor_borrow_vlib_dir = os.dir(array_accessor_borrow_v3_dir)
const array_accessor_borrow_v3_src = os.join_path(array_accessor_borrow_v3_dir, 'v3.v')

fn tmp_array_accessor_borrow_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn build_v3_array_accessor_borrow() string {
	v3_bin := tmp_array_accessor_borrow_path('array_accessor_borrow')
	build :=
		os.execute('${os.quoted_path(array_accessor_borrow_vexe)} -gc none -path "${array_accessor_borrow_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(array_accessor_borrow_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
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

fn run_v3_array_accessor_borrow_program(v3_bin string, name string, src string) string {
	src_path := '${tmp_array_accessor_borrow_path(name)}.v'
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert_clean_borrow_build(compile)
	run := os.execute(os.quoted_path(bin_path))
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
	v3_bin := build_v3_array_accessor_borrow()
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(proj_dir)} -b c -o ${os.quoted_path(bin_path)}')
	assert_clean_borrow_build(compile)
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// Reading a field of `arr.first()`/`arr.last()` is a borrow of the stored element,
// not an independent copy, so it must lower to an in-place `arr[..]` access even when
// the element type owns heap data and has no `clone()` method. This mirrors the HPACK
// dynamic table (`net.http` / `net.quic`) that `v new --web` pulls in via `veb`.
fn test_first_last_field_borrow_on_owned_elements() {
	v3_bin := build_v3_array_accessor_borrow()

	// The exact evict-loop shape: read the last element's scalar field, then remove it.
	last_field := run_v3_array_accessor_borrow_program(v3_bin, 'last_field', "struct Entry {\n\tname  string\n\tvalue string\n\tsize  int\n}\n\nstruct Tbl {\nmut:\n\tentries []Entry\n\ttotal   int\n}\n\nfn (mut t Tbl) shrink() {\n\tfor t.entries.len > 0 {\n\t\tt.total -= t.entries.last().size\n\t\tt.entries.delete_last()\n\t}\n}\n\nfn main() {\n\tmut t := Tbl{}\n\tt.entries << Entry{ name: 'a', value: 'b', size: 5 }\n\tt.entries << Entry{ name: 'c', value: 'd', size: 7 }\n\tt.total = 12\n\tt.shrink()\n\tprintln(int_str(t.total))\n}\n")
	assert last_field == '0'

	// first()/last() field reads through both value and ref receivers, incl. a chained
	// field access (`last().name.len`).
	mixed := run_v3_array_accessor_borrow_program(v3_bin, 'mixed_field', "struct Entry {\n\tname string\n\tn    int\n}\n\nstruct Box {\nmut:\n\tentries []Entry\n}\n\nfn (b &Box) probe() int {\n\treturn b.entries.first().n + b.entries.last().n + b.entries.last().name.len\n}\n\nfn main() {\n\tmut b := Box{}\n\tb.entries << Entry{ name: 'ab', n: 10 }\n\tb.entries << Entry{ name: 'cdef', n: 20 }\n\tprintln(int_str(b.probe()))\n}\n")
	assert mixed == '34'
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
	v3_bin := tmp_array_accessor_borrow_path('array_accessor_borrow_ownership')
	build :=
		os.execute('${os.quoted_path(array_accessor_borrow_vexe)} -gc none -d ownership -path "${array_accessor_borrow_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(array_accessor_borrow_v3_src)}')
	if build.output.contains('ownership support is not compiled into this v3 executable') {
		// The bootstrap compiler running this test lacks the ownership checker, so it
		// cannot build an ownership-enabled v3. Skip rather than fail on such a host.
		return none
	}
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn compile_v3_ownership_program(v3_bin string, name string, src string, extra_args string) os.Result {
	src_path := '${tmp_array_accessor_borrow_path(name)}.v'
	bin_path := tmp_array_accessor_borrow_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	return os.execute('${os.quoted_path(v3_bin)} -ownership -d ownership -no-parallel ${extra_args} -o ${os.quoted_path(bin_path)} ${os.quoted_path(src_path)}')
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
	run := os.execute(os.quoted_path(bin_path))
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
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '0', run.output
}

// Concern: a bound method value (`arr.last().method`) is not a field read. Closure
// generation shallow-copies the receiver, so it must keep the copying accessor semantics
// instead of borrowing the array element. For an owned element with no `clone()` method the
// accessor is genuinely impossible, so this must be rejected rather than silently miscompiled.
fn test_owned_method_value_receiver_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_method_value', "struct E {\n\tname string\n\tsize int\n}\n\nfn (e E) describe() string {\n\treturn e.name\n}\n\nfn main() {\n\tarr := [E{ name: 'a', size: 1 }, E{ name: 'b', size: 2 }]\n\tf := arr.last().describe\n\tprintln(f())\n}\n", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

// A method value whose element type does provide `clone()` keeps working: the receiver is
// cloned (an independent copy), never an alias of the live array element.
fn test_owned_method_value_with_clone_runs() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_method_value_clone', "struct E {\n\tname string\n\tsize int\n}\n\nfn (e E) clone() E {\n\treturn E{ name: e.name.clone(), size: e.size }\n}\n\nfn (e E) describe() string {\n\treturn e.name\n}\n\nfn main() {\n\tarr := [E{ name: 'a', size: 1 }, E{ name: 'bb', size: 2 }]\n\tf := arr.last().describe\n\tprintln(f())\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unsupported node kind'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_method_value_clone_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'bb', run.output
}

// Concern: only the chain's *final* value is read out, so the borrow is safe only when that
// value does not own heap data. `arr.last().name` returns the `string` field itself; borrowing
// would alias the array element's storage, so a returned reference could dangle once the array
// is destroyed. A clone-less owned element must therefore be rejected, not borrowed.
fn test_owned_escaping_field_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_escaping_field', "struct E {\n\tname string\n\tsize int\n}\n\nstruct T {\nmut:\n\tentries []E\n}\n\nfn (t &T) last_name() string {\n\treturn t.entries.last().name\n}\n\nfn main() {\n\tmut t := T{}\n\tt.entries << E{ name: 'hello', size: 5 }\n\tprintln(t.last_name())\n}\n", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

// An explicit builtin string clone allocates independent storage before the selected field
// escapes, so the array element itself can remain borrowed even when it has no clone method.
fn test_owned_escaping_field_string_clone_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_escaping_field_string_clone', "struct E {\n\tname string\n}\n\nstruct T {\nmut:\n\tentries []E\n}\n\nfn (t &T) last_name() string {\n\treturn t.entries.last().name.clone()\n}\n\nfn main() {\n\tmut t := T{}\n\tt.entries << E{ name: 'hello' }\n\tname := t.last_name()\n\tt.entries.delete_last()\n\tprintln(name)\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_escaping_field_string_clone_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello', run.output
}

// The exclusion is limited to the chain's final value: an owned *intermediate* field whose
// own final value is non-owned stays borrowable. `arr.last().name.len` reads only the length
// (an int), so nothing owned escapes and it must still lower to the in-place borrow.
fn test_owned_intermediate_field_chain_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_intermediate_chain', "struct E {\n\tname string\n\tsize int\n}\n\nstruct T {\nmut:\n\tentries []E\n}\n\nfn (t &T) last_name_len() int {\n\treturn t.entries.last().name.len\n}\n\nfn main() {\n\tmut t := T{}\n\tt.entries << E{ name: 'hello', size: 5 }\n\tprintln(int_str(t.last_name_len()))\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('unsupported node kind'), compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_intermediate_chain_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5', run.output
}

// An owned field used only by a comparison does not escape. The accessor can borrow the
// array element even when its type has no clone method.
fn test_owned_field_comparison_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_comparison', "struct E {\n\tname string\n}\n\nstruct T {\nmut:\n\tentries []E\n}\n\nfn (t &T) has_name(name string) bool {\n\treturn t.entries.last().name == name\n}\n\nfn main() {\n\tmut t := T{}\n\tt.entries << E{ name: 'hello' }\n\tprintln(t.has_name('hello'))\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_field_comparison_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true', run.output
}

// Builtin membership only reads and compares the selected string, so the array element can
// remain borrowed when no element equality overload can invoke user code.
fn test_owned_field_membership_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_membership', "struct E {
	name string
}

fn has_last_name(entries []E, names []string) bool {
	return entries.last().name in names
}

fn main() {
	entries := [E{ name: 'hello' }]
	println(has_last_name(entries, ['hello']))
}
", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_field_membership_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true', run.output
}

fn test_owned_field_overloaded_membership_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_overloaded_membership', "struct Needle {
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

fn main() {
	entries = [E{ value: Needle{ text: 'hello' } }]
	println(last_value_is_in([Needle{ text: 'hello' }]))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

// A non-owning pointer selected from the borrowed element can still outlive the array, so it
// must not use the in-place borrow path.
fn test_owned_pointer_field_escape_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_pointer_field_escape', "struct E {\n\tname string\n}\n\nfn last_name_ptr() &u8 {\n\tarr := [E{ name: 'hello' }]\n\treturn arr.last().name.str\n}\n\nfn main() {\n\tprintln(unsafe { cstring_to_vstring(last_name_ptr()) })\n}\n", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

// Indexing a borrowed owned field immediately yields a scalar, so no array-owned storage
// escapes and a clone-less element can stay borrowed in place.
fn test_owned_field_scalar_index_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_scalar_index', "struct E {\n\tname string\n}\n\nfn last_initial() u8 {\n\tarr := [E{ name: 'hello' }]\n\treturn arr.last().name[0]\n}\n\nfn main() {\n\tprintln(int_str(last_initial()))\n}\n", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_field_scalar_index_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '104', run.output
}

// A pointer nested inside a non-owning aggregate can still escape the element's lifetime.
// The final-type check must recurse through the selected aggregate instead of accepting it
// merely because the aggregate itself has no destructor.
fn test_owned_nested_pointer_aggregate_escape_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_nested_pointer_aggregate_escape', "struct View {
	ptr &u8
}

struct Wrapper {
	view View
}

struct E {
	name    string
	wrapper Wrapper
}

fn last_wrapper() Wrapper {
	arr := [E{
		name: 'hello'
		wrapper: Wrapper{
			view: View{
				ptr: unsafe { nil }
			}
		}
	}]
	return arr.last().wrapper
}

fn main() {
	_ = last_wrapper()
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

// Builtin string concatenation allocates the result and copies both operands, so the selected
// string cannot escape through the `+` consumer and the array element can stay borrowed.
fn test_owned_string_concatenation_consumer_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_string_concat_consumer', "struct E {
	name string
}

fn decorated_last_name() string {
	arr := [E{ name: 'hello' }]
	return arr.last().name + '!'
}

fn main() {
	println(decorated_last_name())
}
", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_string_concat_consumer_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello!', run.output
}

// A multi-part interpolation lowers through string__plus and copies the selected string, so the
// accessor can borrow the array element just like the explicit concatenation case.
fn test_owned_allocating_string_interpolation_consumer_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_string_interp_consumer', "struct E {
	name string
}

fn decorated_last_name() string {
	arr := [E{ name: 'hello' }]
	return '<\${arr.last().name}>'
}

fn main() {
	println(decorated_last_name())
}
", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_string_interp_consumer_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '<hello>', run.output
}

// A format directive wraps the expression before the interpolation consumes it. The checker
// must see through that wrapper, and formatting must stabilize a borrowed string because
// padding is allowed to return its input unchanged when the requested width is already met.
fn test_owned_formatted_string_interpolation_consumer_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_formatted_string_interp_consumer', "struct E {
	name string
}

fn formatted_last_name() string {
	arr := [E{ name: 'hello' }]
	return '\${arr.last().name:3s}'
}

fn main() {
	println(formatted_last_name())
}
", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_formatted_string_interp_consumer_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'hello', run.output

	siblings := compile_v3_ownership_program(v3_bin, 'owned_formatted_string_interp_sibling', "struct E {
	name string
}

fn formatted_last_name() string {
	arr := [E{ name: 'hello' }]
	return '\${arr.last().name:3s}\${1:3d}'
}

fn main() {
	println(formatted_last_name())
}
", '')
	assert siblings.exit_code == 0, siblings.output
	assert !siblings.output.contains('cannot return an independent array element'), siblings.output
	siblings_bin_path := tmp_array_accessor_borrow_path('owned_formatted_string_interp_sibling_bin')
	siblings_run := os.execute(os.quoted_path(siblings_bin_path))
	assert siblings_run.exit_code == 0, siblings_run.output
	assert siblings_run.output == 'hello  1\n', siblings_run.output
}

// An allocating consumer is borrow-safe only when evaluating its other operands cannot mutate
// the source array first. Keep the accessor on its independent-copy path when a sibling call can
// delete the selected element before string concatenation consumes the field.
fn test_owned_string_consumer_with_mutating_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	concat := compile_v3_ownership_program(v3_bin, 'owned_string_concat_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '!'
}

fn last_name_then_delete(mut arr []E) string {
	return arr.last().name + delete_last(mut arr)
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert concat.exit_code != 0, concat.output
	assert concat.output.contains('cannot return an independent array element'), concat.output

	interp := compile_v3_ownership_program(v3_bin, 'owned_string_interp_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '!'
}

fn last_name_then_delete(mut arr []E) string {
	return '\${arr.last().name}\${delete_last(mut arr)}'
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert interp.exit_code != 0, interp.output
	assert interp.output.contains('cannot return an independent array element'), interp.output

	stringifier := compile_v3_ownership_program(v3_bin, 'owned_string_interp_mutating_stringifier', "struct E {
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

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_name_then_stringify(Mutator{}))
}
", '-enable-globals')
	assert stringifier.exit_code != 0, stringifier.output
	assert stringifier.output.contains('cannot return an independent array element'), stringifier.output

	nested_stringifier := compile_v3_ownership_program(v3_bin, 'owned_string_interp_nested_mutating_stringifier', "struct E {
	name string
}

struct Mutator {}

struct Wrapper {
	mutator Mutator
}

__global entries []E

fn (Mutator) str() string {
	entries.delete_last()
	return '!'
}

fn last_name_then_stringify(wrapper Wrapper) string {
	return '\${entries.last().name}\${wrapper}'
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_name_then_stringify(Wrapper{}))
}
", '-enable-globals')
	assert nested_stringifier.exit_code != 0, nested_stringifier.output
	assert nested_stringifier.output.contains('cannot return an independent array element'), nested_stringifier.output

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

	comparison := compile_v3_ownership_program(v3_bin, 'owned_string_comparison_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return 'hello'
}

fn last_name_then_delete(mut arr []E) bool {
	return arr.last().name == delete_last(mut arr)
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert comparison.exit_code != 0, comparison.output
	assert comparison.output.contains('cannot return an independent array element'), comparison.output
}

fn test_owned_string_interpolation_with_custom_stringifier_part_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	// The borrowed interpolation part is itself a string alias with a custom `str()`. The
	// interpolation dispatches through that method (not the builtin stringifier), and it mutates
	// the source array before the borrowed shallow receiver is read, so the borrow is unsafe.
	compile := compile_v3_ownership_program(v3_bin, 'owned_string_interp_self_stringifier', "type Name = string

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
	safe_run := os.execute(os.quoted_path(safe_bin_path))
	assert safe_run.exit_code == 0, safe_run.output
	assert safe_run.output.trim_space() == 'hello0', safe_run.output
}

fn test_owned_string_index_with_mutating_index_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_string_index_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) int {
	arr.delete_last()
	return 0
}

fn last_initial_then_delete(mut arr []E) u8 {
	return arr.last().name[delete_last(mut arr)]
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_initial_then_delete(mut arr))
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_field_overloaded_comparison_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_overloaded_comparison', "struct Name {
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

fn main() {
	arr := [E{ name: Name{ text: 'hello' } }]
	println(last_name_matches(arr))
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_string_comparison_with_infix_sibling_checks_overloads() {
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
	builtin_run := os.execute(os.quoted_path(builtin_bin_path))
	assert builtin_run.exit_code == 0, builtin_run.output
	assert builtin_run.output.trim_space() == 'true', builtin_run.output

	overloaded := compile_v3_ownership_program(v3_bin, 'owned_overloaded_infix_sibling', "struct E {
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

fn last_name_matches(left Mutator, right Mutator) bool {
	return entries.last().name == (left + right)
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_name_matches(Mutator{}, Mutator{}))
}
", '-enable-globals')
	assert overloaded.exit_code != 0, overloaded.output
	assert overloaded.output.contains('cannot return an independent array element'), overloaded.output
}

fn test_owned_aggregate_field_overloaded_comparison_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_aggregate_field_overloaded_comparison', "struct Name {
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

fn main() {
	entries = [E{ names: [Name{ text: 'hello' }] }]
	println(last_names_match([Name{ text: 'hello' }]))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_aggregate_field_overloaded_inequality_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_aggregate_field_overloaded_inequality', "struct Name {
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

fn last_names_differ(other []Name) bool {
	return entries.last().names != other
}

fn main() {
	entries = [E{ names: [Name{ text: 'hello' }] }]
	println(last_names_differ([Name{ text: 'world' }]))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_interface_field_overloaded_comparison_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_interface_field_overloaded_comparison', "interface Named {
	name() string
}

struct Name {
	text string
}

struct E {
	value Named
}

__global entries []E

fn (name Name) name() string {
	return name.text
}

fn (left Name) == (right Name) bool {
	entries.delete_last()
	return left.text == right.text
}

fn last_matches(other Named) bool {
	return entries.last().value == other
}

fn main() {
	entries = [E{ value: Named(Name{ text: 'hello' }) }]
	println(last_matches(Named(Name{ text: 'hello' })))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_field_with_overloaded_membership_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_overloaded_membership_sibling', "struct E {
	name string
}

struct Needle {
	value int
}

__global entries []E

fn (left Needle) == (right Needle) bool {
	entries.delete_last()
	return left.value == right.value
}

fn consume(length int, found bool) int {
	return if found { length } else { 0 }
}

fn last_length_with_membership(needle Needle, haystack []Needle) int {
	return consume(entries.last().name.len, needle in haystack)
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_length_with_membership(Needle{ value: 1 }, [Needle{ value: 1 }]))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_nested_string_consumer_with_mutating_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_nested_string_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn last_name_then_delete(mut arr []E) string {
	return (arr.last().name + '!') + delete_last(mut arr)
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output

	call := compile_v3_ownership_program(v3_bin, 'owned_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value string, suffix string) string {
	return value + suffix
}

fn last_name_then_delete(mut arr []E) string {
	return consume(arr.last().name + '!', delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert call.exit_code != 0, call.output
	assert call.output.contains('cannot return an independent array element'), call.output

	interpolation_call := compile_v3_ownership_program(v3_bin, 'owned_interpolation_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value string, suffix string) string {
	return value + suffix
}

fn last_name_then_delete(mut arr []E) string {
	return consume('<\${arr.last().name}>', delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert interpolation_call.exit_code != 0, interpolation_call.output
	assert interpolation_call.output.contains('cannot return an independent array element'), interpolation_call.output

	comparison_call := compile_v3_ownership_program(v3_bin, 'owned_comparison_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value bool, suffix string) bool {
	return value
}

fn last_name_then_delete(mut arr []E) bool {
	return consume(arr.last().name == 'hello', delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert comparison_call.exit_code != 0, comparison_call.output
	assert comparison_call.output.contains('cannot return an independent array element'), comparison_call.output

	wrapped_comparison_call := compile_v3_ownership_program(v3_bin, 'owned_wrapped_comparison_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value bool, suffix string) bool {
	return value
}

fn last_name_then_delete(mut arr []E) bool {
	return consume(!(arr.last().name == 'hello'), delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert wrapped_comparison_call.exit_code != 0, wrapped_comparison_call.output
	assert wrapped_comparison_call.output.contains('cannot return an independent array element'), wrapped_comparison_call.output

	scalar_selector_call := compile_v3_ownership_program(v3_bin, 'owned_scalar_selector_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value int, suffix string) int {
	return value
}

fn last_name_len_then_delete(mut arr []E) int {
	return consume(arr.last().name.len, delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_len_then_delete(mut arr))
}
", '')
	assert scalar_selector_call.exit_code != 0, scalar_selector_call.output
	assert scalar_selector_call.output.contains('cannot return an independent array element'), scalar_selector_call.output

	conditional_call := compile_v3_ownership_program(v3_bin, 'owned_conditional_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value int, suffix string) int {
	return value
}

fn last_name_len_then_delete(mut arr []E, flag bool) int {
	return consume(if flag { arr.last().name.len } else { 0 }, delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_len_then_delete(mut arr, true))
}
", '')
	assert conditional_call.exit_code != 0, conditional_call.output
	assert conditional_call.output.contains('cannot return an independent array element'), conditional_call.output

	logical_wrapper_call := compile_v3_ownership_program(v3_bin, 'owned_logical_wrapper_call_argument_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn consume(value bool, suffix string) bool {
	return value
}

fn last_name_then_delete(mut arr []E) bool {
	return consume((arr.last().name == 'hello') && true, delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert logical_wrapper_call.exit_code != 0, logical_wrapper_call.output
	assert logical_wrapper_call.output.contains('cannot return an independent array element'), logical_wrapper_call.output

	nested_call := compile_v3_ownership_program(v3_bin, 'owned_nested_call_argument_mutating_outer_sibling', "struct E {
	name string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn identity(value string) string {
	return value
}

fn consume(value string, suffix string) string {
	return value + suffix
}

fn last_name_then_delete(mut arr []E) string {
	return consume(identity(arr.last().name + '!'), delete_last(mut arr))
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_name_then_delete(mut arr))
}
", '')
	assert nested_call.exit_code != 0, nested_call.output
	assert nested_call.output.contains('cannot return an independent array element'), nested_call.output
}

fn test_owned_scalar_consumer_in_aggregate_with_mutating_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_aggregate_mutating_sibling', "struct E {
	name string
}

struct Pair {
	n int
	s string
}

fn delete_last(mut arr []E) string {
	arr.delete_last()
	return '?'
}

fn last_length_then_delete(mut arr []E) Pair {
	return Pair{ n: arr.last().name.len, s: delete_last(mut arr) }
}

fn main() {
	mut arr := [E{ name: 'hello' }]
	println(last_length_then_delete(mut arr).n)
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_scalar_consumer_in_aggregate_with_stable_field_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_aggregate_stable_field', "struct E {
	name string
}

struct Pair {
	n int
	s string
}

fn last_length(arr []E) Pair {
	return Pair{ n: arr.last().name.len, s: 'ok' }
}

fn main() {
	arr := [E{ name: 'hello' }]
	println(last_length(arr).n)
}
", '')
	assert compile.exit_code == 0, compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_scalar_aggregate_stable_field_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5', run.output
}

fn test_owned_scalar_consumer_with_stable_aggregate_argument_borrows() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_stable_aggregate_argument', "struct E {
	name string
}

struct Pair {
	n int
}

fn consume_struct(length int, pair Pair) int {
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
	return consume_struct(entries.last().name.len, Pair{ n: 1 })
}

fn last_map_length(entries []E) int {
	return consume_map(entries.last().name.len, { 'n': 1 })
}

fn last_array_length(entries []E) int {
	return consume_array(entries.last().name.len, []int{len: 1, init: 1})
}

fn main() {
	entries1 := [E{ name: 'hello' }]
	entries2 := [E{ name: 'hello' }]
	entries3 := [E{ name: 'hello' }]
	println(last_struct_length(entries1) + last_map_length(entries2) + last_array_length(entries3))
}
", '')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot return an independent array element'), compile.output
	bin_path := tmp_array_accessor_borrow_path('owned_scalar_stable_aggregate_argument_bin')
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '15', run.output
}

fn test_owned_conditional_prerequisites_that_mutate_source_are_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	if_compile := compile_v3_ownership_program(v3_bin, 'owned_mutating_if_prerequisite', "struct E {
	name string
}

fn delete_last(mut entries []E) bool {
	entries.delete_last()
	return true
}

fn last_length(mut entries []E) int {
	return if delete_last(mut entries) { entries.last().name.len } else { 0 }
}

fn main() {
	mut entries := [E{ name: 'hello' }]
	println(last_length(mut entries))
}
", '')
	assert if_compile.exit_code != 0, if_compile.output
	assert if_compile.output.contains('cannot return an independent array element'), if_compile.output

	match_compile := compile_v3_ownership_program(v3_bin, 'owned_mutating_match_prerequisite', "struct E {
	name string
}

fn delete_last(mut entries []E) bool {
	entries.delete_last()
	return true
}

fn last_length(mut entries []E) int {
	return match delete_last(mut entries) {
		true { entries.last().name.len }
		else { 0 }
	}
}

fn main() {
	mut entries := [E{ name: 'hello' }]
	println(last_length(mut entries))
}
", '')
	assert match_compile.exit_code != 0, match_compile.output
	assert match_compile.output.contains('cannot return an independent array element'), match_compile.output

	or_compile := compile_v3_ownership_program(v3_bin, 'owned_mutating_or_prerequisite', "struct E {
	name string
}

fn delete_last(mut entries []E) !int {
	entries.delete_last()
	return error('deleted')
}

fn last_length(mut entries []E) int {
	return delete_last(mut entries) or { entries.last().name.len }
}

fn main() {
	mut entries := [E{ name: 'hello' }]
	println(last_length(mut entries))
}
", '')
	assert or_compile.exit_code != 0, or_compile.output
	assert or_compile.output.contains('cannot return an independent array element'), or_compile.output
}

fn test_owned_match_branch_condition_that_mutates_source_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	// A branch condition is evaluated before the branch tail, so a condition that empties the
	// array invalidates the accessor borrowed in that tail. It must keep copying semantics.
	compile := compile_v3_ownership_program(v3_bin, 'owned_match_branch_condition_mutating', "struct E {
	name string
}

fn delete_last(mut entries []E) int {
	entries.delete_last()
	return 1
}

fn last_length(mut entries []E) int {
	return match 1 {
		delete_last(mut entries) { entries.last().name.len }
		else { 0 }
	}
}

fn main() {
	mut entries := [E{ name: 'hello' }]
	println(last_length(mut entries))
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output

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
	safe_run := os.execute(os.quoted_path(safe_bin_path))
	assert safe_run.exit_code == 0, safe_run.output
	assert safe_run.output.trim_space() == '5', safe_run.output
}

fn test_owned_scalar_consumer_with_mutating_struct_default_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_mutating_struct_default_sibling', "struct E {
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

fn test_owned_scalar_consumer_with_cloning_method_value_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_cloning_method_value_sibling', "struct E {
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

fn consume(length int, callback fn () string) int {
	_ = callback()
	return length
}

fn last_length_with_bound_method(mutator Mutator) int {
	return consume(entries.last().name.len, mutator.run)
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_length_with_bound_method(Mutator{ label: 'run' }))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output

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
	safe_run := os.execute(os.quoted_path(safe_bin_path))
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
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true', run.output
}

fn test_owned_scalar_consumer_with_alias_clone_method_value_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_alias_clone_method_value_sibling', "struct E {
	name string
}

type Reader = string

__global entries []E

fn (reader Reader) clone() Reader {
	entries.delete_last()
	return Reader(string(reader).clone())
}

fn (reader Reader) run() string {
	return reader
}

fn consume(length int, callback fn () string) int {
	_ = callback()
	return length
}

fn last_length_with_bound_method(reader Reader) int {
	return consume(entries.last().name.len, reader.run)
}

fn main() {
	entries = [E{ name: 'hello' }]
	reader := Reader('run')
	println(last_length_with_bound_method(reader))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_scalar_consumer_with_cloning_array_spread_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_cloning_array_spread_sibling', "struct E {
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

fn consume(length int, mutators []Mutator) int {
	_ = mutators
	return length
}

fn last_length_with_spread(mutators []Mutator) int {
	return consume(entries.last().name.len, [...mutators])
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_length_with_spread([Mutator{ label: 'delete' }]))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_scalar_multi_return_with_mutating_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_scalar_multi_return_mutating_sibling', "struct E {
	name string
}

fn delete_last(mut entries []E) string {
	entries.delete_last()
	return 'deleted'
}

fn last_length_then_delete(mut entries []E) (int, string) {
	return entries.last().name.len, delete_last(mut entries)
}

fn main() {
	mut entries := [E{ name: 'hello' }]
	length, action := last_length_then_delete(mut entries)
	println(int_str(length) + ':' + action)
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_field_overloaded_index_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_field_overloaded_index', "struct Name {
	text string
}

struct E {
	name Name
}

fn (name Name) [] (index int) u8 {
	return name.text[index]
}

fn last_initial(arr []E) u8 {
	return arr.last().name[0]
}

fn main() {
	arr := [E{ name: Name{ text: 'hello' } }]
	println(last_initial(arr))
}
", '')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}

fn test_owned_string_comparison_with_overloaded_index_sibling_is_rejected() {
	v3_bin := build_v3_array_accessor_borrow_ownership() or { return }
	compile := compile_v3_ownership_program(v3_bin, 'owned_overloaded_index_sibling', "struct E {
	name string
}

struct Mutator {}

__global entries []E

fn (Mutator) [] (index int) string {
	_ = index
	entries.delete_last()
	return 'hello'
}

fn last_name_matches(mutator Mutator) bool {
	return entries.last().name == mutator[0]
}

fn main() {
	entries = [E{ name: 'hello' }]
	println(last_name_matches(Mutator{}))
}
", '-enable-globals')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return an independent array element'), compile.output
}
