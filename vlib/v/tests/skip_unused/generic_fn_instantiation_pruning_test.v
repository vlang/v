import os

const vexe = @VEXE

fn test_skip_unused_prunes_unused_generic_fn_instantiations() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_26019')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_26019.v')
	source := [
		'module main',
		'',
		'fn new[T]() map[u8]T {',
		'\tx := map[u8]T{}',
		'\treturn x',
		'}',
		'',
		'fn not_used_fn() map[u8]int {',
		'\tx := new[int]()',
		'\treturn x',
		'}',
		'',
		'fn main() {',
		'\tx := new[u32]()',
		'\tdump(x)',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res := os.execute('${os.quoted_path(vexe)} -o - ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('VV_LOC Map_u8_u32 main__new_T_u32(void)')
	assert !res.output.contains('VV_LOC Map_u8_int main__new_T_int(void)')
}

fn test_skip_unused_keeps_generic_offsetof_struct_instantiations() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27283_generic_offsetof')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_27283_generic_offsetof.v')
	binary_path := os.join_path(tmp_dir, 'issue_27283_generic_offsetof')
	source := [
		'module main',
		'',
		'struct Box[T] {',
		'\ta int',
		'\tb T',
		'}',
		'',
		'fn off[T]() u32 {',
		'\treturn __offsetof(Box[T], b)',
		'}',
		'',
		'fn main() {',
		'\tprintln(off[int]())',
		'\tprintln(off[string]())',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res :=
		os.execute('${os.quoted_path(vexe)} -skip-unused -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
}

fn test_skip_unused_does_not_emit_impl_methods_for_interface_extensions() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_skip_unused_interface_extension_collision')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'interface_extension_collision.v')
	source := [
		'module main',
		'',
		'import crypto.internal.subtle',
		'',
		'interface Elem {}',
		'',
		'struct Thing {}',
		'',
		'struct Holder {',
		'\titems []Elem',
		'}',
		'',
		'fn (el Elem) equal(_ Elem) bool {',
		'\treturn true',
		'}',
		'',
		'fn (t Thing) equal(_ Thing) bool {',
		'\treturn subtle.constant_time_compare([u8(1)], [u8(1)]) == 1',
		'}',
		'',
		'fn (h Holder) ok() bool {',
		'\tfor i, item in h.items {',
		'\t\tfor j, obj in h.items {',
		'\t\t\tif i == j {',
		'\t\t\t\treturn item.equal(obj)',
		'\t\t\t}',
		'\t\t}',
		'\t}',
		'\treturn false',
		'}',
		'',
		'fn main() {',
		'\th := Holder{items: [Elem(Thing{}), Elem(Thing{})]}',
		'\tprintln(h.ok())',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res := os.execute('${os.quoted_path(vexe)} -o - ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('VV_LOC bool main__Elem_equal(')
	assert !res.output.contains('main__Thing_equal(')
}

fn test_skip_unused_keeps_json2_embedded_struct_decode_helpers() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_26928')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_26928.v')
	source := [
		'module main',
		'',
		'import time',
		'import json2',
		'',
		'struct Meta {',
		'\tcreated_at ?time.Time',
		'}',
		'',
		'struct Req {',
		'\tMeta',
		'\tname string',
		'}',
		'',
		'fn main() {',
		'\t_ := json2.decode[Req](\'{"name":"x"}\') or { panic(err) }',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res := os.execute('${os.quoted_path(vexe)} -w -o - ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('json2__decode_struct_key_T_main__Req')
	assert res.output.contains('json2__check_required_struct_fields_T_main__Req')
	// the `?time.Time` payload of the embedded `Meta` is decoded through a generic
	// instantiation reachable only via comptime `$for field` codegen; skip-unused
	// must keep it. Assert the payload decoder itself rather than a specific json2
	// helper name, so the test does not break when those helpers are refactored.
	assert res.output.contains('json2__Decoder_decode_value_T_time__Time')
}

fn test_skip_unused_marks_dependencies_inside_generic_anon_fns() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_generic_anon_fn_dependencies')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'generic_anon_fn_dependencies.v')
	binary_path := os.join_path(tmp_dir, 'generic_anon_fn_dependencies')
	source := [
		'module main',
		'',
		'struct Holder[T] {',
		'\tdata []T',
		'}',
		'',
		'fn (h &Holder[T]) nmap[T](others []&Holder[T], f fn ([]T) T) T {',
		'\treturn f([h.data[0], others[0].data[0]])',
		'}',
		'',
		'fn (a &Holder[T]) subtract[T](b &Holder[T]) T {',
		'\treturn a.nmap([b], fn [T] (xs []T) T {',
		'\t\tx := xs[0]',
		'\t\ty := xs[1]',
		'\t\t$if T is string {',
		"\t\t\treturn x.replace(y, '')",
		'\t\t} $else {',
		'\t\t\treturn x - y',
		'\t\t}',
		'\t})',
		'}',
		'',
		'fn unused_string() {',
		"\ta := &Holder[string]{data: ['abc']}",
		"\tb := &Holder[string]{data: ['b']}",
		'\tprintln(a.subtract(b))',
		'}',
		'',
		'fn main() {',
		'\ta := &Holder[int]{data: [1]}',
		'\tb := &Holder[int]{data: [2]}',
		'\tprintln(a.subtract(b))',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res :=
		os.execute('${os.quoted_path(vexe)} -d no_backtrace -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
}

fn test_skip_unused_keeps_generic_next_for_for_in_when_direct_calls_use_other_types() {
	// Regression for https://github.com/vlang/v/issues/27147 .
	// A `for in` over a generic iterator instantiation (e.g. `Range[big.Integer]`)
	// must emit the matching `next` instantiation, even when other code calls
	// methods on a different instantiation of the same generic struct directly
	// (e.g. `iter.next()` on a `Range[int]`).
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27147')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_27147.v')
	binary_path := os.join_path(tmp_dir, 'issue_27147')
	source := [
		'module main',
		'',
		'import math.big',
		'',
		'struct Range[T] {',
		'\tstart T',
		'\tend   T',
		'\tstep  T',
		'mut:',
		'\tcur T',
		'}',
		'',
		'pub fn (mut r Range[T]) next() ?T {',
		'\tif r.cur > r.end {',
		'\t\treturn none',
		'\t}',
		'\tdefer { r.cur += r.step }',
		'\treturn r.cur',
		'}',
		'',
		'pub fn (mut r Range[T]) reset() {',
		'\tr.cur = r.start',
		'}',
		'',
		'pub fn range[T](start T, end T, step T) Range[T] {',
		'\treturn Range[T]{start: start, end: end, step: step, cur: start}',
		'}',
		'',
		'fn main() {',
		'\tmut iter := range(0, 5, 1)',
		'\titer.reset()',
		'\titer.next()',
		'\tend := big.integer_from_int(3)',
		'\tfor i in range[big.Integer](big.zero_int, end, big.one_int) {',
		'\t\tprintln(i)',
		'\t}',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
}
