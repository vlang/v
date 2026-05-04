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
		'import x.json2',
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
	assert res.output.contains('x__json2__decode_struct_key_T_main__Req')
	assert res.output.contains('x__json2__check_required_struct_fields_T_main__Req')
	assert res.output.contains('x__json2__create_value_from_optional_T_time__Time')
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
