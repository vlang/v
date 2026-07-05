import os

const generic_struct_str_vexe = @VEXE
const generic_struct_str_tests_dir = os.dir(@FILE)
const generic_struct_str_v3_dir = os.dir(generic_struct_str_tests_dir)
const generic_struct_str_vlib_dir = os.dir(generic_struct_str_v3_dir)
const generic_struct_str_v3_src = os.join_path(generic_struct_str_v3_dir, 'v3.v')

fn generic_struct_str_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_struct_str_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_struct_str_vexe} -gc none -path "${generic_struct_str_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_struct_str_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn generic_struct_str_write_project(name string, main_source string) string {
	root := os.join_path(os.temp_dir(), 'v3_generic_struct_str_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'gr')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'b')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_struct_str' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'gr/gr.v'), 'module gr

pub struct Inner[T] {
mut:
	items []T
}

pub fn (mut inner Inner[T]) push(item T) {
	inner.items << item
}

pub fn (inner Inner[T]) str() string {
	return "inner=" + inner.array().str()
}

pub fn (inner Inner[T]) array() []T {
	return inner.items
}

pub struct Outer[T] {
mut:
	inner Inner[T]
}

pub fn (mut outer Outer[T]) push(item T) {
	outer.inner.push(item)
}

pub fn (outer Outer[T]) str() string {
	return "outer=" + outer.inner.str()
}

pub struct Array_string {
pub:
	value string
}

pub struct CollisionBox[T] {
pub:
	items []T
}

pub fn (holder CollisionBox[T]) array() []T {
	return holder.items
}

pub fn (holder CollisionBox[T]) str() string {
	return holder.array().str()
}

pub struct FixedBox[T] {
mut:
	items []T
}

pub fn (mut holder FixedBox[T]) push(item T) {
	holder.items << item
}

pub fn (holder FixedBox[T]) str() string {
	return holder.items.str()
}

pub struct LinkList[T] {
mut:
	items []T
}

pub fn (mut list LinkList[T]) push(item T) {
	list.items << item
}

pub fn (list LinkList[T]) array() []T {
	return list.items
}

pub fn (list LinkList[T]) str() string {
	return list.array().str()
}

pub struct WrapperQueue[T] {
mut:
	elements LinkList[T]
}

pub fn (mut queue WrapperQueue[T]) push(item T) {
	queue.elements.push(item)
}

pub fn (queue WrapperQueue[T]) str() string {
	return queue.elements.str()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'a/a.v'), 'module a

pub struct Box[T] {
pub mut:
	items []T
}

pub fn (holder Box[T]) array() []T {
	return holder.items
}

pub fn (holder Box[T]) str() string {
	return holder.array().str()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'b/b.v'), 'module b

pub struct Box[T] {
pub mut:
	items []T
}

pub fn (holder Box[T]) array() []T {
	return holder.items
}

pub fn (holder Box[T]) str() string {
	return holder.array().str()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), main_source) or { panic(err) }
	return os.join_path(root, 'main.v')
}

fn generic_struct_str_compile_project(name string, main_source string) (string, string) {
	v3_bin := generic_struct_str_build_v3()
	main_path := generic_struct_str_write_project(name, main_source)
	out := os.join_path(os.temp_dir(), 'v3_generic_struct_str_${name}_out_${os.getpid()}')
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(out + '.c') or { panic(err) }
	return out, generated
}

fn generic_struct_str_c_fn(generated string, prefix string) string {
	mut offset := 0
	for offset < generated.len {
		haystack := generated[offset..]
		rel_start := haystack.index(prefix) or { return '' }
		start := offset + rel_start
		tail := generated[start + prefix.len..]
		semi := tail.index(';') or { -1 }
		brace := tail.index('{') or { -1 }
		if brace >= 0 && (semi < 0 || brace < semi) {
			rest := generated[start..]
			end := rest.index('\n}') or { return rest }
			return rest[..end + 2]
		}
		offset = start + prefix.len
	}
	return ''
}

fn test_generic_inner_nested_array_str_uses_array_iterator() {
	out, generated := generic_struct_str_compile_project('nested_array', 'module main

import gr

fn main() {
	mut inner := gr.Inner[[]string]{}
	inner.push([\'A\', \'B\'])
	inner.push([\'C\'])
	array_str := inner.array().str()
	interpolated := \'value \${inner}\'
	assert array_str == "[[\'A\', \'B\'], [\'C\']]"
	assert interpolated == \'value inner=\' + array_str
	assert interpolated == "value inner=[[\'A\', \'B\'], [\'C\']]"
	println(array_str)
}
')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == "[['A', 'B'], ['C']]"

	assert generated.contains('gr__Inner_Array_string__str'), generated
	assert generated.contains('Array __arr_str_it_'), generated
	assert !generated.contains('int __arr_str_it_'), generated
	assert !generated.contains('Inner_T__str'), generated
}

fn test_generic_struct_str_preserves_module_array_string_specialization() {
	out, generated := generic_struct_str_compile_project('array_string_collision', "module main

import gr

fn main() {
	holder := gr.CollisionBox[gr.Array_string]{
		items: [gr.Array_string{
			value: 'guard'
		}]
	}
	collision := holder.str()
	assert collision == '[gr.Array_string{}]'
	println(collision)
}
")
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '[gr.Array_string{}]'

	assert generated.contains('gr__CollisionBox_gr__Array_string__str'), generated
	assert generated.contains('gr__Array_string __arr_str_it_'), generated
	assert !generated.contains('gr__CollisionBox_Array_string__str'), generated
}

fn test_generic_struct_str_keeps_same_short_generic_bases_separate() {
	out, generated := generic_struct_str_compile_project('short_base_collision', 'module main

import a
import b

fn main() {
	left := a.Box[[]string]{
		items: [[\'A\'], [\'B\', \'C\']]
	}
	right := b.Box[int]{
		items: [1, 2]
	}
	left_text := left.str()
	right_text := right.str()
	assert left_text == "[[\'A\'], [\'B\', \'C\']]"
	assert right_text == "[1, 2]"
	println(left_text + "|" + right_text)
}
')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == "[['A'], ['B', 'C']]|[1, 2]"

	left_body := generic_struct_str_c_fn(generated, 'string a__Box_Array_string__str(')
	right_body := generic_struct_str_c_fn(generated, 'string b__Box_int__str(')
	assert left_body.len > 0, generated
	assert right_body.len > 0, generated
	assert left_body.contains('Array __arr_str_it_'), left_body
	assert !left_body.contains('int __arr_str_it_'), left_body
	assert right_body.contains('int __arr_str_it_'), right_body
	assert !right_body.contains('Array __arr_str_it_'), right_body
	assert !generated.contains('a__Box_int__str'), generated
	assert !generated.contains('b__Box_Array_string__str'), generated
}

fn test_generic_struct_str_does_not_decode_fixed_array_suffix_as_runtime_array() {
	out, generated := generic_struct_str_compile_project('fixed_array_suffix', 'module main

import gr

type Pair = [2]string

fn main() {
	first := [\'A\', \'B\']!
	second := [\'C\', \'D\']!
	mut holder := gr.FixedBox[Pair]{}
	holder.push(first)
	holder.push(second)
	text := holder.str()
	assert text == "[[\'A\', \'B\'], [\'C\', \'D\']]"
	println(text)
}
')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == "[['A', 'B'], ['C', 'D']]"

	body := generic_struct_str_c_fn(generated, 'string gr__FixedBox_string_2__str(')
	assert body.len > 0, generated
	assert body.contains('Array_fixed_string_2'), body
	assert body.contains('string __arr_str_it_'), body
	assert body.contains('[2]'), body
	assert !body.contains('Array __arr_str_it_'), body
	assert !generated.contains('gr__FixedBox_Array_string__str'), generated
}

fn test_generic_outer_embedded_inner_default_init_str() {
	out, generated := generic_struct_str_compile_project('embedded_default_init', 'module main

import gr

fn main() {
	mut outer := gr.Outer[[]string]{}
	outer.push([\'A\', \'B\'])
	outer.push([\'C\'])
	direct := outer.str()
	interpolated := \'value \${outer}\'
	assert direct == "outer=inner=[[\'A\', \'B\'], [\'C\']]"
	assert interpolated == \'value \' + direct
	assert interpolated == "value outer=inner=[[\'A\', \'B\'], [\'C\']]"
	println(direct)
}
')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == "outer=inner=[['A', 'B'], ['C']]"

	assert generated.contains('gr__Outer_Array_string__str'), generated
	assert generated.contains('gr__Inner_Array_string__str'), generated
	assert !generated.contains(' = outer;'), generated
	assert !generated.contains('Outer_T__str'), generated
	assert !generated.contains('Inner_T__str'), generated
}

fn test_generic_wrapper_list_str_preserves_nested_array_suffix() {
	out, generated := generic_struct_str_compile_project('wrapper_nested_array', 'module main

import gr

fn main() {
	mut queue := gr.WrapperQueue[[]string]{}
	queue.push([\'A\', \'B\'])
	queue.push([\'C\'])
	text := \'\${queue}\'
	assert text == "[[\'A\', \'B\'], [\'C\']]"
	println(text)
}
')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == "[['A', 'B'], ['C']]"

	body := generic_struct_str_c_fn(generated, 'string gr__LinkList_Array_string__str(')
	assert body.len > 0, generated
	assert generated.contains('gr__WrapperQueue_Array_string__str'), generated
	assert body.contains('Array __arr_str_it_'), body
	assert body.contains('string __arr_str_it_'), body
	assert !body.contains('int __arr_str_it_'), body
	assert !generated.contains('WrapperQueue_T__str'), generated
	assert !generated.contains('LinkList_T__str'), generated
}
