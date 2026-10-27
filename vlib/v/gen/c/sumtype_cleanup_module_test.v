module c

import v.flat
import v.types
import os
import v.cmdexec
import v.gen.c.naming { sum_field_name }

fn test_direct_packed_sum_cleanup_evaluates_storage_once_and_copies_back() ! {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set('Choice', ['int', 'string', '&string'])
	tc.type_aliases.set('AliasChoice', 'Choice')
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	sum_id := flat.source_type_id('Choice')
	g.recursive_drop_helpers[ownership_drop_expansion_key(tc.parse_type(sum_id))] = sum_id
	g.gen_ownership_recursive_drop_helper_forward_decls()
	g.gen_ownership_recursive_drop_helpers()
	helpers := g.sb.str()
	direct := g.ownership_drop_value_to_string(tc.parse_type(sum_id), 'outer.items[next_index()].choice')
	alias_drop := g.ownership_drop_value_to_string(tc.parse_type(flat.source_type_id('AliasChoice')), 'outer.items[next_index()].choice')
	wrapped := g.ownership_drop_value_to_string(tc.parse_type(flat.source_type_id('?Choice')), 'wrapped.choice')
	assert direct.count('next_index()') == 1, direct
	assert alias_drop.count('next_index()') == 1, alias_drop
	cc := os.find_abs_path_of_executable('clang') or { return }
	sum_ct := g.value_c_type(tc.parse_type(sum_id))
	pointer_field := sum_field_name('&string')
	pointer_tag := g.sum_type_index(sum_id, flat.source_type_id('&string'))
	dir := os.join_path(os.vtmp_dir(), 'direct_packed_sum_cleanup_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c')
	binary := os.join_path(dir, 'main')
	os.write_file(source, '#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
typedef int IError;
typedef struct { char* str; int len; bool is_lit; } string;
typedef struct { int typ; bool _pointer_variant_is_owned; union {
 long long _int; string _string; string* ${pointer_field};
}; } ${sum_ct};
#pragma pack(push, 1)
typedef struct { unsigned char prefix; ${sum_ct} choice; unsigned char suffix; } Packed;
typedef struct { unsigned char prefix; Packed items[5]; } Outer;
typedef struct { bool ok; ${sum_ct} value; } OptionalChoice;
typedef struct { unsigned char prefix; OptionalChoice choice; unsigned char suffix; } Wrapped;
#pragma pack(pop)
static int index_calls;
static int string_frees;
static int box_frees;
static int next_index(void) { return index_calls++; }
static void string__free(string* value) {
 string_frees++; value->str = NULL; value->len = 0;
}
static void release_box(void* value) { box_frees++; free(value); }
static void v_free(void* value) { release_box(value); }
#define free release_box
${helpers}
#undef free
int main(void) {
 Outer outer;
 memset(&outer, 0, sizeof(outer));
 string borrowed = { "borrowed", 8, true };
 string* owned = malloc(sizeof(string));
 *owned = (string){ "owned", 5, true };
 outer.prefix = 19;
 for (int i = 0; i < 5; i++) {
  outer.items[i].prefix = 31; outer.items[i].suffix = 47;
 }
 outer.items[0].choice.typ = 1;
 outer.items[0].choice._int = 17;
 outer.items[1].choice.typ = 2;
 outer.items[1].choice._string = (string){ "text", 4, true };
 outer.items[2].choice.typ = ${pointer_tag};
 outer.items[2].choice.${pointer_field} = &borrowed;
 outer.items[3].choice.typ = ${pointer_tag};
 outer.items[3].choice.${pointer_field} = owned;
 outer.items[4].choice.typ = ${pointer_tag};
 outer.items[4].choice._pointer_variant_is_owned = true;
 outer.items[4].choice.${pointer_field} = owned;
 { ${direct} }
 assert(index_calls == 1 && string_frees == 0 && box_frees == 0);
 assert(outer.items[0].choice._int == 17);
 { ${alias_drop} }
 assert(index_calls == 2 && string_frees == 1 && box_frees == 0);
 assert(outer.items[1].choice._string.len == 0);
 assert(outer.items[1].choice._string.str == NULL);
 { ${direct} }
 assert(index_calls == 3 && string_frees == 1 && box_frees == 0);
 assert(outer.items[2].choice.${pointer_field} == &borrowed);
 assert(!outer.items[2].choice._pointer_variant_is_owned && borrowed.len == 8);
 { ${alias_drop} }
 assert(index_calls == 4 && string_frees == 1 && box_frees == 0);
 assert(outer.items[3].choice.${pointer_field} == owned);
 assert(!outer.items[3].choice._pointer_variant_is_owned && owned->len == 5);
 { ${direct} }
 assert(index_calls == 5 && string_frees == 2 && box_frees == 1);
 assert(outer.items[4].choice._pointer_variant_is_owned);
 Wrapped wrapped;
 memset(&wrapped, 0, sizeof(wrapped));
 wrapped.prefix = 53; wrapped.suffix = 59;
 wrapped.choice.ok = true;
 wrapped.choice.value.typ = 2;
 wrapped.choice.value._string = (string){ "wrapped", 7, true };
 { ${wrapped} }
 assert(string_frees == 3 && box_frees == 1);
 assert(wrapped.choice.ok && wrapped.choice.value._string.len == 0);
 assert(wrapped.choice.value._string.str == NULL);
 assert(wrapped.prefix == 53 && wrapped.suffix == 59);
 assert(outer.prefix == 19);
 for (int i = 0; i < 5; i++) {
  assert(outer.items[i].prefix == 31 && outer.items[i].suffix == 47);
 }
 return 0;
}
')!
	build := cmdexec.run(cc, ['-std=c11', '-fsanitize=alignment', '-fsanitize-trap=alignment',
		'-o', binary, source])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(binary, []string{})
	assert run.exit_code == 0, '${run.output}\n${direct}\n${alias_drop}\n${wrapped}'
}

fn test_packed_sum_cleanup_preserves_alignment_and_updates_storage() ! {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set('Choice', ['int', 'string'])
	tc.structs['Packed'] = [types.StructField{
		name: 'prefix'
		typ:  tc.parse_type(flat.source_type_id('u8'))
	}, types.StructField{
		name: 'choice'
		typ:  tc.parse_type(flat.source_type_id('Choice'))
	}]
	node := a.add_node(flat.Node{ kind: .struct_decl, value: 'Packed' })
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.register_struct_decl_info_at(int(node), 'Packed', 'Packed', 'main', 'main.v', a.nodes[int(node)])
	g.decl_attrs[int(node)] = ['packed']
	sum_id := flat.source_type_id('Choice')
	g.recursive_drop_helpers[ownership_drop_expansion_key(tc.parse_type(sum_id))] = sum_id
	g.gen_ownership_recursive_drop_helper_forward_decls()
	g.gen_ownership_recursive_drop_helpers()
	helpers := g.sb.str()
	cleanup := g.ownership_drop_value_to_string(tc.parse_type(flat.source_type_id('[2]Packed')), 'items')
	assert !cleanup.contains('__v3_ownership_drop_Choice(&(((items)'), cleanup
	cc := os.find_abs_path_of_executable('clang') or { return }
	packed_ct := g.value_c_type(tc.parse_type(flat.source_type_id('Packed')))
	sum_ct := g.value_c_type(tc.parse_type(sum_id))
	dir := os.join_path(os.vtmp_dir(), 'packed_sum_cleanup_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c')
	binary := os.join_path(dir, 'main')
	os.write_file(source, '#include <assert.h>
#include <stdbool.h>
#include <string.h>
typedef int IError;
typedef struct { char* str; int len; bool is_lit; } string;
typedef struct { int typ; union { long long _int; string _string; }; } ${sum_ct};
#pragma pack(push, 1)
typedef struct { unsigned char prefix; ${sum_ct} choice; } ${packed_ct};
#pragma pack(pop)
static void string__free(string* value) { value->str = NULL; value->len = 0; }
${helpers}
int main(void) {
 ${packed_ct} items[2];
 memset(items, 0, sizeof(items));
 items[0].prefix = 1;
 items[0].choice.typ = 1;
 items[0].choice._int = 17;
 items[1].prefix = 2;
 items[1].choice.typ = 2;
 items[1].choice._string.str = "text";
 items[1].choice._string.len = 4;
 items[1].choice._string.is_lit = true;
 ${cleanup}
 assert(items[0].prefix == 1 && items[0].choice._int == 17);
 assert(items[1].prefix == 2 && items[1].choice._string.len == 0);
 return 0;
}
')!
	build := cmdexec.run(cc, ['-std=c11', '-fsanitize=alignment', '-fsanitize-trap=alignment',
		'-o', binary, source])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(binary, []string{})
	assert run.exit_code == 0, '${run.output}\n${cleanup}'
}

fn test_sum_cleanup_helpers_use_declaration_module_for_autofree_methods() {
	for module_name in ['', 'model'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		sum_name := if module_name == '' { 'Choice' } else { '${module_name}.Choice' }
		method_name := if module_name == '' { 'Item.free' } else { '${module_name}.Item.free' }
		tc.sum_types.set(sum_name, ['int', '&Item'])
		tc.structs['Item'] = []types.StructField{}
		tc.fn_ret_types[method_name] = types.Type(types.void_)
		tc.autofree_mode = true
		tc.cur_module = 'builtin'
		tc.cur_file = 'builtin.v'
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		sum_id := flat.source_type_id(sum_name)
		g.recursive_drop_helpers[ownership_drop_expansion_key(tc.parse_type(sum_id))] = sum_id
		g.gen_ownership_recursive_drop_helpers()
		generated := g.sb.str()
		method_cname := if module_name == '' { 'main__Item_free' } else { 'model__Item__free' }
		assert generated.contains('${method_cname}('), generated
		assert tc.cur_module == 'builtin'
		assert tc.cur_file == 'builtin.v'
	}
}

fn test_sum_cleanup_helper_preserves_canonical_root_despite_file_aliases() {
	check_canonical_sum_cleanup('foo.Choice')
}

fn test_sum_cleanup_helper_preserves_canonical_variant_despite_file_aliases() {
	check_canonical_sum_cleanup('Choice')
}

fn check_canonical_sum_cleanup(sum_name string) {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'ambient.v')
	a.add_val(.module_decl, 'main')
	a.add_node(flat.Node{
		kind:  .import_decl
		value: 'x.foo'
		typ:   flat.source_type_id('foo')
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.sum_types.set(sum_name, ['int', '&foo.Item'])
	tc.sum_types.set('x.foo.Choice', ['bool', 'f64'])
	tc.structs['foo.Item'] = []types.StructField{}
	tc.structs['x.foo.Item'] = []types.StructField{}
	tc.fn_ret_types['foo.Item.free'] = types.Type(types.void_)
	tc.fn_ret_types['x.foo.Item.free'] = types.Type(types.void_)
	tc.autofree_mode = true
	tc.cur_module = 'builtin'
	tc.cur_file = 'ambient.v'
	assert tc.parse_type(flat.source_type_id('foo.Choice')).name() == 'x.foo.Choice'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	sum_id := flat.source_type_id(sum_name)
	g.recursive_drop_helpers[ownership_drop_expansion_key(tc.parse_canonical_type(sum_id))] = sum_id
	g.gen_ownership_recursive_drop_helpers()
	generated := g.sb.str()
	assert generated.contains('foo__Item__free('), generated
	assert !generated.contains('x__foo__Item__free('), generated
	assert tc.cur_module == 'builtin'
	assert tc.cur_file == 'ambient.v'
	tc.autofree_mode = false
	mut names := map[u32]bool{}
	mut seen := map[u32]bool{}
	g.ownership_collect_drop_struct_names(tc.parse_canonical_type(sum_id), 0, mut names, mut seen)
	assert names[flat.source_type_id('foo.Item')]
	assert !names[flat.source_type_id('x.foo.Item')]
}

fn test_synthetic_sum_cleanup_preserves_canonical_type_despite_file_aliases() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'ambient.v')
	a.add_val(.module_decl, 'main')
	a.add_node(flat.Node{
		kind:  .import_decl
		value: 'x.foo'
		typ:   flat.source_type_id('foo')
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.sum_types.set('foo.Choice', ['int', '&foo.Item'])
	tc.sum_types.set('x.foo.Choice', ['bool', 'f64'])
	tc.structs['foo.Item'] = []types.StructField{}
	tc.cur_module = 'builtin'
	tc.cur_file = 'ambient.v'
	sum_id := flat.source_type_id('foo.Choice')
	callee := a.add_val(.ident, 'drop_owned')
	arg := a.add_node(flat.Node{
		kind:  .ident
		value: 'value'
		typ:   sum_id
	})
	start := a.children.len
	a.children << callee
	a.children << arg
	call := a.add_node(flat.Node{
		kind:           .call
		children_start: i32(start)
		children_count: 2
	})
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	mut names := map[u32]bool{}
	mut visited := map[int]bool{}
	g.ownership_collect_synthetic_drop_types(call, mut names, mut visited)
	assert names[sum_id]
	assert !names[flat.source_type_id('x.foo.Choice')]
	assert names[flat.source_type_id('foo.Item')]
}

fn test_sum_cleanup_helper_uses_sum_c_type_for_fixed_array_generic_argument() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	sum_id := flat.source_type_id('Box[[2]string]')
	tc.sum_types.set('Box[[2]string]', ['[2]string', 'int'])
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.recursive_drop_helpers[ownership_drop_expansion_key(tc.parse_canonical_type(sum_id))] = sum_id
	g.gen_ownership_recursive_drop_helper_forward_decls()
	g.gen_ownership_recursive_drop_helpers()
	generated := g.sb.str()
	sum_c_type := g.cname('Box[[2]string]')
	assert sum_c_type != g.struct_cname('Box[[2]string]')
	assert generated.count('(${sum_c_type}* _value)') == 2, generated
}
