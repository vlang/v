module c

import os
import v.cmdexec
import v.flat
import v.token
import v.types

fn cgen_attribute_test_gen() &FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	return &g
}

fn test_noinline_attribute_is_preserved_for_generic_specialization() {
	mut g := cgen_attribute_test_gen()
	g.ccompiler = 'clang'
	source_pos := token.new_span(1, 20, 40)
	template_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'helper'
		pos:   source_pos
	})
	specialization_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'helper_T_int'
		pos:   source_pos
	})
	g.a.specialized_fn_nodes[int(specialization_id)] = true
	g.decl_attrs[int(template_id)] = ['noinline']
	g.decl_attrs_by_source_position[flat_fn_source_position_key(g.a.nodes[int(template_id)])] = [
		'noinline',
	]

	assert g.fn_decl_c_attribute(template_id) == ''
	assert g.fn_decl_c_attribute(specialization_id) == ''
	assert g.fn_decl_inlining_prefix(template_id) == '__attribute__((noinline)) '
	assert g.fn_decl_inlining_prefix(specialization_id) == '__attribute__((noinline)) '

	g.ccompiler = 'msvc'
	assert g.fn_decl_c_attribute(specialization_id) == ''
	assert g.fn_decl_inlining_prefix(specialization_id) == '__declspec(noinline) '
}

fn test_inline_hint_preserves_external_linkage_and_specialization_attributes() {
	mut g := cgen_attribute_test_gen()
	g.ccompiler = 'clang'
	pos := token.new_span(1, 20, 40)
	source := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'helper', pos: pos })
	specialized := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'helper_T_int', pos: pos })
	g.a.specialized_fn_nodes[int(specialized)] = true
	g.decl_attrs[int(source)] = ['inline']
	g.decl_attrs_by_source_position[flat_fn_source_position_key(g.a.nodes[int(source)])] = [
		'inline',
	]
	assert g.fn_decl_inlining_prefix(source) == 'inline '
	assert g.fn_decl_inlining_prefix(specialized) == 'inline '
	g.ccompiler = 'msvc'
	assert g.fn_decl_inlining_prefix(specialized) == '__inline '
	g.decl_attrs[int(source)] = ['inline', 'noinline']
	assert g.fn_decl_inlining_prefix(source) == '__declspec(noinline) '
}

fn test_naked_attribute_is_emitted_for_every_c_compiler_but_msvc() {
	mut g := cgen_attribute_test_gen()
	g.ccompiler = 'gcc'
	pos := token.new_span(1, 20, 40)
	naked := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'naked_body', pos: pos })
	ordinary := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'ordinary_body', pos: pos })
	g.decl_attrs[int(naked)] = ['_naked']

	assert g.fn_decl_naked_prefix(naked) == '__attribute__((naked)) '
	assert g.fn_decl_naked_prefix(ordinary) == ''
	// Body generation tracks the naked prefix separately from lifecycle attributes.
	assert g.fn_decl_c_attribute(naked) == ''

	g.ccompiler = 'clang'
	assert g.fn_decl_naked_prefix(naked) == '__attribute__((naked)) '
	// MSVC spells this `__declspec(naked)` and supports it on x86 only, and it has
	// no inline assembly for a naked body to hold in the first place.
	g.ccompiler = 'msvc'
	assert g.fn_decl_naked_prefix(naked) == ''
}

fn test_constructor_and_destructor_attributes_precede_function_declarators() {
	mut g := cgen_attribute_test_gen()
	pos := token.new_span(1, 20, 40)
	source := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'hook', pos: pos })
	specialized := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'hook_T_int', pos: pos })
	g.a.specialized_fn_nodes[int(specialized)] = true
	for attrs in [['_constructor'], ['_destructor'], ['_constructor', '_destructor']] {
		g.decl_attrs[int(source)] = attrs
		g.decl_attrs_by_source_position[flat_fn_source_position_key(g.a.nodes[int(source)])] = attrs
		expected := '__attribute__((${attrs.map(it.trim_left('_')).join(', ')})) '
		for compiler in ['gcc', 'clang', 'tcc'] {
			g.ccompiler = compiler
			assert g.fn_decl_c_attribute(source) == expected
			assert g.fn_decl_c_attribute(specialized) == expected
			assert g.fn_decl_c_attribute(flat.empty_node) == ''
		}
		g.ccompiler = 'msvc'
		assert g.fn_decl_c_attribute(source) == ''
		assert g.fn_decl_c_attribute(specialized) == ''
	}
}

fn test_exported_constructor_and_destructor_compile_and_run() {
	root := os.join_path(os.vtmp_dir(), 'cgen_lifecycle_attributes_${os.getpid()}')
	module_dir := os.join_path(root, 'policyhost')
	os.mkdir_all(module_dir)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'import policyhost
fn main() { policyhost.run() }
')!
	os.write_file(os.join_path(module_dir, 'core.c.v'), "module policyhost
fn C.puts(&char) int
@[export: 'remember_environment'; _constructor]
fn remember_caller_environment() { C.puts(c'constructor') }
@[export: 'release_environment'; _destructor]
fn release_caller_environment() { C.puts(c'destructor') }
pub fn run() { C.puts(c'main') }
")!
	generated := cmdexec.run(@VEXE, ['-new-compiler', '-gc', 'none', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	assert source.contains('__attribute__((constructor)) void policyhost__remember_caller_environment(void) {')
	assert source.contains('__attribute__((destructor)) void policyhost__release_caller_environment(void) {')
	assert !source.contains('(void) __attribute__((constructor)) {')
	assert !source.contains('(void) __attribute__((destructor)) {')
	compiler := os.find_abs_path_of_executable('gcc') or {
		os.find_abs_path_of_executable('clang') or { return }
	}
	executed := cmdexec.run(@VEXE, ['-new-compiler', '-gc', 'none', '-cc', compiler, 'run',
		source_path])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'constructor\nmain\ndestructor\n'
}
