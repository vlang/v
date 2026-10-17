module c

import os
import v.cmdexec
import v.flat
import v.types

fn formatted_enum_interp_c_expr(format string) string {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.int_literal, '5')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_formatted_string_interp_child_expr(value_id, types.Enum{
		name_id: flat.source_type_id('Color')
	}, format)
	return g.sb.str()
}

fn formatted_u8_interp_c_expr(format string) string {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.int_literal, '102')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_formatted_string_interp_child_expr(value_id, types.Type(types.u8_), format)
	return g.sb.str()
}

fn test_voidptr_function_value_detection_prefers_explicit_node_type() {
	mut a := flat.FlatAst.new()
	stale_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'callback'
		typ:   flat.source_type_id('fn ()')
	})
	mut tc := types.TypeChecker.new(&a)
	tc.structs['gg.WindowReadbackId'] = []types.StructField{}
	tc.struct_modules['gg.WindowReadbackId'] = 'gg'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	value_node := flat.Node{
		kind:  .ident
		value: 'request'
		typ:   flat.source_type_id('gg.WindowReadbackId')
	}
	assert type_is_fn_value(g.usable_expr_type(stale_id))
	assert !g.node_is_fn_value_for_voidptr(stale_id, value_node)
}

fn test_cached_string_literal_table_preserves_explicit_initializers() {
	mut g := FlatGen.new()
	g.cache_stable_symbols = true
	g.intern_string('literal')
	g.string_literals()
	assert g.sb.str() == 'static const string _str_0 = {"literal", 7, 1};\n\n'
}

fn test_string_literal_table_uses_declaration_macros_in_regular_and_split_c_output() {
	for split in [false, true] {
		mut g := FlatGen.new()
		g.cache_split = split
		g.intern_string('literal')
		g.string_literals()
		assert g.sb.str() == '#define _SL(id, s, n) static const string id = {s, n, 1}\n_SL(_str_0, "literal", 7);\n#undef _SL\n\n'
	}
}

fn test_cross_string_literal_declarations_preserve_byte_lengths() {
	for ccompiler in ['clang', 'gcc', 'tcc', 'msvc'] {
		mut g := FlatGen.new()
		g.ccompiler = ccompiler
		g.set_output_cross_c(true)
		g.intern_string('a\x00b')
		g.string_literals()
		assert g.sb.str() == '#define _SL(id, s, n) static const string id = {s, n, 1}\n_SL(_str_0, "a\\000b", 3);\n#undef _SL\n\n'
	}
}

fn test_long_string_literal_table_entries_split_without_changing_bytes() {
	// The compiler's embedded C prelude exceeds MSVC's per-literal limit.
	value := ('prelude\\\"\n\t\r\x00\x7f' + 'x'.repeat(501)).repeat(40)
	for stable in [false, true] {
		for split in [false, true] {
			mut g := FlatGen.new()
			g.cache_stable_symbols = stable
			g.cache_split = split
			g.ccompiler = 'msvc'
			g.intern_string(value)
			g.string_literals()
			output := g.sb.str()
			assert output.contains('"\n"')
			for line in output.split_into_lines() {
				assert line.len < 4095
			}
			joined := output.replace('"\n"', '')
			if stable {
				assert joined == 'static const string _str_0 = {"${c_escape(value)}", ${value.len}, 1};\n\n'
			} else {
				assert joined == '#define _SL(id, s, n) static const string id = {s, n, 1}\n_SL(_str_0, "${c_escape(value)}", ${value.len});\n#undef _SL\n\n'
			}
		}
	}
}

fn test_string_literal_macro_does_not_expand_c_function_names() {
	root := os.join_path(os.vtmp_dir(), 'string_literal_c_function_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	header := os.join_path(root, 'binding.h')
	binary := os.join_path(root, 'main')
	os.write_file(header, 'static inline int _SL(void) { return 42; }\n')!
	os.write_file(source, 'module main
#include "${header.replace('\\', '/')}"
fn C._SL() int
fn main() {
	result := C._SL()
	println("binding: \${result}")
}
')!
	for mode in [([]string{}), ['-no-parallel']] {
		mut args := ['-new-compiler', '-nocache', '-gc', 'none', '-cc', 'clang', '-no-retry-compilation',
			'-o', binary]
		args << mode
		args << source
		compiled := cmdexec.run(@VEXE, args)
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		run := cmdexec.run(binary, [])
		assert run.exit_code == 0, run.output
		assert run.output == 'binding: 42\n', run.output
	}
}

fn test_string_literal_ids_do_not_expand_c_struct_members() {
	root := os.join_path(os.vtmp_dir(), 'string_literal_members_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	binary := os.join_path(root, 'main')
	os.write_file(source, 'module main
#include "@VEXEROOT/vlib/v/gen/c/testdata/string_literal_members.h"
@[typedef]
struct C.LiteralMembers {
mut:
	_str_0 int
	_str_1 int
}
fn main() {
	mut value := C.LiteralMembers{_str_0: 7, _str_1: 11}
	value._str_0 += 1
	println("member \${value._str_0}, \${value._str_1}")
}
')!
	mut modes := [['-keepc'], ['-nocache'], ['-nocache', '-no-parallel']]
	$if bsd || linux {
		modes << ['-nocache', '-parallel-cc']
	}
	for mode in modes {
		mut args := ['-new-compiler', '-gc', 'none', '-cc', 'clang', '-no-retry-compilation', '-showcc',
			'-o', binary]
		args << mode
		args << source
		compiled := cmdexec.run(@VEXE, args)
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		if '-parallel-cc' in mode {
			assert compiled.output.contains('unit_0.c'), compiled.output
			assert compiled.output.contains('unit_1.c'), compiled.output
		}
		run := cmdexec.run(binary, [])
		assert run.exit_code == 0, run.output
		assert run.output == 'member 8, 11\n', run.output
	}
}

fn test_character_interpolation_uses_rune_text() {
	assert formatted_u8_interp_c_expr('1c') == 'rune__str((u32)(102))'
	assert formatted_u8_interp_c_expr('3c') == 'v3_string_pad(rune__str((u32)(102)), 3, 0)'
}

fn test_character_interpolation_unwraps_integer_alias() {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.int_literal, '65')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_formatted_string_interp_child_expr(value_id, types.Alias{
		name_id:   flat.source_type_id('Code')
		base_type: types.Type(types.u8_)
	}, 'c')
	assert g.sb.str() == 'rune__str((u32)(65))'
}

fn test_width_only_enum_interpolation_uses_enum_text() {
	assert formatted_enum_interp_c_expr('8') == 'v3_string_pad(Color__autostr(5), 8, 0)'
	assert formatted_enum_interp_c_expr('-8') == 'v3_string_pad(Color__autostr(5), 8, 1)'
	assert formatted_enum_interp_c_expr('08') == 'v3_string_zpad(Color__autostr(5), 8)'
	assert formatted_enum_interp_c_expr('8d') == 'v3_string_pad(i64__str((i64)(5)), 8, 0)'
}

fn test_ierror_interpolation_uses_dynamic_message_dispatch() {
	mut a := flat.FlatAst.new()
	err_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'err'
		typ:   flat.source_type_id('IError')
	})
	a.children << err_id
	interp_id := a.add_node(flat.Node{
		kind:           .string_interp
		children_start: 0
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(err_id, types.Type(types.Interface{
		name_id: flat.source_type_id('IError')
	}))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.gen_string_interp(a.nodes[int(interp_id)])
	assert g.sb.str() == 'string_plus_many(1, (string[1]){({ IError _ierror_msg0 = err; IError__msg(&_ierror_msg0); })})'
}

fn test_lowered_string_join_keeps_source_identifiers_out_of_its_temporary_namespace() {
	mut a := flat.FlatAst.new()
	name := '__v3_internal_symbol_join_0'
	part := a.add_node(flat.Node{
		kind:  .ident
		value: name
		typ:   flat.source_type_string
	})
	a.children << part
	a.children << part
	mut tc := types.TypeChecker.new(&a)
	tc.cur_scope.insert(name, types.Type(types.string_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.gen_string_interp(flat.Node{
		kind:           .string_interp
		value:          '__v3_string_join'
		typ:            flat.source_type_string
		children_count: 2
	})
	output := g.sb.str()
	assert output.contains('string ${name}[2];')
	assert output.contains('${name}[0] = ${g.cname(name)};')
	assert output.contains('${name}[1] = ${g.cname(name)};')
	assert g.cname(name) != name
	assert output.contains('string_plus_many(2, ${name})')
}
