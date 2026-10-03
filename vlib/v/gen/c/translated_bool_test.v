module c

import os
import v.flat
import v.token
import v.types

fn translated_bool_test_node(mut g FlatGen, kind flat.NodeKind, value string, typ types.Type) flat.NodeId {
	id := g.a.add_node(flat.Node{
		kind:  kind
		value: value
		pos:   token.new_pos(1, 0)
	})
	g.tc.register_synth_type(id, typ)
	return id
}

fn test_translated_bool_conversions_with_unsigned_char_storage() {
	cc := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('cc') or { return }
	}
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('translated_bool.v', 1)
	mut tc := types.TypeChecker.new(&a)
	tc.translated_files['translated_bool.v'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	bool_type := types.Type(types.bool_)
	alias_type := types.Type(types.Alias{ name: 'Flag', base_type: bool_type })
	wide := translated_bool_test_node(mut g, .int_literal, '256', types.Type(types.int_))
	fraction := translated_bool_test_node(mut g, .float_literal, '0.5', types.Type(types.f64_))
	zero := translated_bool_test_node(mut g, .int_literal, '0', types.Type(types.int_))
	one := translated_bool_test_node(mut g, .int_literal, '1', types.Type(types.int_))
	value := translated_bool_test_node(mut g, .ident, 'value', bool_type)
	g.write('typedef unsigned char bool; typedef int i32; typedef long long i64; typedef unsigned long long u64;\n')
	g.write('static bool initial = ')
	g.write(g.global_scalar_static_initializer(wide, bool_type) or { panic('expected static initializer') })
	g.write(';\nint main(void) { bool value; if (initial != 1) return 1;\n')
	for i, id in [wide, fraction, zero] {
		g.write('value = ')
		g.gen_expr_with_expected_type(id, alias_type)
		expected := if id == zero { 0 } else { 1 }
		g.write('; if (value != ${expected}) return ${i + 2};\n')
	}
	g.write('value = 0; value = ')
	g.gen_translated_numeric_compound_value('value', value, wide, bool_type, types.Type(types.int_), '+')
	g.write('; if (value != 1) return 5;\nvalue = 1; value = ')
	g.gen_translated_numeric_compound_value('value', value, fraction, bool_type, types.Type(types.f64_), '*')
	g.write('; if (value != 1) return 6;\nvalue = ')
	g.gen_compound_shift_value('value', value, one, bool_type, .left_shift)
	g.write('; if (value != 1) return 7;\nbool old = ')
	assert g.gen_checked_integer_postfix(value, .inc)
	g.write('; if (old != 1 || value != 1) return 8;\nvalue = 0; old = ')
	assert g.gen_checked_integer_postfix(value, .dec)
	g.write('; if (old != 0 || value != 1) return 9;\nreturn 0; }\n')
	dir := os.join_path(os.vtmp_dir(), 'translated_bool_storage_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c')
	binary := os.join_path(dir, 'main')
	os.write_file(source, g.sb.str())!
	build := os.exec([cc, '-std=gnu11', source, '-o', binary])
	assert build.exit_code == 0, build.output
	run := os.exec([binary])
	assert run.exit_code == 0, 'exit ${run.exit_code}: ${run.output}'
}
