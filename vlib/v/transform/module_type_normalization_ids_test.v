module transform

import v.flat
import v.types

fn test_module_type_normalization_ids_preserve_trimming_and_grammar() {
	t := Transformer{}
	cases := {
		'':                                              ''
		' \t\n':                                         ''
		'  Item\t':                                      'Item'
		'  fn (Item  ':                                  'fn (Item'
		'  map[Item  ':                                  'map[Item'
		'  [3]  ':                                       '[3]'
		'  Box[Item  ':                                  'Box[Item'
		' & Item  ':                                     '&Item'
		' mut Item  ':                                   '&Item'
		' shared Item ':                                 'shared Item'
		' atomic Item ':                                 'atomic Item'
		' ? Item ':                                      '?Item'
		' ! Item ':                                      '!Item'
		' ... Item ':                                    '...Item'
		' [] Item ':                                     '[]Item'
		' chan Item ':                                   'chan Item'
		' map[ string ] & Item ':                        'map[string]&Item'
		' [3] & Item ':                                  '[3]&Item'
		' Box[ Item, [] Item ] ':                        'Box[Item, []Item]'
		' fn ( mut item Item, values [] Item ) ! Item ': 'fn (&Item, []Item) !Item'
		' fn ( Item ) ':                                 'fn (Item)'
		' fn () void ':                                  'fn () void'
	}
	for input, expected in cases {
		input_id := flat.source_type_id(input)
		expected_id := flat.source_type_id(expected)
		assert t.normalize_type_in_module_uncached(input_id, 'main') == expected_id
		assert t.normalize_type_in_module(input_id, 'main') == expected_id
	}
	for input in ['Item', 'pkg.Item', 'int', 'fn (Item)', 'map[Item', 'Box[Item'] {
		input_id := flat.source_type_id(input)
		assert t.normalize_type_in_module_uncached(input_id, 'main') == input_id
	}
}

fn test_module_type_normalization_ids_keep_selective_import_fallbacks() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('alpha.Item')] = []types.StructField{}
	tc.structs[flat.source_type_id('beta.Item')] = []types.StructField{}
	tc.structs[flat.source_type_id('dep.Item')] = []types.StructField{}
	tc.selective_import_files['single.v'] = &types.FileImportInfo{
		selective_imports: {
			flat.source_type_id('Item'): [flat.source_type_id('missing.Item'),
				flat.source_type_id('alpha.Item'), flat.source_type_id('alpha.Item')]
		}
	}
	tc.selective_import_files['ambiguous.v'] = &types.FileImportInfo{
		selective_imports: {
			flat.source_type_id('Item'): [flat.source_type_id('alpha.Item'),
				flat.source_type_id('beta.Item')]
		}
	}
	mut t := Transformer{
		tc:                &tc
		cur_module:        'dep'
		module_type_cache: &ModuleTypeCache{}
	}
	item_id := flat.source_type_id('Item')
	for _ in 0 .. 2 {
		t.cur_file = 'single.v'
		assert t.normalize_type_in_module_uncached(item_id, 'dep') == flat.source_type_id('alpha.Item')
		assert t.normalize_type_in_module(item_id, 'dep') == flat.source_type_id('alpha.Item')
		t.cur_file = 'ambiguous.v'
		assert t.normalize_type_in_module_uncached(item_id, 'dep') == flat.source_type_id('dep.Item')
		assert t.normalize_type_in_module(item_id, 'dep') == flat.source_type_id('dep.Item')
		t.cur_file = 'missing.v'
		assert t.normalize_type_in_module_uncached(item_id, 'dep') == flat.source_type_id('dep.Item')
		assert t.normalize_type_in_module(item_id, 'main') == flat.source_type_id('dep.Item')
	}
}

fn test_module_type_normalization_ids_expand_applied_owner_alias_first() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('dep.Item')] = []types.StructField{}
	tc.type_aliases.set_id(flat.source_type_id('dep.Next'), flat.source_type_id('dep.Item'))
	tc.type_aliases.set_id(flat.source_type_id('dep.Alias'), flat.source_type_id('dep.Next'))
	tc.type_aliases.set_id(flat.source_type_id('dep.Values'), flat.source_type_id('[]T'))
	tc.type_alias_generic_params[flat.source_type_id('dep.Values')] = ['T']
	tc.type_aliases.set_id(flat.source_type_id('Values'), flat.source_type_id('map[string]T'))
	tc.type_alias_generic_params[flat.source_type_id('Values')] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'main.v'
	for _ in 0 .. 2 {
		assert t.normalize_type_in_module(flat.source_type_id('Alias'), 'dep') == flat.source_type_id('dep.Item')
		assert t.normalize_type_in_module(flat.source_type_id('Values[int]'), 'dep') == flat.source_type_id('[]int')
		assert t.normalize_type_in_module(flat.source_type_id('Values[int]'), 'main') == flat.source_type_id('map[string]int')
		assert t.normalize_type_in_module(flat.source_type_id('[]Values[int]'), 'dep') == flat.source_type_id('[][]int')
	}
}
