module transform

import v.flat
import v.types

fn test_ordinary_enum_does_not_resolve_to_same_named_flag_enum() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'ui'
	tc.enum_names[flat.source_type_id('ui.Modifier')] = true
	tc.enum_names[flat.source_type_id('gg.Modifier')] = true
	tc.flag_enums[flat.source_type_id('gg.Modifier')] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'ui'
	assert t.resolve_flag_enum_type_name(flat.source_type_id('Modifier')) == none
	assert t.resolve_flag_enum_type_name(flat.source_type_id('ui.Modifier')) == none
	assert flat.source_type_name(t.resolve_flag_enum_type_name(flat.source_type_id('gg.Modifier'))?) == 'gg.Modifier'
}
