module transform

import v.flat

fn test_string_conversion_respects_local_struct_without_checker() {
	for has_local_struct in [false, true] {
		mut a := flat.FlatAst.new()
		flag_type := flat.source_type_id('Flag')
		value := a.add_node(flat.Node{
			kind:  .ident
			value: 'value'
			typ:   flag_type
		})
		fields := ['foreign']
		mut t := Transformer{
			a:          &a
			cur_module: 'records'
			enum_types: {
				flat.source_type_id('Flag'):          fields
				flat.source_type_id('statuses.Flag'): fields
			}
		}
		if has_local_struct {
			t.structs[flat.source_type_id('records.Flag')] = StructInfo{
				name:   flat.source_type_id('records.Flag')
				module: 'records'
			}
		}
		result := t.wrap_string_conversion(value, flag_type)
		if has_local_struct {
			assert !t.used_fns['statuses__Flag__autostr']
			assert a.node(result).kind == .string_literal
			assert a.node(result).value == 'records.Flag{}'
		} else {
			assert t.used_fns['statuses__Flag__autostr']
			assert a.node(result).kind == .call
			assert a.child_node(a.node(result), 0).value == 'statuses__Flag__autostr'
		}
	}
}
