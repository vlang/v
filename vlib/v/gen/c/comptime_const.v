module c

import v.types
import v.flat

fn (mut g FlatGen) comptime_const_initializer(value types.ComptimeConstValue) string {
	base := types.unalias_type(value.typ)
	if base is types.String {
		return '{(u8*)${c_segmented_string_literal(value.text)}, ${value.text.len}, 1}'
	}
	if base is types.ArrayFixed {
		mut items := []string{cap: value.elements.len}
		for item in value.elements { items << g.comptime_const_initializer(item) }
		return '{' + items.join(', ') + '}'
	}
	if base is types.Struct {
		mut items := []string{}
		for field in g.tc.structs[base.name_id] {
			items << '.${c_field_name(field.name)} = ' + g.comptime_const_initializer(value.fields[field.name])
		}
		return '{' + items.join(', ') + '}'
	}
	if base.source_type_id() == flat.source_type_bool {
		return if value.integer != 0 { 'true' } else { 'false' }
	}
	if base.source_type_id() in [flat.source_type_u64, flat.source_type_uint] {
		return '${u64(value.integer)}ULL'
	}
	if value.integer == i64(-9223372036854775807) - 1 { return '(-9223372036854775807LL - 1)' }
	return '${value.integer}LL'
}
