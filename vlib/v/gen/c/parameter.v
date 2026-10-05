module c

import v.flat
import v.types { unalias_type }

fn indirect_value_parameter(typ &types.Type) bool {
	return unalias_type(typ) is types.SumType
}

fn (mut g FlatGen) gen_indirect_value_argument(id flat.NodeId, typ &types.Type) bool {
	if !indirect_value_parameter(typ) {
		return false
	}
	ct := g.value_c_type(typ)
	g.write('&(${ct}[]){')
	g.gen_expr_with_expected_type(id, typ)
	g.write('}[0]')
	return true
}

fn (mut g FlatGen) parameter_c_type(typ &types.Type) string {
	ct := g.callback_c_type(typ)
	if indirect_value_parameter(typ) {
		return '${ct}*'
	}
	return ct
}
