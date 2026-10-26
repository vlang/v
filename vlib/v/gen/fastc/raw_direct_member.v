module fastc

import v.flat

// render_raw_local_direct_member preserves the raw spelling of one local field
// read. Map lengths and enum type-name collisions retain the general renderer.
fn (g &Parser) render_raw_local_direct_member(tokens []FastcExpressionToken) ?string {
	if !g.selfhost || tokens.len != 3 || tokens[0].tok != .name
		|| tokens[1].tok != .dot || tokens[2].tok != .name
		|| tokens[1].lit !in ['', '.'] {
		return none
	}
	for item in tokens {
		if item.has_source() || item.is_mut_argument { return none }
	}
	root := tokens[0].lit
	if root == 'C' || root.starts_with('@') { return none }
	local := g.locals[root] or { return none }
	if g.is_enum_type_name(root) { return none }
	if tokens[2].lit == 'len' && g.map_length_receiver_type(tokens, 1) != none { return none }
	base := if local.c_name != '' { local.c_name } else { fastc_c_identifier(root) }
	separator := if flat.source_type_query(local.typ).has(.c_pointer_suffix) { '->' } else { '.' }
	return '${base}${separator}${fastc_c_identifier(tokens[2].lit)}'
}
