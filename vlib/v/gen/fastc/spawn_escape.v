module fastc

import v.flat
import v.token

// A spawned worker can outlive the function that received a by-value argument.
// Promote its storage once so every worker and subsequent address uses one cell.
fn (mut g Parser) promote_spawn_parameter_storage() {
	if !g.selfhost || g.locals.len == 0 { return }
	for name in g.spawn_escaping_parameters() {
		local := g.locals[name] or { continue }
		if local.is_reference { continue }
		c_name := g.local_c_name(name)
		storage := g.temporary_name('spawn_parameter')
		g.write_line('__typeof__(${c_name}) *${storage} = (__typeof__(${c_name}) *)v_fastc_interface_box(&${c_name}, sizeof(${c_name}));')
		g.locals[name] = FastcLocal{
			...local
			is_reference: true
			typ: flat.source_type_id(local.typ_text() + '*')
			c_name: storage
		}
	}
	g.type_memo.clear()
}

fn (g &Parser) spawn_escaping_parameters() []string {
	mut names := []string{}
	mut scan := g.s
	mut tok := g.tok
	mut braces := 1
	for tok != .eof && braces > 0 {
		if tok == .key_spawn {
			mut call := scan
			mut call_tok := call.scan()
			callee := call.lit
			if call_tok == .name {
				call_tok = call.scan()
				if call_tok == .dot {
					if local := g.locals[callee] {
						if !local.is_reference && !g.underlying_alias_type(local.typ_text()).ends_with('*') && callee !in names {
							names << callee
						}
					}
				}
			}
			for call_tok !in [.lpar, .semicolon, .rcbr, .eof] { call_tok = call.scan() }
			if call_tok == .lpar {
				mut depth := 1
				mut previous := token.Token.lpar
				for depth > 0 {
					call_tok = call.scan()
					if call_tok == .eof { break }
					if previous == .amp && call_tok == .name && call.lit in g.locals && call.lit !in names {
						names << call.lit
					}
					if call_tok == .lpar { depth++ }
					if call_tok == .rpar { depth-- }
					previous = call_tok
				}
			}
		}
		if tok == .lcbr { braces++ }
		if tok == .rcbr { braces-- }
		tok = scan.scan()
	}
	return names
}
