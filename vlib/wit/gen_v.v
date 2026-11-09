module wit

pub fn (d WitDoc) to_v() string {
	mut out := []string{}
	if d.package.name != '' {
		out << '// package: ${d.package.namespace}:${d.package.name}'
		if d.package.version != '' { out << '// version: ${d.package.version}' }
	}
	for iface in d.interfaces {
		out << ''
		out << '// interface ${iface.name}'
		for it in iface.items {
			match it.kind {
				.record {
					if r := it.record {
						out << 'pub struct ${to_v_ident(r.name)} {'
						for f in r.fields {
							out << '\t${to_v_ident(f.name)} ${wit_type_to_v(f.typ)}'
						}
						out << '}'
					}
				}
				.enum_ {
					if e := it.enum_ {
						out << 'pub enum ${to_v_ident(e.name)} {'
						for c in e.cases {
							out << '\t${to_v_ident(c)}'
						}
						out << '}'
					}
				}
				.flags {
					if f := it.flags {
						out << '// flags ${f.name}: ${f.flags.join(', ')}'
					}
				}
				.variant {
					if v := it.variant {
						out << '// variant ${v.name}'
					}
				}
				.type_alias {
					if t := it.type_def {
						out << 'pub type ${to_v_ident(t.name)} = ${wit_type_to_v(t.typ)}'
					}
				}
				.func {
					if f := it.func {
						params := f.params.map('${to_v_ident(it.name)} ${wit_type_to_v(it.typ)}'.replace(it.name, it.name)).join(', ')
						_ = params
						mut ps := []string{}
						for p in f.params { ps << '${to_v_ident(p.name)} ${wit_type_to_v(p.typ)}' }
						mut rs := 'void'
						if f.results.len == 1 {
							rs = wit_type_to_v(f.results[0])
						} else if f.results.len > 1 {
							mut ts := []string{}
							for r in f.results { ts << wit_type_to_v(r) }
							rs = '(${ts.join(', ')})'
						}
						out << 'pub fn ${to_v_ident(f.name)}(${ps.join(', ')}) ${rs} {'
						out << '\tpanic("TODO: implement")'
						out << '}'
					}
				}
				else {}
			}
		}
	}
	return out.join('\n')
}

fn to_v_ident(s string) string {
	// convert kebab-case to snake_case
	mut r := s.replace('-', '_')
	return r
}

fn wit_type_to_v(t WitType) string {
	match t.kind {
		.prim {
			return match t.prim {
				'bool' { 'bool' }
				'u8' { 'u8' }
				'u16' { 'u16' }
				'u32' { 'u32' }
				'u64' { 'u64' }
				's8' { 'i8' }
				's16' { 'i16' }
				's32' { 'int' }
				's64' { 'i64' }
				'f32' { 'f32' }
				'f64' { 'f64' }
				'char' { 'rune' }
				'string' { 'string' }
				else { '/*${t.prim}*/' }
			}
		}
		.named { return to_v_ident(t.name) }
		.list {
			if t.items.len > 0 { return '[]${wit_type_to_v(t.items[0])}' }
			return '[]u8'
		}
		.option {
			if t.items.len > 0 { return '?${wit_type_to_v(t.items[0])}' }
			return '?voidptr'
		}
		.result {
			if t.items.len > 0 { return '!${wit_type_to_v(t.items[0])}' }
			return '!voidptr'
		}
		.tuple {
			mut ts := []string{}
			for it in t.items { ts << wit_type_to_v(it) }
			return '(${ts.join(', ')})'
		}
	}
}
