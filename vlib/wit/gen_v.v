module wit

pub fn (d WitDoc) to_v() string {
	// collect defined type names first
	mut defined := map[string]bool{}
	for iface in d.interfaces {
		for it in iface.items {
			match it.kind {
				.record {
					if r := it.record { defined[r.name] = true }
				}
				.enum_ {
					if e := it.enum_ { defined[e.name] = true }
				}
				.variant {
					if v := it.variant { defined[v.name] = true }
				}
				.flags {
					if f := it.flags { defined[f.name] = true }
				}
				.resource {
					if r := it.resource { defined[r.name] = true }
				}
				.type_alias {
					if t := it.type_def { defined[t.name] = true }
				}
				else {}
			}
		}
	}
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
						out << 'pub struct ${to_v_type_name(r.name)} {'
						for f in r.fields {
							out << '\t${to_v_ident(f.name)} ${wit_type_to_v(f.typ, defined)}'
						}
						out << '}'
					}
				}
				.enum_ {
					if e := it.enum_ {
						out << 'pub enum ${to_v_type_name(e.name)} {'
						for c in e.cases {
							out << '\t${to_v_ident(c)}'
						}
						out << '}'
					}
				}
				.flags {
					if f := it.flags {
						out << 'pub type ${to_v_type_name(f.name)} = int'
						for i, fl in f.flags {
							out << 'pub const ${to_v_ident(f.name)}_${to_v_ident(fl)} = ${1 << i}'
						}
					}
				}
				.variant {
					if v := it.variant {
						for c in v.cases {
							if ct := c.typ {
								out << 'pub struct ${to_v_type_name(v.name)}_${to_v_type_name(c.name)} {'
								out << '\tvalue ${wit_type_to_v(ct, defined)}'
								out << '}'
							} else {
								out << 'pub struct ${to_v_type_name(v.name)}_${to_v_type_name(c.name)} {}'
							}
						}
						mut cases := []string{}
						for c in v.cases {
							cases << '${to_v_type_name(v.name)}_${to_v_type_name(c.name)}'
						}
						out << 'pub type ${to_v_type_name(v.name)} = ${cases.join(' | ')}'
					}
				}
				.type_alias {
					if t := it.type_def {
						out << 'pub type ${to_v_type_name(t.name)} = ${wit_type_to_v(t.typ, defined)}'
					}
				}
				.resource {
					if r := it.resource {
						out << 'pub struct ${to_v_type_name(r.name)} {}'
						for m in r.methods {
							mut ps := []string{}
							for p in m.params {
								ps << '${to_v_ident(p.name)} ${wit_type_to_v(p.typ, defined)}'
							}
							mut rs := ''
							if m.results.len == 1 {
								rs = wit_type_to_v(m.results[0], defined)
							} else if m.results.len > 1 {
								mut ts := []string{}
								for r2 in m.results { ts << wit_type_to_v(r2, defined) }
								rs = '(${ts.join(', ')})'
							}
							if m.is_static {
								// WIT static methods have no self; emit a plain
								// namespaced function.
								sig := if rs != '' {
									'${ps.join(', ')}) ${rs} {'
								} else {
									'${ps.join(', ')}) {'
								}
								out << 'pub fn ${to_v_ident(r.name)}_${to_v_ident(m.name)}(' + sig
							} else if rs != '' {
								out << 'pub fn (mut self ${to_v_type_name(r.name)}) ${to_v_ident(m.name)}(${ps.join(', ')}) ${rs} {'
							} else {
								out << 'pub fn (mut self ${to_v_type_name(r.name)}) ${to_v_ident(m.name)}(${ps.join(', ')}) {'
							}
							out << '\tpanic("TODO: implement")'
							out << '}'
						}
					}
				}
				.use {
					if u := it.use_ {
						out << '// use ${u.path} { ${u.items.join(', ')} }'
					}
				}
				.func {
					if f := it.func {
						mut ps := []string{}
						for p in f.params {
							ps << '${to_v_ident(p.name)} ${wit_type_to_v(p.typ, defined)}'
						}
						mut rs := ''
						if f.results.len == 1 {
							rs = wit_type_to_v(f.results[0], defined)
						} else if f.results.len > 1 {
							mut ts := []string{}
							for r in f.results { ts << wit_type_to_v(r, defined) }
							rs = '(${ts.join(', ')})'
						}
						if rs != '' {
							out << 'pub fn ${to_v_ident(f.name)}(${ps.join(', ')}) ${rs} {'
						} else {
							out << 'pub fn ${to_v_ident(f.name)}(${ps.join(', ')}) {'
						}
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
	mut r := s.replace('-', '_')
	r = r.trim_left('%')
	if r.len == 0 || (r[0] >= `0` && r[0] <= `9`) {
		r = 'arg_' + r
	}
	return r
}

fn to_v_type_name(s string) string {
	mut r := s.replace('-', '_')
	r = r.trim_left('%')
	if r.len == 0 { return r }
	r = r[0].ascii_str().to_upper() + r[1..]
	return r
}

fn wit_type_to_v(t WitType, defined map[string]bool) string {
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
		.named {
			n := t.name
			if n == '_' { return 'void' }
			if n.starts_with('borrow<') || n.starts_with('own<') { return 'voidptr' }
			if n == 'pollable' { return 'int' }
			// Async surface types (future<T>, stream<T>) and other externals
			// map to voidptr: only the surface (types, imports, exports) is
			// generated; runtime support is out of scope. Known limitation:
			// async has surface types only, no runtime (no task/waitable
			// state, no pollable dispatch); see CONFORMANCE.md lift rows.
			if n !in defined { return 'voidptr' }
			return to_v_type_name(n)
		}
		.list {
			if t.items.len > 0 { return '[]${wit_type_to_v(t.items[0], defined)}' }
			return '[]u8'
		}
		.option {
			if t.items.len > 0 { return '?${wit_type_to_v(t.items[0], defined)}' }
			return '?voidptr'
		}
		.result {
			if t.items.len == 0 { return '!' }
			if t.items[0].kind == .named && t.items[0].name == '_' { return '!' }
			okstr := wit_type_to_v(t.items[0], defined)
			// V cannot express result<option<T>, E> as !?T; collapse to ?T.
			if okstr.starts_with('?') { return okstr }
			return '!${okstr}'
		}
		.tuple {
			// V has no anonymous tuple types usable as fields; homogeneous
			// tuples map to arrays, heterogeneous ones to voidptr.
			mut ts := []string{}
			for it in t.items { ts << wit_type_to_v(it, defined) }
			if ts.len == 0 { return 'voidptr' }
			mut same := true
			for x in ts[1..] {
				if x != ts[0] {
					same = false
					break
				}
			}
			if same { return '[]${ts[0]}' }
			return 'voidptr'
		}
	}
}
