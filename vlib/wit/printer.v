module wit

fn wit_func_sig(f WitFunc) string {
	mut ps := []string{}
	for p in f.params {
		ps << '${p.name}: ${p.typ.str()}'
	}
	if f.results.len == 0 {
		return 'func(${ps.join(', ')})'
	} else if f.results.len == 1 {
		return 'func(${ps.join(', ')}) -> ${f.results[0].str()}'
	}
	mut rs := []string{}
	for r in f.results {
		rs << r.str()
	}
	return 'func(${ps.join(', ')}) -> (${rs.join(', ')})'
}

pub fn (d WitDoc) str() string {
	mut out := []string{}
	if d.package.name != '' {
		out << 'package ${d.package.namespace}:${d.package.name}'
		if d.package.version != '' { out << '@${d.package.version}' }
		out[out.len - 1] += ';'
	}
	for iface in d.interfaces {
		out << ''
		for a in iface.attrs {
			out << a
		}
		out << 'interface ${iface.name} {'
		for it in iface.items {
			for a in it.attrs {
				out << '\t${a}'
			}
			if it.kind == .use {
				if u := it.use_ {
					out << '\tuse ${u.path}.{${u.items.join(', ')}};'
				}
				continue
			}
			match it.kind {
				.record {
					if r := it.record {
						out << '\trecord ${r.name} {'
						for f in r.fields { out << '\t\t${f.name}: ${f.typ.str()},' }
						out << '\t}'
					}
				}
				.resource {
					if r := it.resource {
						out << '\tresource ${r.name} {'
						for m in r.methods {
							for a in m.attrs {
								out << '\t\t${a}'
							}
							mut ps := []string{}
							for p in m.params { ps << '${p.name}: ${p.typ.str()}' }
							if m.name == 'constructor' {
								out << ('\t\tconstructor(' + ps.join(', ') + ');')
								continue
							}
							prefix := if m.is_static { 'static ' } else { '' }
							if m.results.len == 0 {
								out << '\t\t${m.name}: ${prefix}func(${ps.join(', ')});'
							} else if m.results.len == 1 {
								out << '\t\t${m.name}: ${prefix}func(${ps.join(', ')}) -> ${m.results[0].str()};'
							} else {
								mut rs := []string{}
								for rr in m.results { rs << rr.str() }
								out << '\t\t${m.name}: ${prefix}func(${ps.join(', ')}) -> (${rs.join(', ')});'
							}
						}
						out << '\t}'
					}
				}
				.func {
					if f := it.func {
						mut ps := []string{}
						for p in f.params { ps << '${p.name}: ${p.typ.str()}' }
						if f.results.len == 0 {
							out << '\t${f.name}: func(${ps.join(', ')});'
						} else if f.results.len == 1 {
							out << '\t${f.name}: func(${ps.join(', ')}) -> ${f.results[0].str()};'
						} else {
							mut rs := []string{}
							for r in f.results { rs << r.str() }
							out << '\t${f.name}: func(${ps.join(', ')}) -> (${rs.join(', ')});'
						}
					}
				}
				else {}
			}
		}
		out << '}'
	}
	for w in d.worlds {
		out << ''
		for a in w.attrs {
			out << a
		}
		out << 'world ${w.name} {'
		for inc in w.includes {
			if inc.with_renames.len == 0 {
				out << '\tinclude ${inc.path};'
			} else {
				mut parts := []string{}
				for new_name, old_name in inc.with_renames {
					if new_name == old_name {
						parts << new_name
					} else {
						parts << '${old_name} as ${new_name}'
					}
				}
				parts.sort()
				out << '\tinclude ${inc.path} with { ${parts.join(', ')} };'
			}
		}
		for imp in w.imports {
			if f := imp.func {
				out << '\timport ${imp.name}: ${wit_func_sig(f)};'
			} else if imp.interface != '' {
				out << '\timport ${imp.name};'
			}
		}
		for exp in w.exports {
			if f := exp.func {
				out << '\texport ${exp.name}: ${wit_func_sig(f)};'
			} else if exp.interface != '' {
				out << '\texport ${exp.name};'
			}
		}
		out << '}'
	}
	return out.join('\n')
}
