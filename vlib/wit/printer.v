module wit

pub fn (d WitDoc) str() string {
	mut out := []string{}
	if d.package.name != '' {
		out << 'package ${d.package.namespace}:${d.package.name}'
		if d.package.version != '' { out << '@${d.package.version}' }
		out[out.len - 1] += ';'
	}
	for iface in d.interfaces {
		out << ''
		out << 'interface ${iface.name} {'
		for it in iface.items {
			match it.kind {
				.record {
					if r := it.record {
						out << '\trecord ${r.name} {'
						for f in r.fields { out << '\t\t${f.name}: ${f.typ.str()},' }
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
		out << 'world ${w.name} {'
		for imp in w.imports {
			if imp.func != none {
				out << '\timport ${imp.name}: func(...);'
			} else if imp.interface != '' {
				out << '\timport ${imp.name};'
			}
		}
		for exp in w.exports {
			if exp.func != none {
				out << '\texport ${exp.name}: func(...);'
			} else if exp.interface != '' {
				out << '\texport ${exp.name};'
			}
		}
		out << '}'
	}
	return out.join('\n')
}
