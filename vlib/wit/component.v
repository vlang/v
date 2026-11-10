module wit

import wasm

fn is_wit_string(t WitType) bool {
	return t.kind == .prim && t.prim == 'string'
}

fn is_single_string_func(f WitFunc) bool {
	return f.params.len == 1 && is_wit_string(f.params[0].typ) && f.results.len <= 1
}

pub fn wrap_component(core []u8, doc WitDoc, world string) []u8 {
	// Pick the world: the named one, or the first world with exports.
	mut widx := -1
	for i, w in doc.worlds {
		if world != '' && w.name == world {
			widx = i
			break
		}
		if world == '' && w.exports.len > 0 {
			widx = i
			break
		}
	}
	if widx < 0 {
		return wasm.wrap_core_module(core)
	}
	w := doc.worlds[widx]
	// renames maps core export names (kebab-case to snake_case) to component
	// export names for single-string-param functions.
	mut renames := map[string]string{}
	for e in w.exports {
		// direct func export: export run: func(s: string) -> ...;
		if f := e.func {
			if is_single_string_func(f) {
				renames[e.name.replace('-', '_')] = e.name
			}
		}
		// interface export: match funcs of the named interface.
		if e.interface != '' {
			for iface in doc.interfaces {
				if iface.name != e.interface && iface.name != e.interface.all_after_last('/') {
					continue
				}
				for it in iface.items {
					if it.kind == .func {
						if f := it.func {
							if is_single_string_func(f) {
								renames[f.name.replace('-', '_')] = f.name
							}
						}
					}
				}
			}
		}
	}
	if renames.len == 0 {
		return wasm.wrap_core_module(core)
	}
	return wasm.wrap_core_module_strings(core, renames)
}
