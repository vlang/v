module wit

import wasm

// wit_prim_codes maps a WIT primitive to (core wasm valtype, component
// valtype), or (-1, -1) when unrepresentable.
fn wit_prim_codes(p string) (int, int) {
	match p {
		'bool' { return 0x7f, 0x7f }
		'u8' { return 0x7f, 0x7d }
		'u16' { return 0x7f, 0x7b }
		'u32' { return 0x7f, 0x79 }
		'u64' { return 0x7e, 0x77 }
		's8' { return 0x7f, 0x7e }
		's16' { return 0x7f, 0x7c }
		's32' { return 0x7f, 0x7a }
		's64' { return 0x7e, 0x78 }
		'f32' { return 0x7d, 0x76 }
		'f64' { return 0x7c, 0x75 }
		'char' { return 0x7f, 0x74 }
		else { return -1, -1 }
	}
}

// wit_func_lift describes how one WIT function maps onto a core export, or
// none when the shape is not liftable.
fn wit_func_lift(f WitFunc) ?wasm.CabiStringExport {
	mut core_params := []u8{}
	mut comp_params := []u8{}
	mut comp_labels := []string{}
	mut has_string := false
	for p in f.params {
		if p.typ.kind != .prim {
			return none
		}
		if p.typ.prim == 'string' {
			core_params << u8(0x7f)
			core_params << u8(0x7f)
			comp_params << u8(0x73)
			comp_labels << p.name
			has_string = true
			continue
		}
		cc, cv := wit_prim_codes(p.typ.prim)
		if cc < 0 {
			return none
		}
		core_params << u8(cc)
		comp_params << u8(cv)
		comp_labels << p.name
	}
	mut core_results := []u8{}
	mut comp_results := []u8{}
	if f.results.len == 0 {
	} else if f.results.len == 1 {
		r := f.results[0]
		if r.kind != .prim {
			return none
		}
		if r.prim == 'string' {
			core_results = [u8(0x7f), u8(0x7f)]
			comp_results = [u8(0x73)]
			has_string = true
		} else {
			cc, cv := wit_prim_codes(r.prim)
			if cc < 0 {
				return none
			}
			core_results = [u8(cc)]
			comp_results = [u8(cv)]
		}
	} else {
		return none
	}
	if !has_string {
		return none
	}
	return wasm.CabiStringExport{
		comp_params:  comp_params
		comp_labels:  comp_labels
		comp_results: comp_results
		core_params:  core_params
		core_results: core_results
	}
}

pub fn wrap_component(core []u8, doc WitDoc, world string) []u8 {
	// Pick the world: the named one, or the first with exports.
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
	mut string_exports := map[string]wasm.CabiStringExport{}
	// direct func exports
	for e in w.exports {
		if f := e.func {
			if mut spec := wit_func_lift(f) {
				spec.wit_name = e.name
				string_exports[e.name.replace('-', '_')] = spec
			}
		}
		// interface exports: lift matching funcs by their own names
		if e.interface != '' {
			for iface in doc.interfaces {
				if iface.name != e.interface && iface.name != e.interface.all_after_last('/') {
					continue
				}
				for it in iface.items {
					if it.kind == .func {
						if f := it.func {
							if mut spec := wit_func_lift(f) {
								spec.wit_name = f.name
								string_exports[f.name.replace('-', '_')] = spec
							}
						}
					}
				}
			}
		}
	}
	if string_exports.len == 0 {
		return wasm.wrap_core_module(core)
	}
	return wasm.wrap_core_module_strings(core, string_exports)
}
