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
// none when the shape is not liftable. The doc-free form lifts only
// string/primitive shapes; use wit_func_lift_with_doc for enum/flags-aware
// lifting (enums lift as s32, flags as u32).
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

// wit_is_enum reports whether name resolves to a WIT enum in doc.
fn wit_is_enum(name string, doc WitDoc) bool {
	for iface in doc.interfaces {
		for it in iface.items {
			if it.kind == .enum_ {
				if e := it.enum_ {
					if e.name == name {
						return true
					}
				}
			}
		}
	}
	return false
}

// wit_is_flags reports whether name resolves to a WIT flags set in doc.
fn wit_is_flags(name string, doc WitDoc) bool {
	for iface in doc.interfaces {
		for it in iface.items {
			if it.kind == .flags {
				if f := it.flags {
					if f.name == name {
						return true
					}
				}
			}
		}
	}
	return false
}

// wit_func_lift_with_doc lifts string/primitive shapes like wit_func_lift and
// additionally lifts enum params/results as component s32 over core i32 and
// flags params/results (32 or fewer flags) as component u32 over core i32.
// Composite shapes (lists, records, variants, options/results, tuples, maps,
// resources) are still refused: their canonical layout helpers now exist
// (wasm.cabi_*_layout, wit_cabi_size_align) but the core wrapper that would
// marshal them has no allocation path yet.
fn wit_func_lift_with_doc(f WitFunc, doc WitDoc) ?wasm.CabiStringExport {
	mut core_params := []u8{}
	mut comp_params := []u8{}
	mut comp_labels := []string{}
	mut has_lift := false
	for p in f.params {
		if p.typ.kind == .prim {
			if p.typ.prim == 'string' {
				core_params << u8(0x7f)
				core_params << u8(0x7f)
				comp_params << u8(0x73)
				comp_labels << p.name
				has_lift = true
				continue
			}
			cc, cv := wit_prim_codes(p.typ.prim)
			if cc < 0 {
				return none
			}
			core_params << u8(cc)
			comp_params << u8(cv)
			comp_labels << p.name
			continue
		}
		if p.typ.kind == .named {
			if wit_is_enum(p.typ.name, doc) {
				core_params << u8(0x7f)
				comp_params << u8(0x7a)
				comp_labels << p.name
				has_lift = true
				continue
			}
			if wit_is_flags(p.typ.name, doc) {
				core_params << u8(0x7f)
				comp_params << u8(0x79)
				comp_labels << p.name
				has_lift = true
				continue
			}
		}
		return none
	}
	mut core_results := []u8{}
	mut comp_results := []u8{}
	if f.results.len == 0 {
	} else if f.results.len == 1 {
		r := f.results[0]
		if r.kind == .prim {
			if r.prim == 'string' {
				core_results = [u8(0x7f), u8(0x7f)]
				comp_results = [u8(0x73)]
				has_lift = true
			} else {
				cc, cv := wit_prim_codes(r.prim)
				if cc < 0 {
					return none
				}
				core_results = [u8(cc)]
				comp_results = [u8(cv)]
			}
		} else if r.kind == .named {
			if wit_is_enum(r.name, doc) {
				core_results = [u8(0x7f)]
				comp_results = [u8(0x7a)]
				has_lift = true
			} else if wit_is_flags(r.name, doc) {
				core_results = [u8(0x7f)]
				comp_results = [u8(0x79)]
				has_lift = true
			} else {
				return none
			}
		} else {
			return none
		}
	} else {
		return none
	}
	if !has_lift {
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
			if mut spec := wit_func_lift_with_doc(f, doc) {
				spec.wit_name = e.name
				string_exports[e.name.replace('-', '_')] = spec
			} else if mut spec := wit_func_lift(f) {
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
							if mut spec := wit_func_lift_with_doc(f, doc) {
								spec.wit_name = f.name
								string_exports[f.name.replace('-', '_')] = spec
							} else if mut spec := wit_func_lift(f) {
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
