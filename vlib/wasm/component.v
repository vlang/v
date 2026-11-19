module wasm

import encoding.leb128

pub enum ComponentSection as u8 {
	custom_section
	core_module_section = 1
	core_instance_section
	core_type_section
	component_section
	instance_section
	alias_section
	type_section
	canon_section
	start_section
	import_section
	export_section
	value_section
}

// ---- helpers ----

fn read_uleb128(data []u8, ip &int) u32 {
	mut i := *ip
	mut result := u32(0)
	mut shift := u32(0)
	for {
		if i >= data.len { break }
		b := u32(data[i])
		i++
		result |= (b & 0x7f) << shift
		if b & 0x80 == 0 { break }
		shift += 7
	}
	unsafe { *ip = i }
	return result
}

fn read_name(data []u8, ip &int) string {
	n := read_uleb128(data, ip)
	mut i := *ip
	if i + int(n) > data.len {
		unsafe { *ip = i }
		return ''
	}
	s := data[i..i + int(n)].bytestr()
	i += int(n)
	unsafe { *ip = i }
	return s
}

struct CoreFuncType {
	params  []u8
	results []u8
}

struct CoreExport {
	name string
	kind u8
	idx  int
}

struct CoreImport {
	mod     string
	name    string
	kind    u8
	typeidx int
}

// parse core wasm module: returns func exports, func type info and imports.
fn parse_core_module(core []u8) ([]CoreExport, []CoreFuncType, []int, int, []int, []CoreImport) {
	mut exports := []CoreExport{}
	mut types := []CoreFuncType{}
	mut import_func_types := []int{}
	mut defined_func_typeidx := []int{}
	mut import_func_count := 0
	mut imports := []CoreImport{}
	if core.len < 8 { return exports, types, defined_func_typeidx, 0, []int{}, imports }
	mut i := 8
	for i < core.len {
		if i >= core.len { break }
		sec_id := core[i]
		i++
		sec_size := read_uleb128(core, &i)
		end := i + sec_size
		if sec_id == 1 {
			// type section
			count := read_uleb128(core, &i)
			for _ in 0 .. count {
				// func type: 0x60
				if i < core.len && core[i] == 0x60 { i++ }
				pc := read_uleb128(core, &i)
				mut params := []u8{}
				for _ in 0 .. pc {
					params << core[i]
					i++
				}
				rc := read_uleb128(core, &i)
				mut results := []u8{}
				for _ in 0 .. rc {
					results << core[i]
					i++
				}
				types << CoreFuncType{ params: params, results: results }
			}
		} else if sec_id == 2 {
			// import section
			count := read_uleb128(core, &i)
			for _ in 0 .. count {
				mod := read_name(core, &i)
				field := read_name(core, &i)
				kind := core[i]
				i++
				if kind == 0 {
					ti := read_uleb128(core, &i)
					import_func_types << ti
					import_func_count++
					imports << CoreImport{ mod: mod, name: field, kind: kind, typeidx: ti }
				} else if kind == 1 {
					imports << CoreImport{ mod: mod, name: field, kind: kind, typeidx: -1 }
					// table
					i++ // elem type
					// limits
					ft := core[i]
					i++
					read_uleb128(core, &i)
					if ft & 0x01 != 0 { read_uleb128(core, &i) }
				} else if kind == 2 {
					imports << CoreImport{ mod: mod, name: field, kind: kind, typeidx: -1 }
					// memory
					ft := core[i]
					i++
					read_uleb128(core, &i)
					if ft & 0x01 != 0 { read_uleb128(core, &i) }
				} else if kind == 3 {
					imports << CoreImport{ mod: mod, name: field, kind: kind, typeidx: -1 }
					// global
					i += 2 // valtype + mut
				}
			}
		} else if sec_id == 3 {
			// function section
			count := read_uleb128(core, &i)
			for _ in 0 .. count {
				ti := read_uleb128(core, &i)
				defined_func_typeidx << ti
			}
		} else if sec_id == 7 {
			// export section
			count := read_uleb128(core, &i)
			for _ in 0 .. count {
				name := read_name(core, &i)
				kind := core[i]
				i++
				idx := read_uleb128(core, &i)
				exports << CoreExport{ name: name, kind: kind, idx: idx }
			}
		}
		i = end
	}
	// build func_types slice aligned with function index space
	mut func_typeidx := []int{}
	for tt in import_func_types { func_typeidx << tt }
	for tt in defined_func_typeidx { func_typeidx << tt }
	return exports, types, defined_func_typeidx, import_func_count, func_typeidx, imports
}

fn core_to_component_valtype(ct u8) int {
	// returns component primvaltype code, or -1 if unsupported
	match ct {
		0x7f { return 0x7a } // i32 -> s32
		0x7e { return 0x78 } // i64 -> s64
		0x7d { return 0x76 } // f32
		0x7c { return 0x75 } // f64
		else { return -1 }
	}
}

// sanitize_component_label maps an arbitrary core module or field name to a
// valid Component Model label tail: lowercase kebab-case over `[0-9a-z-]`
// where every run of unsupported bytes collapses to a single `-`. The result
// always starts with a letter (`h`/`h-` fallback), so joining two results
// with `-` stays a valid label. Uniqueness across imports is carried by the
// `imp<idx>` prefix added by component_host_import_name, not by this mapping.
fn sanitize_component_label(s string) string {
	mut out := []u8{}
	mut last_dash := true // collapse runs and trim a leading dash
	for i in 0 .. s.len {
		c := s[i]
		mut ch := c
		if c >= 65 && c <= 90 {
			ch = c + 32
		} else if !((c >= 97 && c <= 122) || (c >= 48 && c <= 57) || c == 45) {
			ch = 45 // `-`
		}
		if ch == 45 {
			if !last_dash {
				out << ch
				last_dash = true
			}
		} else {
			out << ch
			last_dash = false
		}
	}
	for out.len > 0 && out.last() == 45 {
		out.delete_last()
	}
	res := out.bytestr()
	if res == '' {
		return 'h'
	}
	if res[0] >= 48 && res[0] <= 57 {
		return 'h-' + res
	}
	return res
}

// component_host_import_name builds the component-level import name for the
// `idx`-th forwarded core function import from core module `mod`, field
// `name`. The name is `imp<idx>-<mod>-<field>` after sanitization, which is a
// valid plain `label`: it starts with a letter and only joins valid labels
// with `-`. The `imp<idx>` prefix also makes the names strongly unique: the
// canonical form (hyphens stripped, acronyms lowercased) is `imp` + the
// decimal index + the stripped base, and the stripped base always starts
// with a letter, so two canonical forms can only match when the indices
// match - and the index is unique per import. The original `mod`/`name` are
// kept verbatim in the core-instance bindings.
fn component_host_import_name(idx int, mod string, field string) string {
	return 'imp${idx}-${sanitize_component_label(mod)}-${sanitize_component_label(field)}'
}

// is_component_label reports whether `s` parses as a Component Model `label`:
// hyphen-separated runs of `[0-9a-z]+` or `[0-9A-Z]+`, starting with a letter.
fn is_component_label(s string) bool {
	if s == '' {
		return false
	}
	if s[0] == 45 || s[s.len - 1] == 45 {
		return false
	}
	parts := s.split('-')
	for i, p in parts {
		if !is_label_fragment(p, i == 0) {
			return false
		}
	}
	return true
}

fn is_label_fragment(f string, first bool) bool {
	if f == '' {
		return false
	}
	mut lower := 0
	mut upper := 0
	for i in 0 .. f.len {
		c := f[i]
		if c >= 97 && c <= 122 {
			lower++
		} else if c >= 65 && c <= 90 {
			upper++
		} else if c >= 48 && c <= 57 {
			if first && i == 0 {
				return false
			}
		} else {
			return false
		}
	}
	if lower > 0 && upper > 0 {
		return false
	}
	return true
}

// canonical_component_label canonicalizes `s` per the Component Model
// strong-uniqueness rule: strip `-` and lowercase ASCII capitals. Two names
// in one scope are strongly unique iff their canonical forms differ.
fn canonical_component_label(s string) string {
	mut out := []u8{cap: s.len}
	for i in 0 .. s.len {
		c := s[i]
		if c == 45 {
			continue
		}
		if c >= 65 && c <= 90 {
			out << c + 32
		} else {
			out << c
		}
	}
	return out.bytestr()
}

fn write_section(mut out []u8, id u8, payload []u8) {
	out << id
	out << leb128.encode_u32(u32(payload.len))
	out << payload
}

// ---- public API ----

pub fn wrap_core_module(core []u8) []u8 {
	return wrap_core_module_strings(core, map[string]CabiStringExport{})
}

// CabiStringExport describes a core export with WIT-known string params
// and/or a string result. comp_params/comp_labels describe the component
// function type, comp_results its (zero or one) results; core_params and
// core_results give the expected core wasm signature the export must have
// to be lifted (string params expand to two i32s; a string result is the
// canonical indirect single i32 pointer to an 8-byte (ptr, len) pair).
pub struct CabiStringExport {
pub mut:
	wit_name     string
	comp_params  []u8
	comp_labels  []string
	comp_results []u8
	core_params  []u8
	core_results []u8
}

// wrap_core_module_strings wraps a core module like wrap_core_module, except
// that core exports listed in string_exports are lifted with the given
// canonical-ABI string params/results and canonical (memory) (realloc)
// options, and exported under their WIT name.
// Core imports with numeric-only func signatures are forwarded: each import
// becomes a component import (named `imp<idx>-<mod>-<field>` in valid
// kebab-case, e.g. `imp0-wasi-snapshot-preview1-fd-write`), lowered to a
// core func, bundled per module into inline-export instances, and supplied as
// instantiation args for the wrapped module. This keeps the produced
// component valid when the core imports e.g. wasi_snapshot_preview1.fd_write.
// Cores with unsupported imports (non-func kinds or unmappable types) fall
// back to plain module wrapping instead of emitting an invalid zero-arg
// instantiation.
// Known limitations: the scratch/sret scheme above is not reentrant
// (nested host -> guest -> host -> guest calls clobber outer slots);
// lifted string results assume static-lifetime guest data; composite types
// (lists, records, variants, options/results, tuples, maps) are not lifted
// yet and fall back to plain wrapping.
pub fn wrap_core_module_strings(core []u8, string_exports map[string]CabiStringExport) []u8 {
	// If we can lift at least one function, produce a richer component.
	exports, types, _defined, _, func_typeidx, core_imports := parse_core_module(core)
	// Detect memory and realloc exports for canonical opts. The component's
	// core-memory index space is populated only by explicit aliases, so the
	// memory export must be aliased before a `(memory 0)` canon opt can
	// reference it. Likewise the `(realloc f)` opt takes the component-level
	// core-func alias index, not the nested module's original func index.
	mut mem_name := ''
	mut has_memory := false
	mut realloc_name := ''
	mut realloc_found := false
	for e in exports {
		if e.kind == 2 && !has_memory {
			has_memory = true
			mem_name = e.name
		}
		if e.kind == 0 && (e.name == 'cabi_realloc' || e.name == 'realloc') && !realloc_found {
			realloc_found = true
			realloc_name = e.name
		}
	}
	// Determine liftable exports, excluding the canonical realloc itself
	// (it is an adapter for canon opts, not a component export) and the
	// WASI `_start` entry (a command export, not a component function; its
	// name is not a valid component extern name).
	mut liftable := []CoreExport{}
	mut str_specs := map[string]CabiStringExport{}
	for e in exports {
		if e.kind != 0 {
			continue
		}
		if e.name == 'cabi_realloc' || e.name == 'realloc' {
			continue
		}
		if e.name == '_start' {
			continue
		}
		// Canonical-ABI string export: core signature must match the expansion.
		if spec := string_exports[e.name] {
			ti := if e.idx >= 0 && e.idx < func_typeidx.len { func_typeidx[e.idx] } else { -1 }
			if ti >= 0 && ti < types.len {
				ft := types[ti]
				if ft.params == spec.core_params && ft.results == spec.core_results {
					liftable << e
					str_specs[e.name] = spec
					continue
				}
			}
		}
		// map export idx to typeidx
		ti := if e.idx >= 0 && e.idx < func_typeidx.len { func_typeidx[e.idx] } else { -1 }
		if ti < 0 || ti >= types.len {
			continue
		}
		ft := types[ti]
		// check params/results supported and at most one result
		mut ok := true
		for p in ft.params {
			if core_to_component_valtype(p) < 0 {
				ok = false
				break
			}
		}
		if ok {
			if ft.results.len > 1 {
				ok = false
			}
			for r in ft.results {
				if core_to_component_valtype(r) < 0 {
					ok = false
					break
				}
			}
		}
		if ok {
			liftable << e
		}
	}
	if liftable.len == 0 {
		// fallback: just wrap module
		mut out := []u8{}
		out << u8(0x00)
		out << u8(0x61)
		out << u8(0x73)
		out << u8(0x6d)
		out << u8(0x0d)
		out << u8(0x00)
		out << u8(0x01)
		out << u8(0x00)
		mut payload := []u8{}
		payload << core
		write_section(mut out, int(ComponentSection.core_module_section), payload)
		return out
	}
	// Forward core imports when the core needs host functions (e.g. a
	// `println` guest importing wasi_snapshot_preview1.fd_write). Only
	// numeric-only func imports are supported; anything else falls back to
	// plain module wrapping instead of emitting an invalid zero-arg instance.
	mut imp_types := [][]u8{}
	mut imp_comp_results := [][]u8{}
	mut forward_imports := false
	mut n_imp := 0
	if core_imports.len > 0 {
		mut ok_all := true
		for imp in core_imports {
			if imp.kind != 0 {
				ok_all = false
				break
			}
			if imp.typeidx < 0 || imp.typeidx >= types.len {
				ok_all = false
				break
			}
			ft := types[imp.typeidx]
			if ft.results.len > 1 {
				ok_all = false
				break
			}
			for p in ft.params {
				if core_to_component_valtype(p) < 0 {
					ok_all = false
					break
				}
			}
			if !ok_all {
				break
			}
			for r in ft.results {
				if core_to_component_valtype(r) < 0 {
					ok_all = false
					break
				}
			}
			if !ok_all {
				break
			}
		}
		if !ok_all {
			// unsupported imports: valid fallback without instantiation
			mut out := []u8{}
			out << u8(0x00)
			out << u8(0x61)
			out << u8(0x73)
			out << u8(0x6d)
			out << u8(0x0d)
			out << u8(0x00)
			out << u8(0x01)
			out << u8(0x00)
			mut payload := []u8{}
			payload << core
			write_section(mut out, int(ComponentSection.core_module_section), payload)
			return out
		}
		for imp in core_imports {
			ft := types[imp.typeidx]
			mut cp := []u8{}
			for p in ft.params {
				cp << u8(core_to_component_valtype(p))
			}
			mut cr := []u8{}
			for r in ft.results {
				cr << u8(core_to_component_valtype(r))
			}
			imp_types << cp
			imp_comp_results << cr
		}
		forward_imports = true
		n_imp = core_imports.len
	}
	// Group imports by module for per-module inline-export instances.
	mut mod_order := []string{}
	mut mod_to_impidx := map[string][]int{}
	if forward_imports {
		for idx, imp in core_imports {
			if imp.mod !in mod_to_impidx {
				mod_order << imp.mod
				mod_to_impidx[imp.mod] = []int{}
			}
			mod_to_impidx[imp.mod] << idx
		}
	}
	n_mod := mod_order.len
	main_inst_idx := n_mod
	// Type section payload: import types first, then lift types.
	mut type_payload := []u8{}
	type_payload << leb128.encode_u32(u32(n_imp + liftable.len))
	for ii in 0 .. n_imp {
		cp := imp_types[ii]
		cr := imp_comp_results[ii]
		type_payload << u8(0x40) // functype
		type_payload << leb128.encode_u32(u32(cp.len))
		for idxp, p in cp {
			label := 'p${idxp}'
			type_payload << leb128.encode_u32(u32(label.len))
			type_payload << label.bytes()
			type_payload << p
		}
		if cr.len == 0 {
			type_payload << u8(0x01)
			type_payload << u8(0x00)
		} else {
			type_payload << u8(0x00)
			type_payload << cr[0]
		}
	}
	for e in liftable {
		ti := if e.idx >= 0 && e.idx < func_typeidx.len { func_typeidx[e.idx] } else { -1 }
		ft := types[ti]
		type_payload << u8(0x40) // functype
		if spec := str_specs[e.name] {
			// WIT-known signature with canonical string params/results
			type_payload << leb128.encode_u32(u32(spec.comp_params.len))
			for idxp, p in spec.comp_params {
				label := spec.comp_labels[idxp]
				type_payload << leb128.encode_u32(u32(label.len))
				type_payload << label.bytes()
				type_payload << p
			}
			if spec.comp_results.len == 0 {
				type_payload << u8(0x01)
				type_payload << u8(0x00)
			} else {
				type_payload << u8(0x00)
				type_payload << spec.comp_results[0]
			}
			continue
		}
		// params
		type_payload << leb128.encode_u32(u32(ft.params.len))
		for idxp, p in ft.params {
			// label
			label := 'p${idxp}'
			type_payload << leb128.encode_u32(u32(label.len))
			type_payload << label.bytes()
			type_payload << u8(core_to_component_valtype(p))
		}
		// results
		if ft.results.len == 0 {
			type_payload << u8(0x01)
			type_payload << u8(0x00)
		} else {
			type_payload << u8(0x00)
			type_payload << u8(core_to_component_valtype(ft.results[0]))
		}
	}
	// Import section payload (component imports for host funcs). Names are
	// valid plain labels (`imp<idx>-<mod>-<field>`); the original core
	// module/field names are kept verbatim in the core-instance bindings.
	mut import_payload := []u8{}
	if forward_imports {
		import_payload << leb128.encode_u32(u32(n_imp))
		for ii, imp in core_imports {
			iname := component_host_import_name(ii, imp.mod, imp.name)
			import_payload << u8(0x00) // nameattributes plain
			import_payload << leb128.encode_u32(u32(iname.len))
			import_payload << iname.bytes()
			import_payload << u8(0x01) // sort func
			import_payload << leb128.encode_u32(u32(ii)) // type idx
		}
	}
	// Canon lowers payload (separate canon section before instances).
	mut lower_payload := []u8{}
	if forward_imports {
		lower_payload << leb128.encode_u32(u32(n_imp))
		for ii in 0 .. n_imp {
			lower_payload << u8(0x01)
			lower_payload << u8(0x00) // func sort
			lower_payload << leb128.encode_u32(u32(ii)) // component func idx
			lower_payload << leb128.encode_u32(u32(0)) // no opts
		}
	}
	// Core instance section payload: inline-export instances per module, then
	// instantiate module 0 with those instances as args.
	mut coreinst_payload := []u8{}
	coreinst_payload << leb128.encode_u32(u32(n_mod + 1)) // count
	for mi, mname in mod_order {
		coreinst_payload << u8(0x01) // inline exports
		idxs := mod_to_impidx[mname]
		coreinst_payload << leb128.encode_u32(u32(idxs.len))
		for imp_idx in idxs {
			fname := core_imports[imp_idx].name
			coreinst_payload << leb128.encode_u32(u32(fname.len))
			coreinst_payload << fname.bytes()
			coreinst_payload << u8(0x00) // core func sort
			coreinst_payload << leb128.encode_u32(u32(imp_idx)) // lowered core func idx
		}
		_ = mi
	}
	coreinst_payload << u8(0x00) // instantiate
	coreinst_payload << leb128.encode_u32(0) // module idx
	coreinst_payload << leb128.encode_u32(u32(n_mod)) // arg count
	for mi, mname in mod_order {
		coreinst_payload << leb128.encode_u32(u32(mname.len))
		coreinst_payload << mname.bytes()
		coreinst_payload << u8(0x12) // instance sort
		coreinst_payload << leb128.encode_u32(u32(mi)) // instance idx
	}
	// Alias section payload: alias core exports from the main instance. Func
	// aliases come first so liftable indices stay stable after the lowered
	// import funcs; the realloc adapter (if present) gets the next func-alias
	// index; the memory alias lives in the separate core-memory index space.
	mut func_alias_names := []string{}
	for e in liftable {
		func_alias_names << e.name
	}
	mut realloc_alias_idx := -1
	if realloc_found {
		mut found_idx := -1
		for i, n in func_alias_names {
			if n == realloc_name {
				found_idx = i
				break
			}
		}
		if found_idx >= 0 {
			realloc_alias_idx = n_imp + found_idx
		} else {
			realloc_alias_idx = n_imp + func_alias_names.len
			func_alias_names << realloc_name
		}
	}
	mut alias_payload := []u8{}
	alias_payload << leb128.encode_u32(u32(func_alias_names.len + if has_memory { 1 } else { 0 }))
	for n in func_alias_names {
		alias_payload << u8(0x00) // core sort prefix
		alias_payload << u8(0x00) // core func sort
		alias_payload << u8(0x01) // core instance
		alias_payload << leb128.encode_u32(u32(main_inst_idx)) // instance idx
		alias_payload << leb128.encode_u32(u32(n.len))
		alias_payload << n.bytes()
	}
	if has_memory {
		alias_payload << u8(0x00) // core sort prefix
		alias_payload << u8(0x02) // core memory sort
		alias_payload << u8(0x01) // core instance
		alias_payload << leb128.encode_u32(u32(main_inst_idx)) // instance idx
		alias_payload << leb128.encode_u32(u32(mem_name.len))
		alias_payload << mem_name.bytes()
	}
	// Canonical section payload: lift each
	mut canon_payload := []u8{}
	canon_payload << leb128.encode_u32(u32(liftable.len))
	for i, _ in liftable {
		canon_payload << u8(0x00)
		canon_payload << u8(0x00) // func sort
		canon_payload << leb128.encode_u32(u32(n_imp + i)) // core func idx (alias index)
		// opts: the count is the number of options, not bytes.
		mut opts := []u8{}
		mut nopts := 0
		if has_memory {
			opts << u8(0x03)
			opts << leb128.encode_u32(0)
			nopts++
		}
		if realloc_found {
			opts << u8(0x04)
			opts << leb128.encode_u32(u32(realloc_alias_idx))
			nopts++
		}
		canon_payload << leb128.encode_u32(u32(nopts))
		canon_payload << opts
		canon_payload << leb128.encode_u32(u32(n_imp + i)) // type idx
	}
	// Export section payload: each export ends with the mandatory byte for an
	// absent optional export type (0x00); the decoder reads it unconditionally.
	mut export_payload := []u8{}
	export_payload << leb128.encode_u32(u32(liftable.len))
	for i, e in liftable {
		mut ename := e.name
		if spec := str_specs[e.name] {
			ename = spec.wit_name
		}
		export_payload << u8(0x00) // nameattributes plain
		export_payload << leb128.encode_u32(u32(ename.len))
		export_payload << ename.bytes()
		export_payload << u8(0x01) // sort func
		export_payload << leb128.encode_u32(u32(n_imp + i)) // func idx
		export_payload << u8(0x00) // absent optional externtype
	}
	// Assemble
	mut out := []u8{}
	out << u8(0x00)
	out << u8(0x61)
	out << u8(0x73)
	out << u8(0x6d)
	out << u8(0x0d)
	out << u8(0x00)
	out << u8(0x01)
	out << u8(0x00)
	// type section id 7
	write_section(mut out, int(ComponentSection.type_section), type_payload)
	// core module section id 1
	mut mod_payload := []u8{}
	mod_payload << core
	write_section(mut out, int(ComponentSection.core_module_section), mod_payload)
	if forward_imports {
		// import section id 10
		write_section(mut out, int(ComponentSection.import_section), import_payload)
		// canon lowers section id 8 (before instances)
		write_section(mut out, int(ComponentSection.canon_section), lower_payload)
	}
	// core instance section id 2
	write_section(mut out, int(ComponentSection.core_instance_section), coreinst_payload)
	// alias section id 6
	write_section(mut out, int(ComponentSection.alias_section), alias_payload)
	// canon lifts section id 8
	write_section(mut out, int(ComponentSection.canon_section), canon_payload)
	// export section id 11
	write_section(mut out, int(ComponentSection.export_section), export_payload)
	return out
}

pub fn component_preamble() []u8 {
	return [u8(0x00), 0x61, 0x73, 0x6d, 0x0d, 0x00, 0x01, 0x00]
}

pub fn is_component(bytes []u8) bool {
	if bytes.len < 8 { return false }
	return bytes[0] == 0x00 && bytes[1] == 0x61 && bytes[2] == 0x73 && bytes[3] == 0x6d
		&& bytes[4] == 0x0d && bytes[5] == 0x00 && bytes[6] == 0x01 && bytes[7] == 0x00
}
