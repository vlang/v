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

// parse core wasm module: returns func exports and func type info.
fn parse_core_module(core []u8) ([]CoreExport, []CoreFuncType, []int, int, []int) {
	mut exports := []CoreExport{}
	mut types := []CoreFuncType{}
	mut import_func_types := []int{}
	mut defined_func_typeidx := []int{}
	mut import_func_count := 0
	if core.len < 8 { return exports, types, defined_func_typeidx, 0, []int{} }
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
				read_name(core, &i) // module name
				read_name(core, &i) // field name
				kind := core[i]
				i++
				if kind == 0 {
					ti := read_uleb128(core, &i)
					import_func_types << ti
					import_func_count++
				} else if kind == 1 {
					// table
					i++ // elem type
					// limits
					ft := core[i]
					i++
					read_uleb128(core, &i)
					if ft & 0x01 != 0 { read_uleb128(core, &i) }
				} else if kind == 2 {
					// memory
					ft := core[i]
					i++
					read_uleb128(core, &i)
					if ft & 0x01 != 0 { read_uleb128(core, &i) }
				} else if kind == 3 {
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
	for t in import_func_types { func_typeidx << t }
	for t in defined_func_typeidx { func_typeidx << t }
	return exports, types, defined_func_typeidx, import_func_count, func_typeidx
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

fn write_section(mut out []u8, id u8, payload []u8) {
	out << id
	out << leb128.encode_u32(u32(payload.len))
	out << payload
}

// ---- public API ----

pub fn wrap_core_module(core []u8) []u8 {
	// If we can lift at least one function, produce a richer component.
	exports, types, _defined, _, func_typeidx := parse_core_module(core)
	// Detect memory and realloc exports for canonical opts.
	mut has_memory := false
	mut realloc_idx := -1
	for e in exports {
		if e.kind == 2 { has_memory = true }
		if e.kind == 0 && (e.name == 'cabi_realloc' || e.name == 'realloc') {
			realloc_idx = e.idx
		}
	}
	// Determine liftable exports.
	mut liftable := []CoreExport{}
	for e in exports {
		if e.kind != 0 { continue }
		// map export idx to typeidx
		ti := if e.idx >= 0 && e.idx < func_typeidx.len { func_typeidx[e.idx] } else { -1 }
		if ti < 0 || ti >= types.len { continue }
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
			if ft.results.len > 1 { ok = false }
			for r in ft.results {
				if core_to_component_valtype(r) < 0 {
					ok = false
					break
				}
			}
		}
		if ok { liftable << e }
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

	// Type section payload
	mut type_payload := []u8{}
	type_payload << leb128.encode_u32(u32(liftable.len))
	for e in liftable {
		ti := if e.idx >= 0 && e.idx < func_typeidx.len { func_typeidx[e.idx] } else { -1 }
		ft := types[ti]
		type_payload << u8(0x40) // functype
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

	// Core instance section payload: instantiate module 0
	mut coreinst_payload := []u8{}
	coreinst_payload << leb128.encode_u32(1) // count
	coreinst_payload << u8(0x00) // instantiate
	coreinst_payload << leb128.encode_u32(0) // module idx
	coreinst_payload << leb128.encode_u32(0) // arg count

	// Alias section payload: alias core exports
	mut alias_payload := []u8{}
	alias_payload << leb128.encode_u32(u32(liftable.len))
	for e in liftable {
		alias_payload << u8(0x00) // core sort prefix
		alias_payload << u8(0x00) // core func sort
		alias_payload << u8(0x01) // core instance
		alias_payload << leb128.encode_u32(0) // instance idx
		alias_payload << leb128.encode_u32(u32(e.name.len))
		alias_payload << e.name.bytes()
	}

	// Canonical section payload: lift each
	mut canon_payload := []u8{}
	canon_payload << leb128.encode_u32(u32(liftable.len))
	for i, _ in liftable {
		canon_payload << u8(0x00)
		canon_payload << u8(0x00) // func sort
		canon_payload << leb128.encode_u32(u32(i)) // core func idx (alias index)
		// opts
		mut opts := []u8{}
		if has_memory {
			opts << u8(0x03)
			opts << leb128.encode_u32(0)
		}
		if realloc_idx >= 0 {
			opts << u8(0x04)
			opts << leb128.encode_u32(u32(realloc_idx))
		}
		canon_payload << leb128.encode_u32(u32(opts.len))
		canon_payload << opts
		canon_payload << leb128.encode_u32(u32(i)) // type idx
	}

	// Export section payload
	mut export_payload := []u8{}
	export_payload << leb128.encode_u32(u32(liftable.len))
	for i, e in liftable {
		export_payload << u8(0x00) // nameattributes plain
		export_payload << leb128.encode_u32(u32(e.name.len))
		export_payload << e.name.bytes()
		export_payload << u8(0x01) // sort func
		export_payload << leb128.encode_u32(u32(i)) // func idx
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
	// core instance section id 2
	write_section(mut out, int(ComponentSection.core_instance_section), coreinst_payload)
	// alias section id 6
	write_section(mut out, int(ComponentSection.alias_section), alias_payload)
	// canon section id 8
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
