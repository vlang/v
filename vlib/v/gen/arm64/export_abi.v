module arm64

import v.ssa

// Entries for `@[export]` functions that C calls with the C calling convention.
//
// Native V functions pass every aggregate in general registers (two words at
// most) or by address. The AAPCS64 passes and returns a homogeneous float
// aggregate (HFA: up to four f32 or f64 members, `struct { double x, y; }`)
// in d registers instead, so a C caller of such a function would read and
// write the wrong registers. Like the outbound C-call path, which moves HFAs
// into d registers for the callee, an exported function with an HFA parameter
// or result gets an entry under its C name that takes the C convention,
// rebuilds the aggregates in its frame, calls the native body, and moves an
// HFA result back into d registers.

// Arm64ExportFrame is the layout of an export entry's frame, from sp: the
// incoming x0-x7, d0-d7 and x8 (the C caller's indirect result address), then
// the aggregates the entry rebuilds.
const arm64_export_saved_x = 0
const arm64_export_saved_d = 64
const arm64_export_saved_x8 = 128
const arm64_export_buffers = 136

// Arm64ExportLoad moves one native argument register from the entry's frame.
struct Arm64ExportLoad {
	is_float bool // d register (otherwise x)
	is_f32   bool
	reg      int
	// is_address loads the address of the frame offset, not its contents.
	is_address bool
	offset     int
}

// Arm64ExportCopy copies one HFA member from its saved d register into the
// aggregate rebuilt in the frame.
struct Arm64ExportCopy {
	is_f32 bool
	from   int
	to     int
}

struct Arm64ExportEntry {
mut:
	loads  []Arm64ExportLoad
	copies []Arm64ExportCopy
	// ret_hfa is set when the result is an HFA; the native body returns it in
	// x0/x1, or, above 16 bytes, through x8 into ret_buffer.
	ret_hfa       Arm64HfaLayout
	ret_buffer    int
	ret_indirect  bool // the native body takes x8 (an HFA buffer or the caller's x8)
	ret_hfa_large bool
	frame_size    int
}

// export_needs_c_entry reports whether a C caller of `func` would pass its
// arguments or expect its result differently than the native body does.
fn (g &Gen) export_needs_c_entry(func &ssa.Function) bool {
	if _ := g.c_homogeneous_float_aggregate(func.typ) {
		return true
	}
	for pid in func.params {
		if _ := g.c_homogeneous_float_aggregate(g.m.values[pid].typ) {
			return true
		}
	}
	return false
}

// plan_export_entry maps the C argument registers of `func` onto the native
// ones. It returns none when an argument would be passed on the stack by
// either convention, which the entries do not translate.
fn (g &Gen) plan_export_entry(func &ssa.Function) ?Arm64ExportEntry {
	if func.is_variadic {
		return none
	}
	mut entry := Arm64ExportEntry{}
	mut buffer := arm64_export_buffers
	mut c_x := 0
	mut c_d := 0
	mut native_x := 0
	mut native_d := 0
	for pid in func.params {
		typ := g.m.values[pid].typ
		if g.is_float_type(typ) {
			if c_d >= 8 || native_d >= 8 {
				return none
			}
			entry.loads << Arm64ExportLoad{
				is_float: true
				is_f32:   g.is_f32_type(typ)
				reg:      native_d
				offset:   arm64_export_saved_d + c_d * 8
			}
			c_d++
			native_d++
			continue
		}
		size := g.m.type_size(typ)
		if hfa := g.c_homogeneous_float_aggregate(typ) {
			if c_d + hfa.elements.len > 8 {
				return none
			}
			for i, element in hfa.elements {
				entry.copies << Arm64ExportCopy{
					is_f32: g.is_f32_type(element.typ)
					from:   arm64_export_saved_d + (c_d + i) * 8
					to:     buffer + element.offset
				}
			}
			c_d += hfa.elements.len
			if g.is_large_struct_type(typ) {
				if native_x >= 8 {
					return none
				}
				entry.loads << Arm64ExportLoad{
					reg:        native_x
					is_address: true
					offset:     buffer
				}
				native_x++
			} else {
				words := (size + 7) / 8
				if native_x + words > 8 {
					return none
				}
				for w in 0 .. words {
					entry.loads << Arm64ExportLoad{
						reg:    native_x + w
						offset: buffer + w * 8
					}
				}
				native_x += words
			}
			buffer += (size + 7) & ~7
			continue
		}
		// Integers, pointers and other aggregates use the same general
		// registers in both conventions; an aggregate above 16 bytes is passed
		// by address in both.
		words := if g.is_large_struct_type(typ) || size <= 8 { 1 } else { (size + 7) / 8 }
		if g.is_value_aggregate_type(typ) && words == 2 && g.m.type_align(typ) >= 16 {
			// AAPCS64 starts a 16-byte aligned pair on an even register.
			c_x = (c_x + 1) & ~1
		}
		if c_x + words > 8 || native_x + words > 8 {
			return none
		}
		for w in 0 .. words {
			entry.loads << Arm64ExportLoad{
				reg:    native_x + w
				offset: arm64_export_saved_x + (c_x + w) * 8
			}
		}
		c_x += words
		native_x += words
	}
	if hfa := g.c_homogeneous_float_aggregate(func.typ) {
		entry.ret_hfa = hfa
		entry.ret_buffer = buffer
		entry.ret_hfa_large = g.is_large_struct_type(func.typ)
		entry.ret_indirect = entry.ret_hfa_large
		buffer += (g.m.type_size(func.typ) + 15) & ~15
	} else if g.is_large_struct_type(func.typ) {
		entry.ret_indirect = true
	}
	entry.frame_size = (buffer + 15) & ~15
	if entry.frame_size >= 4096 {
		return none
	}
	return entry
}

// gen_export_entries emits the C entries of the exported functions that need
// one (see export_needs_c_entry) and returns the C names they serve.
fn (mut g Gen) gen_export_entries() map[string]bool {
	mut served := map[string]bool{}
	mut names := g.m.export_aliases.keys()
	names.sort()
	for fn_name in names {
		cname := g.m.export_aliases[fn_name]
		if '_' + cname in force_external_syms && !g.freestanding {
			continue
		}
		// The parallel generator does not keep fn_offsets; the symbol has it.
		body_sym := g.macho.sym_by_name['_' + fn_name] or { continue }
		if g.macho.symbols[body_sym].sect != 1 {
			continue
		}
		body := int(g.macho.symbols[body_sym].value)
		func_idx := g.export_function_index(fn_name) or { continue }
		func := g.m.funcs[func_idx]
		if func.blocks.len == 0 || !g.export_needs_c_entry(&func) {
			continue
		}
		entry := g.plan_export_entry(&func) or { continue }
		if cname == fn_name {
			// The body has the C name: move it aside so the name reaches the
			// entry. V code calls the body by offset, and takes its address
			// through the symbol index, which the rename keeps.
			g.rename_text_symbol('_' + fn_name, '_' + fn_name + '.v_native')
		}
		start := g.text_offset()
		g.macho.add_symbol('_' + cname, u64(start), true, 1)
		g.emit_export_entry(entry, body)
		served[cname] = true
	}
	return served
}

fn (g &Gen) export_function_index(fn_name string) ?int {
	for i, func in g.m.funcs {
		if func.name == fn_name && !func.is_c_extern {
			return i
		}
	}
	return none
}

fn (mut g Gen) rename_text_symbol(old_name string, new_name string) {
	idx := g.macho.sym_by_name[old_name] or { return }
	g.macho.symbols[idx].name = new_name
	g.macho.sym_by_name.delete(old_name)
	g.macho.sym_by_name[new_name] = idx
	if pairs := g.macho.unwind_saved_pairs[old_name] {
		g.macho.unwind_saved_pairs.delete(old_name)
		g.macho.unwind_saved_pairs[new_name] = pairs
	}
}

fn (mut g Gen) emit_export_entry(entry Arm64ExportEntry, body int) {
	g.emit32(asm_stp_fp_lr_pre())
	g.emit32(asm_mov_fp_sp())
	g.emit32(asm_sub_imm(sp, sp, u32(entry.frame_size)))
	for i in 0 .. 4 {
		g.emit32(asm_stp_offset(Reg(2 * i), Reg(2 * i + 1), sp,
			i32((arm64_export_saved_x + i * 16) / 8)))
		g.emit32(asm_stp_d_offset(2 * i, 2 * i + 1, sp, i32((arm64_export_saved_d + i * 16) / 8)))
	}
	g.emit32(asm_str_imm(Reg(8), sp, u32(arm64_export_saved_x8 / 8)))
	// Rebuild the HFA arguments from the d registers they arrived in.
	for copy in entry.copies {
		if copy.is_f32 {
			g.emit32(asm_ldr_imm_w(Reg(9), sp, u32(copy.from / 4)))
			g.emit32(asm_str_imm_w(Reg(9), sp, u32(copy.to / 4)))
		} else {
			g.emit32(asm_ldr_imm(Reg(9), sp, u32(copy.from / 8)))
			g.emit32(asm_str_imm(Reg(9), sp, u32(copy.to / 8)))
		}
	}
	// Every load reads the frame, so no argument register is overwritten
	// before it was saved.
	for load in entry.loads {
		if load.is_address {
			g.emit32(asm_add_imm(Reg(load.reg), sp, u32(load.offset)))
		} else if load.is_float {
			g.emit32(asm_ldr_d_imm(load.reg, sp, u32(load.offset / 8)))
		} else {
			g.emit32(asm_ldr_imm(Reg(load.reg), sp, u32(load.offset / 8)))
		}
	}
	if entry.ret_hfa_large {
		g.emit32(asm_add_imm(Reg(8), sp, u32(entry.ret_buffer)))
	} else if entry.ret_indirect {
		g.emit32(asm_ldr_imm(Reg(8), sp, u32(arm64_export_saved_x8 / 8)))
	}
	g.emit32(asm_bl(i32((body - g.text_offset()) / 4)))
	if entry.ret_hfa.elements.len > 0 {
		if !entry.ret_hfa_large {
			g.emit32(asm_stp_offset(Reg(0), Reg(1), sp, i32(entry.ret_buffer / 8)))
		}
		for i, element in entry.ret_hfa.elements {
			offset := entry.ret_buffer + element.offset
			if g.is_f32_type(element.typ) {
				g.emit32(asm_ldr_s_imm(i, sp, u32(offset / 4)))
			} else {
				g.emit32(asm_ldr_d_imm(i, sp, u32(offset / 8)))
			}
		}
	}
	g.emit32(asm_add_imm(sp, fp, 0))
	g.emit32(asm_ldp_fp_lr_post())
	g.emit32(asm_ret())
}
