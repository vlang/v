// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

// Mach-O Constants for ARM64
const mh_magic_64 = u32(0xfeedfacf)
const cpu_type_arm64 = 0x0100000c
const cpu_subtype_arm64_all = 0

const lc_segment_64 = 0x19
const lc_symtab = 0x2

const arm64_reloc_branch26 = 2
const arm64_reloc_page21 = 3
const arm64_reloc_pageoff12 = 4
const arm64_reloc_got_load_page21 = 5
const arm64_reloc_got_load_pageoff12 = 6

// MachOObject represents mach oobject data used by arm64.
pub struct MachOObject {
pub mut:
	text_data []u8
	str_data  []u8
	data_data []u8

	relocs      []RelocationInfo
	symbols     []Symbol
	str_table   []u8
	sym_by_name map[string]int // symbol name → index in symbols
	// callee-saved register pairs each function saves, for `__compact_unwind`
	unwind_saved_pairs map[string]int
}

// RelocationInfo stores relocation info metadata used by arm64.
struct RelocationInfo {
	addr    int
	sym_idx int
	pcrel   bool
	length  int
	extern  bool
	type_   int
}

// Symbol represents symbol data used by arm64.
struct Symbol {
mut:
	name     string
	type_    u8
	sect     u8
	desc     u16
	value    u64
	name_off int
}

// new creates a MachOObject value for arm64.
pub fn MachOObject.new() &MachOObject {
	mut m := &MachOObject{
		text_data:          []u8{}
		str_data:           []u8{}
		data_data:          []u8{}
		relocs:             []RelocationInfo{}
		symbols:            []Symbol{}
		str_table:          [u8(0)]
		sym_by_name:        map[string]int{}
		unwind_saved_pairs: map[string]int{}
	}
	return m
}

// add_symbol updates add symbol state for MachOObject.
pub fn (mut m MachOObject) add_symbol(name string, addr u64, is_ext bool, sect u8) int {
	typ := if is_ext { u8(0x0f) } else { u8(0x0e) } // N_SECT | N_EXT : N_SECT

	// Check if symbol already exists (e.g., was added as undefined earlier)
	if i := m.sym_by_name[name] {
		mut s := &m.symbols[i]
		s.type_ = typ
		s.sect = sect
		s.value = addr
		return i
	}

	// Add new symbol
	idx := m.symbols.len
	name_off := m.str_table.len
	unsafe { m.str_table.push_many(name.str, name.len) }
	m.str_table << 0

	m.symbols << Symbol{
		name:     name
		type_:    typ
		sect:     sect
		desc:     0
		value:    addr
		name_off: name_off
	}
	m.sym_by_name[name] = idx
	return idx
}

// add_undefined updates add undefined state for MachOObject.
pub fn (mut m MachOObject) add_undefined(name string) int {
	// Check for any existing symbol with this name (defined or undefined)
	if i := m.sym_by_name[name] {
		return i
	}

	idx := m.symbols.len
	name_off := m.str_table.len
	unsafe { m.str_table.push_many(name.str, name.len) }
	m.str_table << 0

	m.symbols << Symbol{
		name:     name
		type_:    0x01 // N_UNDF | N_EXT
		sect:     0
		desc:     0
		value:    0
		name_off: name_off
	}
	m.sym_by_name[name] = idx
	return idx
}

// add_reloc updates add reloc state for MachOObject.
pub fn (mut m MachOObject) add_reloc(addr int, sym_idx int, typ int, pcrel bool) {
	m.relocs << RelocationInfo{
		addr:    addr
		sym_idx: sym_idx // 0-based symbol table index
		type_:   typ
		pcrel:   pcrel
		length:  2
		extern:  true
	}
}

// MachOObjectOptions configures how write_object turns the generated code into a
// relocatable object for the system linker.
pub struct MachOObjectOptions {
pub:
	// freestanding keeps the program's own definitions of libc-named symbols.
	freestanding bool
	// exported lists the symbols C code may reference: `_main` and the
	// `@[export]` names.
	exported map[string]bool
	// min_os is the minimum macOS version recorded in LC_BUILD_VERSION, as
	// `major.minor` (default 11.0, the first macOS release for Apple silicon).
	min_os string = '11.0'
}

const mh_object = 1
const lc_dysymtab_cmd = 0xb
const lc_build_version_cmd = 0x32
const platform_macos = 1
const n_ext = u8(0x01)
const n_undf = u8(0x00)
const n_sect = u8(0x0e)
const s_regular = u32(0)
const s_zerofill = u32(1)
const s_attr_debug = u32(0x02000000)
const unwind_arm64_mode_frame = u32(0x04000000)
const s_attr_pure_instructions = u32(0x80000000)
const s_attr_some_instructions = u32(0x00000400)
const compact_unwind_entry_size = 32

struct CompactUnwindFunction {
	symbol int
	length u32
}

// encode_macos_version packs `major.minor[.patch]` as the X.Y.Z nibble format of
// LC_BUILD_VERSION (xxxx.yy.zz).
fn encode_macos_version(version string) u32 {
	parts := version.split('.')
	major := if parts.len > 0 { parts[0].u32() } else { u32(11) }
	minor := if parts.len > 1 { parts[1].u32() } else { u32(0) }
	patch := if parts.len > 2 { parts[2].u32() } else { u32(0) }
	return (major << 16) | ((minor & 0xff) << 8) | (patch & 0xff)
}

// write_object writes a relocatable MH_OBJECT that Apple's ld (or ld64.lld)
// can link together with C objects, static archives, dylibs and frameworks.
// This is the "external linking" mode (like Go's `-linkmode=external`): the
// built-in linker is still used for programs that need nothing but libSystem.
//
// Layout follows what clang emits for arm64:
// - sections __TEXT,__text, __TEXT,__const (string literals) and __DATA,__data,
//   at the same addresses the generator used for its symbol values;
// - symbols ordered locals, external definitions, undefined, as LC_DYSYMTAB
//   requires;
// - LC_BUILD_VERSION, so the linker knows the platform and does not warn.
//
// String literals go to a regular `__const` section instead of `__cstring`:
// the linker splits `S_CSTRING_LITERALS` at NUL bytes and may merge or move the
// pieces, which breaks V literals that contain NULs or are referenced with an
// offset.
//
// Hosted V wrappers such as `malloc` or `exit` share their name with the libc
// function they call. The built-in linker sends every reference to those names
// to libSystem; the object does the same by renaming the V definitions to
// private symbols, so neither V code nor C libraries linked next to it can
// recurse into the wrapper.
//
// Like Go's machosymtab1 (cmd/link/internal/ld/macho.go), which marks a symbol
// N_EXT only when it is exported, V's own definitions stay local to the object:
// `fn print` or a runtime helper with the name of a C function (`v3_pthread_create`
// and pthread_helper.o) must not clash with C code linked next to it. Only
// `opts.exported` names and the program's own libc-named definitions stay external.
pub fn (mut m MachOObject) write_object(path string, opts MachOObjectOptions) ! {
	text_len := m.text_data.len
	const_addr := text_len
	const_len := m.str_data.len
	data_addr := (const_addr + const_len + 15) & ~15
	// Trailing zero bytes are not stored: they become `__bss`, as a C compiler
	// emits zero-initialized globals (S_ZEROFILL).
	data_file_len := m.data_file_len()
	bss_addr_in_data := data_addr + data_file_len
	bss_len := m.data_data.len - data_file_len
	unwind_addr := (data_addr + data_file_len + 7) & ~7

	// Generator symbol values are already the section addresses used below: text
	// offsets, `text_len + string offset` and `align8(text_len + strings) + data
	// offset` (see Gen.gen_post_pass).
	// Symbol table: locals, then external definitions, then undefined symbols.
	mut local_syms := []int{}
	mut ext_syms := []int{}
	mut undef_syms := []int{}
	mut local_runtime := map[string]bool{}
	mut force_external := map[string]bool{}
	for name in force_external_syms {
		force_external[name] = true
	}
	if opts.freestanding {
		for sym in m.symbols {
			if sym.type_ & n_sect == n_sect && sym.name in force_external {
				local_runtime[sym.name] = true
			}
		}
	}
	// shadowed: defined libc-named wrappers whose references must go to libc.
	mut shadowed := []bool{len: m.symbols.len}
	mut shadowed_count := 0
	for i, sym in m.symbols {
		if sym.type_ & n_sect == n_sect {
			if sym.name in force_external && sym.name !in local_runtime {
				shadowed[i] = true
				shadowed_count++
				local_syms << i
				undef_syms << i
			} else if sym.type_ & n_ext != 0 && (opts.exported[sym.name] || sym.name in local_runtime) {
				ext_syms << i
			} else {
				local_syms << i
			}
		} else {
			undef_syms << i
		}
	}
	ext_syms.sort_with_compare(fn [m] (a &int, b &int) int {
		return compare_strings(m.symbols[*a].name, m.symbols[*b].name)
	})
	undef_syms.sort_with_compare(fn [m] (a &int, b &int) int {
		return compare_strings(m.symbols[*a].name, m.symbols[*b].name)
	})

	// The `__bss` symbols move with their section, after `__compact_unwind`.
	unwind_functions := m.compact_unwind_functions(shadowed)
	unwind_size := unwind_functions.len * compact_unwind_entry_size
	final_bss_addr := (unwind_addr + unwind_size + 15) & ~15
	bss_section := fn [bss_addr_in_data] (sym Symbol) u8 {
		return if sym.sect == 3 && sym.value >= u64(bss_addr_in_data) { u8(5) } else { sym.sect }
	}
	bss_value := fn [bss_addr_in_data, final_bss_addr] (sym Symbol) u64 {
		return if sym.sect == 3 && sym.value >= u64(bss_addr_in_data) {
			sym.value - u64(bss_addr_in_data) + u64(final_bss_addr)
		} else {
			sym.value
		}
	}
	mut strtab := []u8{cap: m.str_table.len + 64}
	strtab << 0
	mut symtab := []u8{cap: (m.symbols.len + shadowed_count) * 16}
	// reloc_index maps an old symbol index to the index relocations must use.
	mut reloc_index := []int{len: m.symbols.len}
	mut out_index := 0
	for i in local_syms {
		sym := m.symbols[i]
		mut name := sym.name
		if shadowed[i] {
			name = 'ltmp_v_shadowed${name}'
		} else {
			reloc_index[i] = out_index
		}
		write_nlist(mut symtab, mut strtab, name, n_sect, bss_section(sym), 0, bss_value(sym))
		out_index++
	}
	for i in ext_syms {
		sym := m.symbols[i]
		reloc_index[i] = out_index
		write_nlist(mut symtab, mut strtab, sym.name, n_sect | n_ext, bss_section(sym), sym.desc,
			bss_value(sym))
		out_index++
	}
	for i in undef_syms {
		sym := m.symbols[i]
		reloc_index[i] = out_index
		write_nlist(mut symtab, mut strtab, sym.name, n_undf | n_ext, 0, sym.desc, 0)
		out_index++
	}
	for strtab.len % 8 != 0 {
		strtab << 0
	}
	n_syms := out_index

	unwind_entries, unwind_relocs := m.compact_unwind(unwind_functions, reloc_index)
	unwind_len := unwind_entries.len
	n_sects := 5
	bss_addr := (unwind_addr + unwind_len + 15) & ~15
	header_size := 32
	seg_cmd_size := 72 + 80 * n_sects
	build_version_cmd_size := 24
	symtab_cmd_size := 24
	dysymtab_cmd_size := 80
	load_cmds_size := seg_cmd_size + build_version_cmd_size + symtab_cmd_size + dysymtab_cmd_size

	text_off := (header_size + load_cmds_size + 15) & ~15
	const_off := text_off + const_addr
	data_off := text_off + data_addr
	seg_filesize := unwind_addr + unwind_len
	seg_vmsize := bss_addr + bss_len
	reloc_off := (text_off + seg_filesize + 7) & ~7
	unwind_reloc_off := reloc_off + m.relocs.len * 8
	sym_off := unwind_reloc_off + unwind_relocs.len
	str_off := sym_off + symtab.len

	mut buf := []u8{cap: str_off + strtab.len}
	write_mh_magic_64(mut buf)
	write_u32_le(mut buf, u32(cpu_type_arm64))
	write_u32_le(mut buf, u32(cpu_subtype_arm64_all))
	write_u32_le(mut buf, mh_object)
	write_u32_le(mut buf, 4) // ncmds
	write_u32_le(mut buf, u32(load_cmds_size))
	write_u32_le(mut buf, 0) // flags
	write_u32_le(mut buf, 0) // reserved

	// LC_SEGMENT_64 (unnamed, as in every relocatable object)
	write_u32_le(mut buf, u32(lc_segment_64))
	write_u32_le(mut buf, u32(seg_cmd_size))
	write_string_fixed(mut buf, '', 16)
	write_u64_le(mut buf, 0) // vmaddr
	write_u64_le(mut buf, u64(seg_vmsize)) // vmsize
	write_u64_le(mut buf, u64(text_off)) // fileoff
	write_u64_le(mut buf, u64(seg_filesize)) // filesize
	write_u32_le(mut buf, 7) // maxprot
	write_u32_le(mut buf, 7) // initprot
	write_u32_le(mut buf, u32(n_sects))
	write_u32_le(mut buf, 0)

	write_section_64(mut buf, '__text', '__TEXT', 0, text_len, text_off, 2, reloc_off,
		m.relocs.len, s_regular | s_attr_pure_instructions | s_attr_some_instructions)
	write_section_64(mut buf, '__const', '__TEXT', const_addr, const_len, const_off, 0, 0, 0,
		s_regular)
	write_section_64(mut buf, '__data', '__DATA', data_addr, data_file_len, data_off, 4, 0, 0,
		s_regular)
	write_section_64(mut buf, '__compact_unwind', '__LD', unwind_addr, unwind_len,
		text_off + unwind_addr, 3, unwind_reloc_off, unwind_relocs.len / 8, s_attr_debug)
	write_section_64(mut buf, '__bss', '__DATA', bss_addr, bss_len, 0, 4, 0, 0, s_zerofill)

	// LC_BUILD_VERSION
	write_u32_le(mut buf, u32(lc_build_version_cmd))
	write_u32_le(mut buf, u32(build_version_cmd_size))
	write_u32_le(mut buf, platform_macos)
	write_u32_le(mut buf, encode_macos_version(opts.min_os))
	write_u32_le(mut buf, 0) // sdk: unknown
	write_u32_le(mut buf, 0) // ntools

	// LC_SYMTAB
	write_u32_le(mut buf, u32(lc_symtab))
	write_u32_le(mut buf, u32(symtab_cmd_size))
	write_u32_le(mut buf, u32(sym_off))
	write_u32_le(mut buf, u32(n_syms))
	write_u32_le(mut buf, u32(str_off))
	write_u32_le(mut buf, u32(strtab.len))

	// LC_DYSYMTAB
	write_u32_le(mut buf, u32(lc_dysymtab_cmd))
	write_u32_le(mut buf, u32(dysymtab_cmd_size))
	write_u32_le(mut buf, 0) // ilocalsym
	write_u32_le(mut buf, u32(local_syms.len))
	write_u32_le(mut buf, u32(local_syms.len)) // iextdefsym
	write_u32_le(mut buf, u32(ext_syms.len))
	write_u32_le(mut buf, u32(local_syms.len + ext_syms.len)) // iundefsym
	write_u32_le(mut buf, u32(undef_syms.len))
	for _ in 0 .. 12 {
		write_u32_le(mut buf, 0)
	}

	got_pairs := m.got_address_pairs(undef_syms)
	text := m.text_with_got_addresses(got_pairs)
	for buf.len < text_off {
		buf << 0
	}
	buf << text
	buf << m.str_data
	for buf.len < data_off {
		buf << 0
	}
	buf << m.data_data[..data_file_len]
	for buf.len < text_off + unwind_addr {
		buf << 0
	}
	buf << unwind_entries
	for buf.len < reloc_off {
		buf << 0
	}
	buf << m.object_reloc_bytes(got_pairs, reloc_index)
	buf << unwind_relocs
	buf << symtab
	buf << strtab

	if !write_file_array_raw(path, buf) {
		return error('failed to write Mach-O object `${path}`')
	}
}

// data_file_len returns how much of the data must be stored, aligned so `__bss`
// can start right after it.
fn (m &MachOObject) data_file_len() int {
	mut end := m.data_data.len
	for end > 0 && m.data_data[end - 1] == 0 {
		end--
	}
	return arm64_min_int((end + 15) & ~15, m.data_data.len)
}

// compact_unwind_functions returns the symbols of the functions that get a
// `__compact_unwind` entry, by address.
fn (m &MachOObject) compact_unwind_functions(shadowed []bool) []CompactUnwindFunction {
	mut symbols := []int{}
	for i, sym in m.symbols {
		if sym.type_ & n_sect == n_sect && sym.sect == 1 {
			symbols << i
		}
	}
	symbols.sort_with_compare(fn [m] (a &int, b &int) int {
		return int(i64(m.symbols[*a].value) - i64(m.symbols[*b].value))
	})
	mut functions := []CompactUnwindFunction{cap: symbols.len}
	for position, index in symbols {
		start := m.symbols[index].value
		end := if position + 1 < symbols.len {
			m.symbols[symbols[position + 1]].value
		} else {
			u64(m.text_data.len)
		}
		if end > start && !shadowed[index] {
			functions << CompactUnwindFunction{
				symbol: index
				length: u32(end - start)
			}
		}
	}
	return functions
}

// compact_unwind returns the `__LD,__compact_unwind` entries of the object, one
// per function, and their relocations. Every V function starts with
// `stp x29, x30, [sp, #-16]!; mov x29, sp`, so each is described by the frame
// mode encoding. With it the system linker builds `__unwind_info`, and an
// Objective-C exception or an `_Unwind_Backtrace` can cross V frames. Layout as
// in Zig's src/link/MachO/UnwindInfo.zig, which reads what compilers emit here.
fn (m &MachOObject) compact_unwind(functions []CompactUnwindFunction, reloc_index []int) ([]u8, []u8) {
	mut entries := []u8{cap: functions.len * compact_unwind_entry_size}
	mut relocs := []u8{cap: functions.len * 8}
	for function in functions {
		write_u32_le(mut relocs, u32(entries.len))
		// r_symbolnum, r_length 3 (8 bytes), r_extern, ARM64_RELOC_UNSIGNED
		write_u32_le(mut relocs, (u32(reloc_index[function.symbol]) & 0xffffff) | (3 << 25) | (1 << 27))
		write_u64_le(mut entries, 0) // the function: an external relocation's addend
		write_u32_le(mut entries, function.length)
		saved_pairs := m.unwind_saved_pairs[m.symbols[function.symbol].name] or { 0 }
		x_pairs := u32(saved_pairs) & 0xf
		d_pairs := (u32(saved_pairs) >> 4) & 0xf
		write_u32_le(mut entries, unwind_arm64_mode_frame | ((u32(1) << x_pairs) - 1) | (((u32(1) << d_pairs) - 1) << 8))
		write_u64_le(mut entries, 0) // personality
		write_u64_le(mut entries, 0) // LSDA
	}
	return entries, relocs
}

// The generator takes the address of a C function with ADRP + ADD, which the
// built-in linker points at the function's stub. A system linker cannot give a
// dylib symbol an address that way (ld64: "does not have address"), so in an
// object the pairs that name an undefined symbol load the address from the GOT
// instead: ADRP + LDR with GOT_LOAD relocations, as clang emits them.
fn (m &MachOObject) got_address_pairs(undef_syms []int) []bool {
	mut undefined := []bool{len: m.symbols.len}
	for i in undef_syms {
		undefined[i] = true
	}
	// pairs flags each relocation that moves to the GOT. The generator emits the
	// ADRP relocation right before the page-offset one it belongs to.
	mut pairs := []bool{len: m.relocs.len}
	for i, r in m.relocs {
		if i == 0 || r.type_ != arm64_reloc_pageoff12 || !undefined[r.sym_idx] {
			continue
		}
		page := m.relocs[i - 1]
		if page.type_ != arm64_reloc_page21 || page.addr != r.addr - 4 || page.sym_idx != r.sym_idx {
			continue
		}
		if read_u32_le(m.text_data, r.addr) & 0xffc00000 != 0x91000000 {
			continue
		}
		pairs[i] = true
		pairs[i - 1] = true
	}
	return pairs
}

fn (m &MachOObject) text_with_got_addresses(pairs []bool) []u8 {
	mut text := m.text_data.clone()
	for i, moves_to_got in pairs {
		r := m.relocs[i]
		if moves_to_got && r.type_ == arm64_reloc_pageoff12 {
			// add xd, xn, #off -> ldr xd, [xn, #off]
			instr := read_u32_le(text, r.addr)
			write_u32_le_at_arr(mut text, r.addr, 0xf9400000 | (instr & 0x3ff))
		}
	}
	return text
}

// object_reloc_bytes encodes the text relocations in the descending address
// order ld64 expects, as compilers emit them. The generator records them in
// ascending order, so they only need reversing.
fn (m &MachOObject) object_reloc_bytes(pairs []bool, reloc_index []int) []u8 {
	mut order := []int{len: m.relocs.len, init: index}
	mut ascending := true
	for i in 1 .. m.relocs.len {
		if m.relocs[i - 1].addr >= m.relocs[i].addr {
			ascending = false
			break
		}
	}
	if ascending {
		order.reverse_in_place()
	} else {
		order.sort_with_compare(fn [m] (a &int, b &int) int {
			return m.relocs[*b].addr - m.relocs[*a].addr
		})
	}
	mut bytes := []u8{len: order.len * 8}
	for position, index in order {
		r := m.relocs[index]
		mut type_ := r.type_
		if pairs[index] {
			type_ = if r.type_ == arm64_reloc_page21 {
				arm64_reloc_got_load_page21
			} else {
				arm64_reloc_got_load_pageoff12
			}
		}
		mut info := u32(reloc_index[r.sym_idx]) & 0xffffff
		if r.pcrel {
			info |= 1 << 24
		}
		info |= u32(r.length) << 25
		if r.extern {
			info |= 1 << 27
		}
		info |= u32(type_) << 28
		write_u32_le_at_arr(mut bytes, position * 8, u32(r.addr))
		write_u32_le_at_arr(mut bytes, position * 8 + 4, info)
	}
	return bytes
}

fn write_nlist(mut symtab []u8, mut strtab []u8, name string, typ u8, sect u8, desc u16, value u64) {
	write_u32_le(mut symtab, u32(strtab.len))
	unsafe { strtab.push_many(name.str, name.len) }
	strtab << 0
	symtab << typ
	symtab << sect
	write_u16_le(mut symtab, desc)
	write_u64_le(mut symtab, value)
}

fn write_section_64(mut buf []u8, sectname string, segname string, addr int, size int, offset int, align int, reloff int, nreloc int, flags u32) {
	write_string_fixed(mut buf, sectname, 16)
	write_string_fixed(mut buf, segname, 16)
	write_u64_le(mut buf, u64(addr))
	write_u64_le(mut buf, u64(size))
	write_u32_le(mut buf, u32(offset))
	write_u32_le(mut buf, u32(align))
	write_u32_le(mut buf, u32(reloff))
	write_u32_le(mut buf, u32(nreloc))
	write_u32_le(mut buf, flags)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
}

// write_u32_le writes u32 le output for arm64.
fn write_u32_le(mut b []u8, v u32) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
}

// write_mh_magic_64 writes mh magic 64 output for arm64.
fn write_mh_magic_64(mut b []u8) {
	b << u8(0xcf)
	b << u8(0xfa)
	b << u8(0xed)
	b << u8(0xfe)
}

// write_u64_le writes u64 le output for arm64.
fn write_u64_le(mut b []u8, v u64) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
	b << u8(v >> 32)
	b << u8(v >> 40)
	b << u8(v >> 48)
	b << u8(v >> 56)
}

// write_u16_le writes u16 le output for arm64.
fn write_u16_le(mut b []u8, v u16) {
	b << u8(v)
	b << u8(v >> 8)
}

// write_string_fixed writes string fixed output for arm64.
fn write_string_fixed(mut b []u8, s string, len int) {
	mut bytes := s.bytes()
	for bytes.len < len {
		bytes << 0
	}
	for i in 0 .. len {
		b << bytes[i]
	}
}
