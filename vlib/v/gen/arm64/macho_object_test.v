module arm64

import os

fn object_u32(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[off + 3]) << 24)
}

fn object_u64(data []u8, off int) u64 {
	return u64(object_u32(data, off)) | (u64(object_u32(data, off + 4)) << 32)
}

struct ObjectSymbol {
	name  string
	typ   u8
	sect  u8
	desc  u16
	value u64
}

struct ObjectReloc {
	addr   u32
	symbol int
	typ    int
	pcrel  bool
	extern bool
}

struct ParsedObject {
	filetype   u32
	sections   map[string][]u64 // name -> [addr, size, offset, align]
	symbols    []ObjectSymbol
	relocs     []ObjectReloc
	ilocal     u32
	nlocal     u32
	iextdef    u32
	nextdef    u32
	iundef     u32
	nundef     u32
	platform   u32
	minos      u32
	text_bytes []u8
}

fn parse_object(data []u8) ParsedObject {
	mut p := ParsedObject{
		filetype: object_u32(data, 12)
	}
	ncmds := int(object_u32(data, 16))
	mut off := 32
	mut sections := map[string][]u64{}
	mut reloff := 0
	mut nreloc := 0
	for _ in 0 .. ncmds {
		cmd := object_u32(data, off)
		size := int(object_u32(data, off + 4))
		match cmd {
			0x19 {
				nsects := int(object_u32(data, off + 64))
				for s in 0 .. nsects {
					base := off + 72 + s * 80
					name := data[base..base + 16].bytestr().trim_right('\0')
					sections[name] = [object_u64(data, base + 32), object_u64(data, base + 40),
						u64(object_u32(data, base + 48)), u64(object_u32(data, base + 52))]
					if name == '__text' {
						reloff = int(object_u32(data, base + 56))
						nreloc = int(object_u32(data, base + 60))
						start := int(object_u32(data, base + 48))
						p = ParsedObject{
							...p
							text_bytes: data[start..start + int(object_u64(data, base + 40))]
						}
					}
				}
			}
			0x32 {
				p = ParsedObject{
					...p
					platform: object_u32(data, off + 8)
					minos:    object_u32(data, off + 12)
				}
			}
			0x2 {
				symoff := int(object_u32(data, off + 8))
				nsyms := int(object_u32(data, off + 12))
				stroff := int(object_u32(data, off + 16))
				mut symbols := []ObjectSymbol{}
				for i in 0 .. nsyms {
					base := symoff + i * 16
					name_off := stroff + int(object_u32(data, base))
					mut end := name_off
					for data[end] != 0 {
						end++
					}
					symbols << ObjectSymbol{
						name:  data[name_off..end].bytestr()
						typ:   data[base + 4]
						sect:  data[base + 5]
						desc:  u16(data[base + 6]) | (u16(data[base + 7]) << 8)
						value: object_u64(data, base + 8)
					}
				}
				p = ParsedObject{
					...p
					symbols: symbols
				}
			}
			0xb {
				p = ParsedObject{
					...p
					ilocal:  object_u32(data, off + 8)
					nlocal:  object_u32(data, off + 12)
					iextdef: object_u32(data, off + 16)
					nextdef: object_u32(data, off + 20)
					iundef:  object_u32(data, off + 24)
					nundef:  object_u32(data, off + 28)
				}
			}
			else {}
		}
		off += size
	}
	mut relocs := []ObjectReloc{}
	for i in 0 .. nreloc {
		addr := object_u32(data, reloff + i * 8)
		info := object_u32(data, reloff + i * 8 + 4)
		relocs << ObjectReloc{
			addr:   addr
			symbol: int(info & 0xffffff)
			pcrel:  (info >> 24) & 1 == 1
			extern: (info >> 27) & 1 == 1
			typ:    int(info >> 28)
		}
	}
	return ParsedObject{
		...p
		sections: sections
		relocs:   relocs
	}
}

fn sample_object() &MachOObject {
	mut m := MachOObject.new()
	// main: bl _helper (forward, undefined at first), adrp/add of a string,
	// bl _puts (libc), bl _malloc (a libc-named V wrapper below)
	helper_ref := m.add_undefined('_helper')
	m.add_reloc(0, helper_ref, arm64_reloc_branch26, true)
	puts_ref := m.add_undefined('_puts')
	m.add_reloc(4, puts_ref, arm64_reloc_branch26, true)
	malloc_ref := m.add_undefined('_malloc')
	m.add_reloc(8, malloc_ref, arm64_reloc_branch26, true)
	str := m.add_symbol('L_str_0', 0, false, 2)
	m.add_reloc(12, str, arm64_reloc_page21, true)
	m.add_reloc(16, str, arm64_reloc_pageoff12, false)
	m.add_symbol('_main', 0, true, 1)
	m.add_symbol('_helper', 20, true, 1)
	m.add_symbol('_malloc', 24, true, 1)
	m.text_data = []u8{len: 28}
	m.str_data = 'hi\0'.bytes()
	// Like Gen.gen_post_pass: string and data symbol values are section
	// addresses (text_len + offset, align8(text_len + strings) + offset).
	m.symbols[str].value += 28
	m.data_data = []u8{len: 16}
	m.add_symbol('_counter', u64(32 + 8), true, 3)
	return m
}

fn test_write_object_is_a_relocatable_object_for_the_system_linker() {
	mut m := sample_object()
	path := os.join_path(os.vtmp_dir(), 'arm64_object_${os.getpid()}_hosted.o')
	defer {
		os.rm(path) or {}
	}
	m.write_object(path, MachOObjectOptions{
		exported: {
			'_main':    true
			'_helper':  true
			'_malloc':  true
			'_counter': true
		}
	})!
	o := parse_object(os.read_bytes(path)!)
	assert o.filetype == 1 // MH_OBJECT
	assert o.platform == 1 // macOS
	assert o.minos == 0x000b0000 // 11.0
	assert o.sections['__text'][0] == 0
	assert o.sections['__text'][1] == 28
	assert o.sections['__const'][0] == 28
	assert o.sections['__data'][0] == 32 // align8(28 + 3)
	assert o.sections['__data'][3] == 4
	assert o.text_bytes.len == 28
	// LC_DYSYMTAB groups: locals, external definitions, undefined.
	assert o.ilocal == 0
	assert o.iextdef == o.nlocal
	assert o.iundef == o.nlocal + o.nextdef
	assert int(o.nlocal + o.nextdef + o.nundef) == o.symbols.len
	for i, sym in o.symbols {
		if i < int(o.nlocal) {
			assert sym.typ == 0x0e, sym.name
		} else if i < int(o.iundef) {
			assert sym.typ == 0x0f, sym.name
		} else {
			assert sym.typ == 0x01, sym.name
		}
	}
	names := o.symbols.map(it.name)
	// The V wrapper named like libc's malloc becomes private, and calls to
	// `_malloc` bind to libc, as with the built-in linker.
	assert 'ltmp_v_shadowed_malloc' in names
	malloc_index := names.index('_malloc')
	assert malloc_index >= int(o.iundef)
	helper_index := names.index('_helper')
	assert o.symbols[helper_index].typ == 0x0f
	assert o.symbols[helper_index].value == 20
	counter := o.symbols[names.index('_counter')]
	assert counter.sect == 5
	assert counter.value >= o.sections['__bss'][0]
	string_symbol := o.symbols[names.index('L_str_0')]
	assert string_symbol.sect == 2
	assert string_symbol.value == 28
	// Relocations: descending addresses, all extern, remapped symbol indices.
	assert o.relocs.len == 5
	for i in 1 .. o.relocs.len {
		assert o.relocs[i - 1].addr > o.relocs[i].addr
	}
	mut relocs := map[u32]ObjectReloc{}
	for r in o.relocs {
		assert r.extern
		relocs[r.addr] = r
	}
	assert names[relocs[0].symbol] == '_helper'
	assert relocs[0].typ == arm64_reloc_branch26 && relocs[0].pcrel
	assert names[relocs[4].symbol] == '_puts'
	assert names[relocs[8].symbol] == '_malloc'
	assert names[relocs[12].symbol] == 'L_str_0'
	assert relocs[12].typ == arm64_reloc_page21
	assert names[relocs[16].symbol] == 'L_str_0'
	assert relocs[16].typ == arm64_reloc_pageoff12 && !relocs[16].pcrel
}

fn test_write_object_keeps_freestanding_runtime_definitions() {
	mut m := sample_object()
	path := os.join_path(os.vtmp_dir(), 'arm64_object_${os.getpid()}_freestanding.o')
	defer {
		os.rm(path) or {}
	}
	m.write_object(path, MachOObjectOptions{ freestanding: true })!
	o := parse_object(os.read_bytes(path)!)
	names := o.symbols.map(it.name)
	assert 'ltmp_v_shadowed_malloc' !in names
	malloc := o.symbols[names.index('_malloc')]
	assert malloc.typ == 0x0f
	assert malloc.value == 24
}

fn test_encode_macos_version() {
	assert encode_macos_version('11.0') == 0x000b0000
	assert encode_macos_version('14.5') == 0x000e0500
	assert encode_macos_version('10.15.7') == 0x000a0f07
}

// Definitions stay local unless they are exported, so V's own names cannot
// clash with C code linked next to the object.
fn test_write_object_keeps_unexported_definitions_local() {
	mut m := MachOObject.new()
	m.text_data << [u8(0xc0), 0x03, 0x5f, 0xd6]
	m.add_symbol('_main', 0, true, 1)
	m.add_symbol('_print', 0, true, 1)
	path := os.join_path(os.vtmp_dir(), 'arm64_object_local_${os.getpid()}.o')
	defer {
		os.rm(path) or {}
	}
	m.write_object(path, MachOObjectOptions{
		exported: {
			'_main': true
		}
	})!
	o := parse_object(os.read_bytes(path)!)
	names := o.symbols.map(it.name)
	assert o.symbols[names.index('_main')].typ == 0x0f
	assert o.symbols[names.index('_print')].typ == 0x0e
}
