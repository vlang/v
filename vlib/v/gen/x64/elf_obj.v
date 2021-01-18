// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module x64

/*
This file is unused right now, since binaries without sections
are generated.

But it will be necessary once we have dynamic linking.
*/
enum SectionType {
	null = 0
	progbits = 1
	symtab = 2
	strtab = 3
	rela = 4
}

struct SectionConfig {
	name    string
	typ     SectionType
	flags   i64
	data    voidptr
	is_saa  bool
	datalen i64
	link    int
	info    int
	align   i64
	entsize i64
}

fn (mut g Gen) section_header(c SectionConfig) {
	g.write32(g.sect_header_name_pos)
	g.sect_header_name_pos += c.name.len + 1
	g.write32(int(c.typ))
	g.write64(c.flags)
	g.write64(0) // sh_addr
	g.write64(g.offset) // offset
	g.offset += c.datalen + 1
	g.write64(c.datalen)
	g.write32(c.link)
	g.write32(c.info)
	g.write64(c.align)
	g.write64(c.entsize)
}

fn genobj() {
	/*
	// SHN_UNDEF
	mut g := Gen{}
	nr_sections := 7
	g.section_header(SectionConfig{
		name: ''
		typ: .null
		flags:0
		data: 0
		is_saa: false
		link: 0
		info:0
		align:0
		entsize: 0
	})

		/*
	for sect in sections {
		g.section_header(SectionConfig{
			name:0
			typ: sect.typ
			flags: sect.flags
			data: sect.data
			is_saa: true
			datalen: sect.len
			link: 0
			info: 0
			align: sect.align
			entsize: sect.entsize
		})

	}
	*/

	g.section_header(SectionConfig{
		name: '.DATA'
		typ: .progbits
		flags: 0x2
		//data: sect.data
		is_saa: true
		datalen: 0xd
		link: 0
		info: 0
		align: 1
		entsize: 0
	})

	g.section_header(SectionConfig{
		name: '.TEXT'
		typ: .progbits
		flags: 0x2
		//data: sect.data
		is_saa: true
		datalen: 0xd
		link: 0
		info: 0
		align: 1
		entsize: 0
	})
	g.section_header(SectionConfig{
		name: '.shstrtab'
		typ: .strtab
		flags: 0x2
		//data: sect.data
		is_saa: true
		datalen: 0x22
		link: 0
		info: 0
		align: 1
		entsize: 0
	})
	g.section_header(SectionConfig{
		name: '.symtab'
		typ: .symtab
		flags: 0x2
		//data: sect.data
		is_saa: true
		datalen: 0xd
		link: 0
		info: 0
		align: 1
		entsize: 0
	})
	g.section_header(SectionConfig{
		name: '.strtab'
		typ: .symtab
		flags: 0x2
		//data: sect.data
		is_saa: true
		datalen: 0xd
		link: 0
		info: 0
		align: 1
		entsize: 0
	})
	g.section_header(SectionConfig{
		name: '.rela.TEXT'
		typ: .rela
		flags: 0x0
		//data: sect.data
		is_saa: true
		datalen: 0x18
		link: 4
		info: 2
		align: 8
		entsize: 0x18
	})
	*/
}
