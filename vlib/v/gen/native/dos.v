// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

const dos_header_size = 0x40
const dos_header_lfanew_offset = 0x3c

pub fn (mut g Gen) gen_dos_header() {
	dos_header := [
		i32(PeMagic.mz),
		// signature
		0x80,
		// bytes on last page of file
		1,
		// pages in file
		0,
		// relocations
		4,
		// header size in paragraph
		0x10,
		// minimum extra paragraphs
		0xffff,
		// maximum extra paragraphs
		0,
		// initial relative ss
		0x140,
		// initial SP
		0,
		// checksum
		0,
		// IP
		0,
		// IP relative CS
		0x40,
		// address of relocation table
		0,
		// overlay number
		0,
		0,
		0,
		0,
		// reserved
		0,
		// oemid
		0,
		// oeminfo
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0x80,
		0,
		// address of PE header
	]

	// debug descriptions when `-v` is used
	dos_header_description := if g.pref.is_verbose {
		[
			'e_magic',
			'e_cblp',
			'e_cp',
			'e_crlc',
			'e_cparhdr',
			'e_minalloc',
			'e_maxalloc',
			'e_ss',
			'e_sp',
			'e_csum',
			'e_ip',
			'e_cs',
			'e_lfarlc',
			'e_ovno',
			'<reserved>',
			'e_oemid',
			'e_oeminfo',
			'<reserved>',
		]
	} else {
		[]string{}
	}

	for i, b in dos_header {
		g.write16(b)
		if g.pref.is_verbose && i < dos_header_description.len {
			g.println('; ' + dos_header_description[i])
		}
	}
	if g.pos() != dos_header_size {
		g.n_error('Invalid dos header size')
	}

	g.println('')
	g.println('^^^ DOS Header (64)')
}

// size is 0xb8
pub fn (mut g Gen) gen_dos_stub() {
	// 14 code bytes + "This program cannot be run in DOS mode.\r\r\n$" + 6 * 0x00
	g.write([
		u8(0x0e),
		0x1f,
		0xba,
		0x0e,
		0x00,
		0xb4,
		0x09,
		0xcd,
		0x21,
		0xb8,
		0x01,
		0x4c,
		0xcd,
		0x21,
		0x54,
		0x68,
		0x69,
		0x73,
		0x20,
		0x70,
		0x72,
		0x6f,
		0x67,
		0x72,
		0x61,
		0x6d,
		0x20,
		0x63,
		0x61,
		0x6e,
		0x6e,
		0x6f,
		0x74,
		0x20,
		0x62,
		0x65,
		0x20,
		0x72,
		0x75,
		0x6e,
		0x20,
		0x69,
		0x6e,
		0x20,
		0x44,
		0x4f,
		0x53,
		0x20,
		0x6d,
		0x6f,
		0x64,
		0x65,
		0x2e,
		0x0d,
		0x0d,
		0x0a,
		0x24,
	])

	g.pad_to(dos_stub_end)

	g.println('')
	g.println('^^^ DOS Stub')
}
