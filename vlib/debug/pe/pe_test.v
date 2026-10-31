module pe

import os

fn test_too_small() {
	if _ := new_file([]u8{len: 10}) {
		assert false
	} else {
		assert err.msg().contains('too small')
	}
}

fn test_invalid_signature() {
	mut data := []u8{len: 128}
	// e_lfanew -> 64
	data[0x3C] = 64
	if _ := new_file(data) {
		assert false
	} else {
		assert err.msg().contains('invalid PE signature')
	}
}

fn test_parse_real_pe() {
	path := 'C:/Users/MR/v/v.exe'
	if !os.exists(path) {
		return
	}
	data := os.read_file(path) or { return }
	f := new_file(data.bytes()) or { panic(err) }
	assert f.machine == image_file_machine_amd64
	assert f.sections.len > 0
	// .text section should exist
	s := f.section_by_name('.text')
	assert s != none
}
