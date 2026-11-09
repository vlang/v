module main

import wasm

fn test_memory_and_start_sections() {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, none)
	mut func := m.new_function('_start', [], [])
	{
		func.i32_const(0)
		func.drop()
	}
	m.commit(func, true)
	m.assign_start('_start')
	m.new_data_segment(none, 0, [u8(65), 66, 67])
	b := m.compile()
	// Verify magic
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	// Parse sections
	mut i := 8
	mut has_memory := false
	mut has_start := false
	mut has_data := false
	mut has_export := false
	for i < b.len {
		sid := b[i]
		i++
		mut size := u32(0)
		mut shift := u32(0)
		for {
			byte := u32(b[i])
			i++
			size |= (byte & 0x7f) << shift
			if byte & 0x80 == 0 { break }
			shift += 7
		}
		end := i + int(size)
		if sid == 5 { has_memory = true }
		if sid == 7 { has_export = true }
		if sid == 8 { has_start = true }
		if sid == 11 { has_data = true }
		i = end
	}
	assert has_memory
	assert has_export
	assert has_start
	assert has_data
}

fn test_table_and_element() {
	mut m := wasm.Module{}
	tidx := m.assign_table('t', false, .funcref_t, 1, none)
	mut func := m.new_function('f', [], [])
	{
		func.ref_null(.funcref_t)
		func.drop()
	}
	m.commit(func, true)
	b := m.compile()
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	// table section id 4
	mut has_table := false
	mut i := 8
	for i < b.len {
		sid := b[i]
		i++
		mut size := u32(0)
		mut shift := u32(0)
		for {
			byte := u32(b[i])
			i++
			size |= (byte & 0x7f) << shift
			if byte & 0x80 == 0 { break }
			shift += 7
		}
		end := i + int(size)
		if sid == 4 { has_table = true }
		i = end
	}
	assert has_table
	_ = tidx
}

fn test_import_section() {
	mut m := wasm.Module{}
	m.new_function_import('env', 'log', [.i32_t], [])
	mut func := m.new_function('main', [], [])
	{
		func.i32_const(42)
		func.call_import('env', 'log')
	}
	m.commit(func, true)
	b := m.compile()
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	mut has_import := false
	mut i := 8
	for i < b.len {
		sid := b[i]
		i++
		mut size := u32(0)
		mut shift := u32(0)
		for {
			byte := u32(b[i])
			i++
			size |= (byte & 0x7f) << shift
			if byte & 0x80 == 0 { break }
			shift += 7
		}
		end := i + int(size)
		if sid == 2 { has_import = true }
		i = end
	}
	assert has_import
}