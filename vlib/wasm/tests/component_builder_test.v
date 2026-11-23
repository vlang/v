module main

import wasm

fn test_memory_section_emitted() {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, none)
	mut func := m.new_function('f', [], [])
	{
	}
	m.commit(func, true)
	b := m.compile()
	// memory section id 5 should be present
	mut found := false
	mut i := 8
	for i < b.len {
		sid := b[i]
		i++
		// read uleb size
		mut size := u32(0)
		mut shift := u32(0)
		for {
			byte := u32(b[i])
			i++
			size |= (byte & 0x7f) << shift
			if byte & 0x80 == 0 { break }
			shift += 7
		}
		if sid == 5 { found = true }
		i += int(size)
	}
	assert found
}

fn test_start_section_emitted() {
	mut m := wasm.Module{}
	mut func := m.new_function('_start', [], [])
	{
	}
	m.commit(func, true)
	m.assign_start('_start')
	b := m.compile()
	mut found := false
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
		if sid == 8 { found = true }
		i += int(size)
	}
	assert found
}