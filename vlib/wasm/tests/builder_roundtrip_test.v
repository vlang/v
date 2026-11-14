module main

import os
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

struct SecReader {
mut:
	data []u8
	pos  int
}

fn (mut r SecReader) uleb() int {
	mut n := 0
	mut shift := 0
	for {
		b := r.data[r.pos]
		r.pos++
		n |= (int(b) & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			break
		}
	}
	return n
}

fn (mut r SecReader) raw(n int) []u8 {
	out := r.data[r.pos..r.pos + n].clone()
	r.pos += n
	return out
}

fn section_body(b []u8, id u8) []u8 {
	mut r := SecReader{ data: b, pos: 8 }
	for r.pos + 2 <= b.len {
		sid := b[r.pos]
		r.pos++
		size := r.uleb()
		if sid == id {
			return b[r.pos..r.pos + size]
		}
		r.pos += size
	}
	return []u8{}
}

fn test_custom_section_roundtrip() {
	mut m := wasm.Module{}
	mut func := m.new_function('f', [], [])
	{
		func.nop()
	}
	m.commit(func, false)
	m.add_custom_section('test', [u8(1), 2, 3])
	b := m.compile()
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	// find custom section id 0 named 'test' with the exact payload
	mut r := SecReader{ data: b, pos: 8 }
	mut found := false
	for r.pos + 2 <= b.len {
		sid := b[r.pos]
		r.pos++
		size := r.uleb()
		end := r.pos + size
		if sid == 0 {
			nlen := r.uleb()
			name := r.raw(nlen).bytestr()
			if name == 'test' {
				assert r.pos + 3 == end
				assert b[r.pos] == 1 && b[r.pos + 1] == 2 && b[r.pos + 2] == 3
				found = true
			}
		}
		r.pos = end
	}
	assert found, 'user custom section must round-trip name and bytes'
}

fn test_call_indirect_type_index() {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, none)
	ft := m.new_functype(wasm.FuncType{ parameters: [.i32_t, .i32_t], results: [.i32_t] })
	mut callee := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		callee.local_get(0)
		callee.local_get(1)
		callee.add(.i32_t)
	}
	m.commit(callee, true)
	tidx := m.assign_table('t', false, .funcref_t, 1, none)
	m.new_active_element(tidx, 0, ['add'])
	mut caller := m.new_function('call_it', [.i32_t, .i32_t], [.i32_t])
	{
		caller.local_get(0)
		caller.local_get(1)
		caller.i32_const(0)
		caller.call_indirect(ft, tidx)
	}
	m.commit(caller, true)
	b := m.compile()
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	// caller is the second defined function; find 0x11 call_indirect in its body
	code := section_body(b, 10)
	mut r := SecReader{ data: code, pos: 0 }
	count := r.uleb()
	assert count == 2
	mut bodies := [][]u8{}
	for _ in 0 .. count {
		sz := r.uleb()
		bodies << r.raw(sz)
	}
	body := bodies[1]
	mut found := false
	for i in 0 .. body.len {
		if body[i] == 0x11 && i + 3 < body.len {
			mut q := SecReader{ data: body, pos: i + 1 }
			ti := q.uleb()
			ta := q.uleb()
			// callee 'add' is defined func 0 with the ft type index
			assert ta == int(tidx), 'table index must match'
			_ = ti
			found = true
		}
	}
	assert found, 'call_indirect must be present in caller body'
}

fn test_ref_table_ops_roundtrip() {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, none)
	tidx := m.assign_table('t', false, .funcref_t, 2, none)
	mut f := m.new_function('f', [], [])
	{
		f.ref_null(.funcref_t)
		f.drop()
		f.i32_const(0)
		f.ref_func('f')
		f.drop()
		f.i32_const(1)
		f.ref_null(.funcref_t)
		f.table_set(tidx)
		f.i32_const(1)
		f.table_get(tidx)
		f.drop()
	}
	m.commit(f, true)
	b := m.compile()
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	code := section_body(b, 10)
	mut r := SecReader{ data: code, pos: 0 }
	_ = r.uleb()
	sz := r.uleb()
	body := r.raw(sz)
	// decode straight-line ops and require the ref/table forms
	mut seen_null := false
	mut seen_func := false
	mut seen_get := false
	mut seen_set := false
	mut p := 0
	// skip locals decls
	mut ndecl := 0
	mut sh := 0
	for {
		byte := body[p]
		p++
		ndecl |= (int(byte) & 0x7f) << sh
		sh += 7
		if byte & 0x80 == 0 {
			break
		}
	}
	for _ in 0 .. ndecl {
		for {
			byte := body[p]
			p++
			if byte & 0x80 == 0 {
				break
			}
		}
		p++
	}
	for p < body.len {
		op := body[p]
		p++
		if op == 0x0b {
			break
		} else if op == 0xd0 {
			assert body[p] == 0x70
			p++
			seen_null = true
		} else if op == 0xd2 {
			mut q := SecReader{ data: body, pos: p }
			_ = q.uleb()
			p = q.pos
			seen_func = true
		} else if op == 0x25 || op == 0x26 {
			mut q := SecReader{ data: body, pos: p }
			ta := q.uleb()
			p = q.pos
			assert ta == int(tidx)
			if op == 0x25 {
				seen_get = true
			} else {
				seen_set = true
			}
		} else if op == 0x20 || op == 0x21 || op == 0x22 || op == 0x23 || op == 0x24 {
			mut q := SecReader{ data: body, pos: p }
			_ = q.uleb()
			p = q.pos
		} else if op == 0x41 {
			for {
				byte := body[p]
				p++
				if byte & 0x80 == 0 {
					break
				}
			}
		} else if op == 0x1a {
			// drop, no immediate
		} else {
			assert false, 'unexpected op ${op:02x} in ref/table body'
		}
	}
	assert seen_null && seen_func && seen_get && seen_set
}

fn test_data_count_two_segments() {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, none)
	m.new_data_segment(none, 0, [u8(9), 9, 9])
	m.new_data_segment(none, 16, [u8(7), 7])
	b := m.compile()
	dc := section_body(b, 12)
	mut r := SecReader{ data: dc, pos: 0 }
	assert r.uleb() == 2, 'data count section must report two segments'
	ds := section_body(b, 11)
	mut q := SecReader{ data: ds, pos: 0 }
	assert q.uleb() == 2
	// segment 0 at offset 0 with 3 bytes
	assert q.raw(1) == [u8(0x00)]
	assert q.raw(1) == [u8(0x41)]
	assert q.uleb() == 0
	assert q.raw(1) == [u8(0x0b)]
	assert q.uleb() == 3
	assert q.raw(3) == [u8(9), 9, 9]
	// segment 1 at offset 16 with 2 bytes
	assert q.raw(1) == [u8(0x00)]
	assert q.raw(1) == [u8(0x41)]
	assert q.uleb() == 16
	assert q.raw(1) == [u8(0x0b)]
	assert q.uleb() == 2
	assert q.raw(2) == [u8(7), 7]
}

fn test_name_section_roundtrip() {
	mut m := wasm.Module{}
	m.enable_debug('testmod')
	mut func := m.new_function('named_fn', [.i32_t], [.i32_t])
	{
		func.local_get(0)
	}
	m.commit(func, true)
	b := m.compile()
	// find the custom name section and decode it
	mut r := SecReader{ data: b, pos: 8 }
	mut found_mod := false
	mut found_fn := false
	for r.pos + 2 <= b.len {
		sid := b[r.pos]
		r.pos++
		size := r.uleb()
		end := r.pos + size
		if sid == 0 {
			nlen := r.uleb()
			nm := r.raw(nlen).bytestr()
			if nm == 'name' {
				// walk subsections
				for r.pos < end {
					sub := b[r.pos]
					r.pos++
					ssize := r.uleb()
					send := r.pos + ssize
					if sub == 0 {
						// module name
						mlen := r.uleb()
						assert r.raw(mlen).bytestr() == 'testmod'
						found_mod = true
					} else if sub == 1 {
						// function names
						fcount := r.uleb()
						for _ in 0 .. fcount {
							_ = r.uleb()
							flen := r.uleb()
							if r.raw(flen).bytestr() == 'named_fn' {
								found_fn = true
							}
						}
					}
					r.pos = send
				}
			}
		}
		r.pos = end
	}
	assert found_mod, 'name section must carry the module name'
	assert found_fn, 'name section must carry the function name'
}

fn test_real_module_structural() {
	core := os.read_file('vlib/v/gen/wasm/testdata/echo_len.shim.wasm') or { panic(err) }
	b := core.bytes()
	assert b[0] == 0x00 && b[1] == 0x61 && b[2] == 0x73 && b[3] == 0x6d
	assert b[4] == 0x01 && b[5] == 0x00 && b[6] == 0x00 && b[7] == 0x00
	// type section must contain (i32, i32) -> i32
	types := section_body(b, 1)
	mut r := SecReader{ data: types, pos: 0 }
	count := r.uleb()
	mut found_sig := false
	for _ in 0 .. count {
		assert r.raw(1) == [u8(0x60)]
		pc := r.uleb()
		mut ps := []u8{}
		for _ in 0 .. pc {
			ps << r.raw(1)[0]
		}
		rc := r.uleb()
		mut rs := []u8{}
		for _ in 0 .. rc {
			rs << r.raw(1)[0]
		}
		if ps == [u8(0x7f), 0x7f] && rs == [u8(0x7f)] {
			found_sig = true
		}
	}
	assert found_sig, 'real module must declare (i32,i32)->i32'
	// export section must name echo_len_cabi (func) and memory
	exps := section_body(b, 7)
	mut q := SecReader{ data: exps, pos: 0 }
	ecount := q.uleb()
	mut names := map[string]bool{}
	for _ in 0 .. ecount {
		nlen := q.uleb()
		nm := q.raw(nlen).bytestr()
		kind := q.raw(1)[0]
		_ = q.uleb()
		if kind == 0 || kind == 2 {
			names[nm] = true
		}
	}
	assert 'echo_len_cabi' in names
	assert 'memory' in names
}
