module wasm

fn test_core_imports_supplied_to_instance() {
	mut m := Module{}
	m.new_function_import('wasi_snapshot_preview1', 'fd_write', [.i32_t, .i32_t, .i32_t, .i32_t], [.i32_t])
	mut func := m.new_function('say', [], [])
	{
		func.nop()
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_core_module(core)
	assert is_component(out)
	// Component must import the host func instead of instantiating with zero args.
	inames := component_import_names(out)
	assert inames.any(it.contains('fd_write')), 'component imports must include fd_write, got ${inames}'
	args, main_idx := core_instantiate_args(out)
	assert args.len == 1, 'wrapped module with one import module needs one instantiate arg, got ${args}'
	assert args[0] == 'wasi_snapshot_preview1', 'arg must name the import module, got ${args}'
	_ = main_idx
	// Aliases must point at the main instance (index 1), not 0.
	assert alias_instance_idx(out) == 1, 'aliases must reference the instantiated module'
}

fn test_unsupported_imports_fall_back_without_instantiation() {
	mut m := Module{}
	m.new_global_import('env', 'g', .i32_t, false)
	mut func := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		func.local_get(0)
		func.local_get(1)
		func.add(.i32_t)
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_core_module(core)
	assert is_component(out)
	// Fallback is a plain module wrapper with no component exports.
	assert component_section(out, 11).len == 0, 'unsupported imports must not produce lifted exports'
}

struct CompReader {
mut:
	data []u8
	pos  int
}

fn (mut r CompReader) uleb() int {
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

fn component_section(data []u8, id u8) []u8 {
	mut r := CompReader{
		data: data
		pos:  8
	}
	for r.pos + 2 <= data.len {
		sid := data[r.pos]
		r.pos++
		size := r.uleb()
		if sid == id {
			return data[r.pos..r.pos + size]
		}
		r.pos += size
	}
	return []u8{}
}

fn component_import_names(data []u8) []string {
	body := component_section(data, 10)
	if body.len == 0 {
		return []string{}
	}
	mut r := CompReader{
		data: body
	}
	count := r.uleb()
	mut out := []string{}
	for _ in 0 .. count {
		r.pos++ // nameattributes kind
		nlen := r.uleb()
		name := r.data[r.pos..r.pos + nlen].bytestr()
		r.pos += nlen
		r.pos++ // sort func
		_ = r.uleb() // type idx
		out << name
	}
	return out
}

fn core_instantiate_args(data []u8) ([]string, int) {
	body := component_section(data, 2)
	if body.len == 0 {
		return []string{}, -1
	}
	mut r := CompReader{
		data: body
	}
	count := r.uleb()
	mut args := []string{}
	mut main_idx := -1
	for ii in 0 .. count {
		kind := body[r.pos]
		r.pos++
		if kind == 0x01 {
			// inline-export instance: skip
			ecount := r.uleb()
			for _ in 0 .. ecount {
				nlen := r.uleb()
				r.pos += nlen
				r.pos++ // sort
				_ = r.uleb()
			}
		} else {
			// instantiate
			_ = r.uleb() // module idx
			acount := r.uleb()
			for _ in 0 .. acount {
				alen := r.uleb()
				aname := r.data[r.pos..r.pos + alen].bytestr()
				r.pos += alen
				r.pos++ // 0x12 instance sort
				_ = r.uleb() // instance idx
				args << aname
			}
			main_idx = ii
		}
	}
	return args, main_idx
}

fn alias_instance_idx(data []u8) int {
	body := component_section(data, 6)
	if body.len == 0 {
		return -1
	}
	mut r := CompReader{
		data: body
	}
	_ = r.uleb()
	// first alias: core sort prefix, core func sort, core instance, instance idx, ...
	r.pos++ // 0x00 core sort prefix
	r.pos++ // 0x00 core func sort
	r.pos++ // 0x01 core instance
	return r.uleb()
}
