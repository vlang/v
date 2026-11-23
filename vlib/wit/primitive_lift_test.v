module wit

import wasm

fn test_primitive_only_preserves_wit_names_and_types() {
	src := 'package review:scalar;\nworld guest {\n\texport add-one: func(x: u32) -> u32;\n}\n'
	doc := parse(src) or {
		assert false, err.msg()
		return
	}
	assert doc.worlds.len == 1
	w := doc.worlds[0]
	mut f := WitFunc{}
	mut found := false
	for e in w.exports {
		if func := e.func {
			f = func
			found = true
			break
		}
	}
	assert found, 'world must export add-one'
	if spec := wit_func_lift_with_doc(f, doc) {
		assert spec.comp_params == [u8(0x79)], 'u32 param must stay unsigned'
		assert spec.comp_results == [u8(0x79)], 'u32 result must stay unsigned'
		assert spec.comp_labels == ['x']
		assert spec.core_params == [u8(0x7f)]
		assert spec.core_results == [u8(0x7f)]
	} else {
		assert false, 'primitive-only func must lift'
	}
	// Component-level: WIT export name and unsigned types survive.
	mut m := wasm.Module{}
	mut func := m.new_function('add_one', [.i32_t], [.i32_t])
	{
		func.local_get(0)
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_component(core, doc, '')
	assert wasm.is_component(out)
	ename := component_export_name(out, 0)
	assert ename == 'add-one', 'component must keep WIT name, got ${ename}'
	pt, rt := component_first_type(out)
	assert pt == [u8(0x79)], 'component param must be u32, got ${pt}'
	assert rt == [u8(0x79)], 'component result must be u32, got ${rt}'
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

fn comp_section(data []u8, id u8) []u8 {
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

fn component_export_name(data []u8, want int) string {
	body := comp_section(data, 11)
	if body.len == 0 {
		return ''
	}
	mut r := CompReader{
		data: body
	}
	count := r.uleb()
	for i in 0 .. count {
		_ = body[r.pos]
		r.pos++ // nameattributes kind
		nlen := r.uleb()
		name := body[r.pos..r.pos + nlen].bytestr()
		r.pos += nlen
		_ = body[r.pos]
		r.pos++ // sort
		_ = r.uleb() // func idx
		r.pos++ // absent externtype
		if i == want {
			return name
		}
	}
	return ''
}

fn component_first_type(data []u8) ([]u8, []u8) {
	body := comp_section(data, 7)
	if body.len == 0 {
		return []u8{}, []u8{}
	}
	mut r := CompReader{
		data: body
	}
	count := r.uleb()
	if count == 0 {
		return []u8{}, []u8{}
	}
	assert body[r.pos] == 0x40
	r.pos++
	pc := r.uleb()
	mut params := []u8{}
	for _ in 0 .. pc {
		llen := r.uleb()
		r.pos += llen
		params << body[r.pos]
		r.pos++
	}
	mut results := []u8{}
	if body[r.pos] == 0x01 {
		r.pos++
		_ = r.uleb()
	} else {
		r.pos++ // 0x00
		results << body[r.pos]
		r.pos++
	}
	return params, results
}
