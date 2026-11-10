module wit

import os
import wasm

fn test_parse_basic() {
	src := '
package example:demo@0.1.0;

interface types {
	record point {
		x: f64,
		y: f64,
	}
	enum color { red, green, blue }
	variant shape { circle(f64), empty }
	flags perms { read, write }
	type alias = u32;

	add: func(a: s32, b: s32) -> s32;
	greet: func(name: string) -> string;
}

world app {
	import types;
	export run: func();
}
'
	doc := parse(src) or { panic(err) }
	assert doc.package.namespace == 'example'
	assert doc.package.name == 'demo'
	assert doc.interfaces.len == 1
	iface := doc.interfaces[0]
	assert iface.name == 'types'
	assert iface.items.len == 7
	assert doc.worlds.len == 1
	w := doc.worlds[0]
	assert w.name == 'app'
	assert w.imports.len == 1
	assert w.exports.len == 1
	// round-trip type str
	assert prim_type('s32').str() == 's32'

	generated := doc.to_v()
	assert generated.contains('pub struct Point')
	assert generated.contains('pub fn add')

	doc2 := parse(src) or { panic(err) }
	printed := doc2.str()
	assert printed.contains('interface types')
	assert printed.contains('record point')
}

fn test_wasi_fs_types_codegen_compiles() {
	src := os.read_file('vlib/wasm/wit/testdata/wasi/fs_types.wit') or { panic(err) }
	doc := parse(src) or { panic(err) }
	generated := doc.to_v()
	assert generated.len > 0
	os.write_file('vlib/wasm/wit/testdata/wasi/fs_types_generated.v', generated) or { panic(err) }
	res := os.exec(['v', '-check', 'vlib/wasm/wit/testdata/wasi/fs_types_generated.v'])
	assert res.exit_code == 0, res.output
}

fn test_validate_unknown_type() {
	src := 'package p:q;

interface i {
	f: func(x: unknown_type) -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains('error_unknown_type')
		return
	}
	panic('expected error')
}

fn test_validate_duplicate_name() {
	src := 'package p:q;

interface i {
	x: func() -> u32;
	x: func() -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains('error_duplicate_name')
		return
	}
	panic('expected error')
}

fn test_validate_valid() {
	src := 'package p:q;

interface i {
	f: func(x: u32) -> u32;
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or { panic(err) }
}

fn test_validate_duplicate_method() {
	src := 'package p:q;

interface i {
	resource r {
		m: func() -> u32;
		m: func() -> u32;
	}
}
'
	doc := parse(src) or { panic(err) }
	validate(doc) or {
		assert err.msg().contains('error_duplicate_name')
		return
	}
	panic('expected error')
}

fn test_parse_list_option_result() {
	src := '
package p:q;

interface i {
	f: func(x: list<u8>, y: option<string>) -> result<s32, string>;
}
'
	doc := parse(src) or { panic(err) }
	assert doc.interfaces.len == 1
	f := doc.interfaces[0].items[0].func or { panic('no func') }
	assert f.params.len == 2
	assert f.results.len == 1
	assert f.results[0].str() == 'result<s32, string>'
}

fn test_wrap_component_lifts_string_export() {
	// core module with (i32, i32) -> i32 echo_len, cabi_realloc and memory
	mut m := wasm.Module{}
	mut echo := m.new_function('echo_len', [.i32_t, .i32_t], [.i32_t])
	{
		echo.local_get(1)
	}
	m.commit(echo, true)
	mut realloc := m.new_function('cabi_realloc', [.i32_t, .i32_t, .i32_t, .i32_t], [
		.i32_t,
	])
	{
		realloc.i32_const(0)
	}
	m.commit(realloc, true)
	m.assign_memory('memory', true, 1, none)
	core := m.compile()
	doc := parse('package example:echo@0.1.0;

world echo {
	export echo-len: func(s: string) -> u32;
}
') or { panic(err) }
	comp := wrap_component(core, doc, '')
	assert wasm.is_component(comp)
	// walk component sections
	mut pos := 8
	mut export_names := []string{}
	mut has_string_param := false
	mut lift_has_mem_opt := false
	mut lift_has_realloc_opt := false
	for pos < comp.len {
		sid := comp[pos]
		pos++
		mut size := 0
		mut shift := 0
		for {
			b := comp[pos]
			pos++
			size |= (int(b) & 0x7f) << shift
			shift += 7
			if b & 0x80 == 0 {
				break
			}
		}
		end := pos + size
		if sid == 7 {
			// type section: look for a functype with one 0x73 (string) param
			mut p := pos
			mut cnt := 0
			mut sh := 0
			for {
				b := comp[p]
				p++
				cnt |= (int(b) & 0x7f) << sh
				sh += 7
				if b & 0x80 == 0 {
					break
				}
			}
			for _ in 0 .. cnt {
				assert comp[p] == 0x40
				p++
				mut pc := 0
				sh = 0
				for {
					b := comp[p]
					p++
					pc |= (int(b) & 0x7f) << sh
					sh += 7
					if b & 0x80 == 0 {
						break
					}
				}
				mut ptypes := []u8{}
				for _ in 0 .. pc {
					mut ln := 0
					sh = 0
					for {
						b := comp[p]
						p++
						ln |= (int(b) & 0x7f) << sh
						sh += 7
						if b & 0x80 == 0 {
							break
						}
					}
					p += ln
					ptypes << comp[p]
					p++
				}
				if ptypes == [u8(0x73)] {
					has_string_param = true
				}
				// skip results
				if comp[p] == 0x01 {
					p += 2
				} else {
					p += 2
				}
			}
		}
		if sid == 8 {
			// canon section: any lift carrying both memory and realloc opts
			mut p := pos
			mut cnt := 0
			mut sh := 0
			for {
				b := comp[p]
				p++
				cnt |= (int(b) & 0x7f) << sh
				sh += 7
				if b & 0x80 == 0 {
					break
				}
			}
			for _ in 0 .. cnt {
				assert comp[p] == 0x00
				p++
				assert comp[p] == 0x00
				p++
				// func idx
				for {
					b := comp[p]
					p++
					if b & 0x80 == 0 {
						break
					}
				}
				// opts
				mut ocnt := 0
				sh = 0
				for {
					b := comp[p]
					p++
					ocnt |= (int(b) & 0x7f) << sh
					sh += 7
					if b & 0x80 == 0 {
						break
					}
				}
				mut has_mem := false
				mut has_re := false
				for _ in 0 .. ocnt {
					code := comp[p]
					p++
					if code == 0x03 {
						has_mem = true
						for {
							b := comp[p]
							p++
							if b & 0x80 == 0 {
								break
							}
						}
					} else if code == 0x04 {
						has_re = true
						for {
							b := comp[p]
							p++
							if b & 0x80 == 0 {
								break
							}
						}
					} else {
						for {
							b := comp[p]
							p++
							if b & 0x80 == 0 {
								break
							}
						}
					}
				}
				// type idx
				for {
					b := comp[p]
					p++
					if b & 0x80 == 0 {
						break
					}
				}
				if has_mem && has_re {
					lift_has_mem_opt = true
					lift_has_realloc_opt = true
				}
			}
		}
		if sid == 11 {
			mut p := pos
			mut cnt := 0
			mut sh := 0
			for {
				b := comp[p]
				p++
				cnt |= (int(b) & 0x7f) << sh
				sh += 7
				if b & 0x80 == 0 {
					break
				}
			}
			for _ in 0 .. cnt {
				assert comp[p] == 0x00
				p++
				mut ln := 0
				sh = 0
				for {
					b := comp[p]
					p++
					ln |= (int(b) & 0x7f) << sh
					sh += 7
					if b & 0x80 == 0 {
						break
					}
				}
				name := comp[p..p + ln].bytestr()
				p += ln
				export_names << name
				// sort + idx
				p++
				for {
					b := comp[p]
					p++
					if b & 0x80 == 0 {
						break
					}
				}
			}
		}
		pos = end
	}
	assert 'echo-len' in export_names, 'component must export echo-len, got ${export_names}'
	assert has_string_param, 'component type must declare a string param'
	assert lift_has_mem_opt, 'canon lift must carry (memory)'
	assert lift_has_realloc_opt, 'canon lift must carry (realloc)'
}
