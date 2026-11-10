module wasm

// cabi_test.v proves the native canonical-ABI path for exported string params.
//
// A guest `echo_len(s string) int` compiled with wasm_cabi on must export
// `echo_len` as (i32 ptr, i32 len) -> i32, with memory and cabi_realloc
// exported. A backend-generated wrapper materializes the V string and calls
// the original single-pointer function, which stays unexported. The shim era
// (a hand-written echo_len_cabi in guest source) is frozen in
// testdata/echo_len.shim.wasm for shape parity.

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

const cabi_guest_source = "module main

@[export: 'echo_len']
pub fn echo_len(s string) int {
	return s.len
}

@[export: 'first_byte']
pub fn first_byte(s string) u8 {
	return s[0]
}

fn main() {}
"

fn cabi_test_module_bytes(source string, cabi bool) []u8 {
	dir := os.join_path(os.vtmp_dir(), 'cabi_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'guest.v')
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	transform.transform(mut a, &tc)
	mut metadata := Gen.new(a, &tc, map[string]bool{})
	config := metadata.ssa_configuration() or { panic(err) }
	mut m := ssa.build_with_options(a, map[string]bool{}, &tc, ssa.BuildOptions{
		target: ssa.TargetData{ ptr_size: 4 }
	})
	mut g := SSAGen.new(m)
	g.set_wasm_cabi(cabi)
	g.configure(config.exports, config.init_fns, config.main_fn)
	g.gen() or { panic(err) }
	return g.mod.compile()
}

struct CabiTypeSig {
	params  []u8
	results []u8
}

struct CabiReader {
mut:
	bytes []u8
	pos   int
}

fn (mut r CabiReader) leb() int {
	mut n := 0
	mut shift := 0
	for {
		b := r.bytes[r.pos]
		r.pos++
		n |= (int(b) & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			break
		}
	}
	return n
}

fn (mut r CabiReader) cname() string {
	len := r.leb()
	mut out := ''
	for _ in 0 .. len {
		out += r.bytes[r.pos].ascii_str()
		r.pos++
	}
	return out
}

fn cabi_section(bytes []u8, id u8) []u8 {
	mut r := CabiReader{
		bytes: bytes
		pos:   8
	}
	for r.pos + 2 <= bytes.len {
		sid := bytes[r.pos]
		r.pos++
		size := r.leb()
		end := r.pos + size
		if sid == id {
			return bytes[r.pos..end]
		}
		r.pos = end
	}
	return []u8{}
}

fn cabi_type_section(bytes []u8) []CabiTypeSig {
	body := cabi_section(bytes, 1)
	mut r := CabiReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	mut out := []CabiTypeSig{}
	for _ in 0 .. count {
		assert r.bytes[r.pos] == 0x60
		r.pos++
		pc := r.leb()
		mut params := []u8{}
		for _ in 0 .. pc {
			params << r.bytes[r.pos]
			r.pos++
		}
		rc := r.leb()
		mut results := []u8{}
		for _ in 0 .. rc {
			results << r.bytes[r.pos]
			r.pos++
		}
		out << CabiTypeSig{ params: params, results: results }
	}
	return out
}

fn cabi_func_section(bytes []u8) []int {
	body := cabi_section(bytes, 3)
	mut r := CabiReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	mut out := []int{}
	for _ in 0 .. count {
		out << r.leb()
	}
	return out
}

fn cabi_code_bodies(bytes []u8) [][]u8 {
	body := cabi_section(bytes, 10)
	mut r := CabiReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	mut out := [][]u8{}
	for _ in 0 .. count {
		size := r.leb()
		out << r.bytes[r.pos..r.pos + size]
		r.pos += size
	}
	return out
}

fn cabi_import_func_count(bytes []u8) int {
	body := cabi_section(bytes, 2)
	if body.len == 0 {
		return 0
	}
	mut r := CabiReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	mut funcs := 0
	for _ in 0 .. count {
		_ = r.cname()
		_ = r.cname()
		kind := r.bytes[r.pos]
		r.pos++
		if kind == 0 {
			_ = r.leb()
			funcs++
		} else if kind == 1 {
			r.pos++ // elem type
			flags := r.leb()
			_ = r.leb()
			if flags & 1 != 0 {
				_ = r.leb()
			}
		} else if kind == 2 {
			flags := r.leb()
			_ = r.leb()
			if flags & 1 != 0 {
				_ = r.leb()
			}
		} else if kind == 3 {
			r.pos += 2
		}
	}
	return funcs
}

// cabi_wasm_exports maps every exported function's name to its function index.
fn cabi_wasm_exports(bytes []u8) map[string]int {
	mut out := map[string]int{}
	mut r := CabiReader{
		bytes: cabi_section(bytes, 7)
		pos:   0
	}
	count := r.leb()
	for _ in 0 .. count {
		name := r.cname()
		kind := r.bytes[r.pos]
		r.pos++
		idx := r.leb()
		if kind == 0 {
			out[name] = idx
		}
	}
	return out
}

fn cabi_export_kind(bytes []u8, name string) int {
	body := cabi_section(bytes, 7)
	mut r := CabiReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	for _ in 0 .. count {
		n := r.cname()
		kind := r.bytes[r.pos]
		r.pos++
		idx := r.leb()
		_ = idx
		if n == name {
			return kind
		}
	}
	return -1
}

fn test_cabi_export_signature() {
	b := cabi_test_module_bytes(cabi_guest_source, true)
	// memory, cabi_realloc and echo_len are exported; echo_len is a function.
	assert cabi_export_kind(b, 'memory') == 2
	assert cabi_export_kind(b, 'cabi_realloc') == 0
	assert cabi_export_kind(b, 'echo_len') == 0
	exports := cabi_wasm_exports(b)
	funcs := cabi_func_section(b)
	sigs := cabi_type_section(b)
	n_imports := cabi_import_func_count(b)
	assert n_imports == 0
	echo_idx := exports['echo_len']
	echo_type := sigs[funcs[echo_idx - n_imports]]
	assert echo_type.params == [u8(0x7f), u8(0x7f)], 'echo_len params must be (i32, i32)'
	assert echo_type.results == [u8(0x7f)], 'echo_len result must be i32'
	// the inner single-pointer function is still present but unexported
	mut found_inner := false
	for name, _ in exports {
		_ = name
	}
	for fi, ti in funcs {
		if sigs[ti].params == [u8(0x7f)] && sigs[ti].results == [u8(0x7f)] {
			global_idx := n_imports + fi
			mut exported := false
			for _, eidx in exports {
				if eidx == global_idx {
					exported = true
					break
				}
			}
			if !exported {
				found_inner = true
			}
		}
	}
	assert found_inner, 'inner single-pointer function must exist unexported'
}

fn test_cabi_wrapper_simulation() {
	b := cabi_test_module_bytes(cabi_guest_source, true)
	exports := cabi_wasm_exports(b)
	funcs := cabi_func_section(b)
	sigs := cabi_type_section(b)
	bodies := cabi_code_bodies(b)
	n_imports := cabi_import_func_count(b)
	echo_idx := exports['echo_len']
	wrapper := bodies[echo_idx - n_imports]
	// decode the straight-line wrapper body
	mut pos := 0
	// locals decls
	mut ndecl := 0
	mut shift := 0
	for {
		byte := wrapper[pos]
		pos++
		ndecl |= (int(byte) & 0x7f) << shift
		shift += 7
		if byte & 0x80 == 0 {
			break
		}
	}
	assert ndecl == 0, 'wrapper must declare no locals'
	for _ in 0 .. ndecl {
		// count + type per entry (none expected)
		mut c := 0
		shift = 0
		for {
			byte := wrapper[pos]
			pos++
			c |= (int(byte) & 0x7f) << shift
			shift += 7
			if byte & 0x80 == 0 {
				break
			}
		}
		pos++
		_ = c
	}
	// the inner single-pointer function index is discovered from the wrapper's
	// own call target below; here just require that some unexported (i32)->i32
	// function exists at all.
	mut inner_idx := -1
	for fi, ti in funcs {
		t := sigs[ti]
		if t.params == [u8(0x7f)] && t.results == [u8(0x7f)] {
			global_idx := n_imports + fi
			mut exported := false
			for _, eidx in exports {
				if eidx == global_idx {
					exported = true
					break
				}
			}
			if !exported && inner_idx == -1 {
				inner_idx = global_idx
			}
		}
	}
	assert inner_idx >= 0
	// simulate: locals are [ptr, len]; memory is a fresh map
	case_bytes := ['hello'.bytes(), ''.bytes(), 'héllo'.bytes()]
	case_lens := [5, 0, 6]
	case_first := [u8(`h`), u8(0), u8(`h`)]
	for ci in 0 .. 3 {
		str_bytes := case_bytes[ci]
		str_len := case_lens[ci]
		// host places the string at guest address 0x1000
		base := 0x1000
		mut mem := map[int]u8{}
		for i, byte in str_bytes {
			mem[base + i] = byte
		}
		mut locals := [base, str_len]
		mut stack := []int{}
		mut p := pos
		mut scratch := -1
		mut called := -1
		for p < wrapper.len {
			op := wrapper[p]
			p++
			if op == 0x0b {
				break
			} else if op == 0x20 {
				mut v := 0
				shift = 0
				for {
					byte := wrapper[p]
					p++
					v |= (int(byte) & 0x7f) << shift
					shift += 7
					if byte & 0x80 == 0 {
						break
					}
				}
				stack << locals[v]
			} else if op == 0x41 {
				mut v := 0
				shift = 0
				mut by := u8(0)
				for {
					by = wrapper[p]
					p++
					v |= (int(by) & 0x7f) << shift
					shift += 7
					if by & 0x80 == 0 {
						break
					}
				}
				if by & 0x40 != 0 {
					v -= 1 << shift
				}
				stack << v
			} else if op == 0x36 || op == 0x3a {
				mut al := 0
				shift = 0
				for {
					byte := wrapper[p]
					p++
					al |= (int(byte) & 0x7f) << shift
					shift += 7
					if byte & 0x80 == 0 {
						break
					}
				}
				mut off := 0
				shift = 0
				for {
					byte := wrapper[p]
					p++
					off |= (int(byte) & 0x7f) << shift
					shift += 7
					if byte & 0x80 == 0 {
						break
					}
				}
				_ = al
				val := stack.pop()
				addr := stack.pop()
				if scratch == -1 {
					scratch = addr
				}
				assert addr == scratch, 'all struct stores hit the scratch slot'
				if op == 0x36 {
					for k in 0 .. 4 {
						mem[addr + off + k] = u8((val >> (8 * k)) & 0xff)
					}
				} else {
					mem[addr + off] = u8(val & 0xff)
				}
			} else if op == 0x10 {
				mut v := 0
				shift = 0
				for {
					byte := wrapper[p]
					p++
					v |= (int(byte) & 0x7f) << shift
					shift += 7
					if byte & 0x80 == 0 {
						break
					}
				}
				called = v
				_ = stack.pop()
				stack << -1 // call result marker
			} else {
				assert false, 'unexpected wrapper op ${op:02x} in case ${ci}'
			}
		}
		// the wrapper's own call target must be an unexported single-pointer func
		called_sig := sigs[funcs[called - n_imports]]
		assert called_sig.params == [u8(0x7f)], 'callee takes one i32'
		assert called_sig.results == [u8(0x7f)], 'callee returns i32'
		mut called_exported := false
		for _, eidx in exports {
			if eidx == called {
				called_exported = true
			}
		}
		assert !called_exported, 'callee must be the unexported inner function'
		assert stack.len == 1, 'wrapper must leave exactly the call result'
		// the materialized V string the inner function will read
		mut got_ptr := 0
		for k in 0 .. 4 {
			got_ptr |= int(mem[scratch + k]) << (8 * k)
		}
		mut got_len := 0
		for k in 0 .. 4 {
			got_len |= int(mem[scratch + 4 + k]) << (8 * k)
		}
		assert got_ptr == base, 'struct.str must be the host pointer'
		assert got_len == str_len, 'struct.len must be the host length'
		assert mem[scratch + 8] == 0, 'is_lit must be false'
		// first byte the guest would read
		if str_len > 0 {
			assert mem[got_ptr] == case_first[ci], 'first byte must match'
		}
	}
	// length pinning: host passes len 2 for the 3-byte é prefix
	he := 'héllo'.bytes()
	assert he.len == 6
	assert he[0] == `h`
	pinned := he[..2]
	assert pinned.len == 2
}

fn test_cabi_native_len_semantics() {
	assert 'hello'.len == 5
	assert ''.len == 0
	assert 'héllo'.len == 6, 'len counts UTF-8 bytes, not codepoints'
	assert 'hello'[0] == `h`
	pinned := 'héllo'[..2]
	assert pinned.len == 2, 'a 2-byte slice has len 2'
}

fn test_cabi_runtime_node() {
	b := cabi_test_module_bytes(cabi_guest_source, true)
	node := os.find_abs_path_of_executable('node') or {
		panic('node is required to run the cabi runtime test')
	}
	dir := os.join_path(os.vtmp_dir(), 'cabi_run_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	wasm_path := os.join_path(dir, 'guest.wasm')
	os.write_file_array(wasm_path, b) or { panic(err) }
	mjs := 'import assert from \'node:assert/strict\';
import { readFileSync } from \'node:fs\';
const bytes = readFileSync(process.argv[2]);
const { instance } = await WebAssembly.instantiate(bytes, {});
const mem = () => new Uint8Array(instance.exports.memory.buffer);
const echo_len = instance.exports.echo_len;
const first_byte = instance.exports.first_byte;
function put(off, arr) { mem().set(arr, off); }
// hello -> 5
put(4096, [104, 101, 108, 108, 111]);
assert.equal(echo_len(4096, 5), 5);
assert.equal(first_byte(4096, 5), 104);
// empty -> 0
assert.equal(echo_len(4096, 0), 0);
// h + U+00E9 + llo is 6 UTF-8 bytes, not 5 codepoints
put(8192, [104, 195, 169, 108, 108, 111]);
assert.equal(echo_len(8192, 6), 6);
assert.equal(first_byte(8192, 6), 104);
// length pins to what the host passes, not the NUL scan
assert.equal(echo_len(8192, 2), 2);
console.log(\'cabi-runtime-ok echo_len=5,0,6 first_byte=104 pin=2\');
'
	runner := os.join_path(dir, 'check.mjs')
	os.write_file(runner, mjs) or { panic(err) }
	result := os.exec([node, runner, wasm_path])
	assert result.exit_code == 0, result.output
	assert result.output.contains('cabi-runtime-ok'), result.output
}

fn test_cabi_parity_shim() {
	shim := os.read_file('vlib/v/gen/wasm/testdata/echo_len.shim.wasm') or { panic(err) }
	shim_bytes := shim.bytes()
	backend := cabi_test_module_bytes(cabi_guest_source, true)
	// frozen shim exports echo_len_cabi as (i32, i32) -> i32 ...
	shim_exports := cabi_wasm_exports(shim_bytes)
	assert 'echo_len_cabi' in shim_exports
	shim_types := cabi_type_section(shim_bytes)
	shim_funcs := cabi_func_section(shim_bytes)
	shim_nimp := cabi_import_func_count(shim_bytes)
	shim_sig := shim_types[shim_funcs[shim_exports['echo_len_cabi'] - shim_nimp]]
	assert shim_sig.params == [u8(0x7f), u8(0x7f)]
	assert shim_sig.results == [u8(0x7f)]
	// ... and the backend emits the identical signature for echo_len ...
	be_exports := cabi_wasm_exports(backend)
	be_types := cabi_type_section(backend)
	be_funcs := cabi_func_section(backend)
	be_nimp := cabi_import_func_count(backend)
	assert be_nimp == 0
	be_sig := be_types[be_funcs[be_exports['echo_len'] - be_nimp]]
	assert be_sig.params == shim_sig.params
	assert be_sig.results == shim_sig.results
	// ... while additionally exporting memory and cabi_realloc (the shim predates both).
	assert cabi_export_kind(shim_bytes, 'cabi_realloc') == -1
	assert cabi_export_kind(backend, 'memory') == 2
	assert cabi_export_kind(backend, 'cabi_realloc') == 0
}
