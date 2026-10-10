module wasm

// e2e_test.v drives small V programs through the whole wasm pipeline the way the
// backend's own path does — parser, checker, transform, the shared SSA builder
// (ssa.build_with_options), then SSAGen.gen and Module.compile — and inspects the
// emitted module. The checks split by design. Byte-level tests read the module's
// own sections, which is the only way to see structure a runtime hides: the data
// offset a string lands at, the exact imported declarations, the raw constant
// encoding. Runtime tests then hand the same module to `node` and assert on what
// it actually computes or writes. Each test file here compiles as its own unit,
// so the section reader and pipeline mirror the one in ssa_gen_test.v rather than
// importing it.

import os
import v.parser
import v.pref
import v.ssa
import v.ssa.optimize
import v.transform
import v.types

fn e2e_module_bytes(name string, source string, production bool) []u8 {
	dir := os.join_path(os.vtmp_dir(), 'e2e_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, '${name}.v')
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
	if production {
		optimize.optimize(mut m)
	}
	mut g := SSAGen.new(m)
	g.configure(config.exports, config.init_fns, config.main_fn)
	g.gen() or { panic(err) }
	return g.mod.compile()
}

// ---- wasm binary inspector (mirrors WasmReader in ssa_gen_test.v) ----

struct WasmReader {
mut:
	bytes []u8
	pos   int
}

fn (mut r WasmReader) leb() int {
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

fn (mut r WasmReader) name() string {
	len := r.leb()
	mut out := ''
	for _ in 0 .. len {
		out += r.bytes[r.pos].ascii_str()
		r.pos++
	}
	return out
}

// wasm_body returns the raw bytes of the section with the given id.
fn wasm_body(bytes []u8, id u8) []u8 {
	mut r := WasmReader{
		bytes: bytes
		pos:   8
	}
	for r.pos + 2 <= bytes.len {
		sid := bytes[r.pos]
		r.pos++
		end := r.pos + r.leb()
		if sid == id {
			return bytes[r.pos..end]
		}
		r.pos = end
	}
	return []u8{}
}

// wasm_exports maps every exported function's name to its function index.
fn wasm_exports(bytes []u8) map[string]int {
	mut out := map[string]int{}
	mut r := WasmReader{
		bytes: wasm_body(bytes, 7)
		pos:   0
	}
	count := r.leb()
	for _ in 0 .. count {
		name := r.name()
		kind := r.bytes[r.pos]
		r.pos++
		idx := r.leb()
		if kind == 0 {
			out[name] = idx
		}
	}
	return out
}

// wasm_imports maps every `module.name` the import section declares to its type
// index, ignoring non-function imports (there are none here).
fn wasm_imports(bytes []u8) map[string]int {
	mut out := map[string]int{}
	body := wasm_body(bytes, 2)
	if body.len == 0 {
		return out
	}
	mut r := WasmReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	for _ in 0 .. count {
		module := r.name()
		name := r.name()
		kind := r.bytes[r.pos]
		r.pos++
		if kind != 0 {
			continue
		}
		out['${module}.${name}'] = r.leb()
	}
	return out
}

// wasm_type_at decodes the `(params) -> (results)` signature registered at
// `index`, so a test can check the shape of an export or import.
fn wasm_type_at(bytes []u8, index int) ([]u8, []u8) {
	mut params := []u8{}
	mut results := []u8{}
	mut r := WasmReader{
		bytes: bytes
		pos:   8
	}
	for r.pos + 2 <= bytes.len {
		id := bytes[r.pos]
		r.pos++
		end := r.pos + r.leb()
		if id == 1 {
			mut cnt := r.leb()
			for idx in 0 .. cnt {
				if r.bytes[r.pos] != 0x60 {
					break
				}
				r.pos++
				nparams := r.leb()
				mut ps := []u8{}
				for _ in 0 .. nparams {
					ps << r.bytes[r.pos]
					r.pos++
				}
				nresults := r.leb()
				mut rs := []u8{}
				for _ in 0 .. nresults {
					rs << r.bytes[r.pos]
					r.pos++
				}
				if idx == index {
					params = ps.clone()
					results = rs.clone()
				}
			}
		}
		r.pos = end
	}
	return params, results
}

// wasm_func_type_idx resolves a function index to its type index by walking the
// function section past the imported functions that occupy the low indices.
fn wasm_func_type_idx(bytes []u8, func_index int) int {
	defined := func_index - wasm_imports(bytes).len
	body := wasm_body(bytes, 3)
	mut r := WasmReader{
		bytes: body
		pos:   0
	}
	cnt := r.leb()
	mut ti := -1
	for i in 0 .. cnt {
		v := r.leb()
		if i == defined {
			ti = v
		}
	}
	return ti
}

// wasm_func_bodies returns each defined function's code body in order, so a test
// can look at one function's constants in isolation.
fn wasm_func_bodies(bytes []u8) [][]u8 {
	body := wasm_body(bytes, 10)
	mut r := WasmReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	mut out := [][]u8{}
	for _ in 0 .. count {
		size := r.leb()
		start := r.pos
		out << body[start..start + size]
		r.pos = start + size
	}
	return out
}

// wasm_first_data_seg returns the offset and payload of the first active data
// segment, which is where SSAGen places the interned string data.
fn wasm_first_data_seg(bytes []u8) (int, []u8) {
	body := wasm_body(bytes, 11)
	mut r := WasmReader{
		bytes: body
		pos:   0
	}
	r.leb() // segment count
	r.pos++ // flags: active, memory 0
	r.pos++ // offset opcode: 0x41 i32.const
	mut off := i64(0)
	mut shift := 0
	for {
		b := r.bytes[r.pos]
		r.pos++
		off |= i64(b & 0x7f) << shift
		shift += 7
		if b & 0x80 == 0 {
			if b & 0x40 != 0 {
				off |= i64(-1) << shift
			}
			break
		}
	}
	r.pos++ // offset expr end: 0x0b
	len := r.leb()
	start := r.pos
	return int(off), body[start..start + len]
}

// ---- byte search helpers ----

fn find_bytes(haystack []u8, needle []u8) int {
	if needle.len == 0 {
		return 0
	}
	for i in 0 .. haystack.len {
		if i + needle.len > haystack.len {
			break
		}
		mut ok := true
		for j in 0 .. needle.len {
			if haystack[i + j] != needle[j] {
				ok = false
				break
			}
		}
		if ok {
			return i
		}
	}
	return -1
}

fn le_present(haystack []u8, val int) bool {
	p := [u8(val & 0xff), u8((val >> 8) & 0xff), u8((val >> 16) & 0xff), u8((val >> 24) & 0xff)]
	return find_bytes(haystack, p) >= 0
}

fn hex(bytes []u8) string {
	mut s := ''
	for b in bytes {
		s += '${b:02x}'
	}
	return s
}

// sleb encodes a value as signed LEB128, and sleb64_const prefixes the `i64.const`
// opcode (0x42). This is how the backend emits integer literals for `int`
// arithmetic: an i64 constant immediately followed by `i32.wrap_i64` (0xa7).
fn sleb(v i64) []u8 {
	mut val := v
	mut out := []u8{}
	for {
		mut b := u8(val & 0x7f)
		val >>= 7
		done := (val == 0 && (b & 0x40) == 0) || (val == -1 && (b & 0x40) != 0)
		if !done {
			b |= 0x80
		}
		out << b
		if done {
			break
		}
	}
	return out
}

fn sleb64_const(v i64) []u8 {
	mut out := [u8(0x42)]
	out << sleb(v)
	return out
}

// ---- node runners ----

fn e2e_node(name string, wasm_bytes []u8, mjs string) string {
	node := os.find_abs_path_of_executable('node') or {
		panic('e2e_node: node is required to run a wasm module, and it is not on PATH')
	}
	dir := os.join_path(os.vtmp_dir(), 'e2e_noderun_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	wasm_path := os.join_path(dir, '${name}.wasm')
	os.write_file_array(wasm_path, wasm_bytes) or { panic(err) }
	runner := os.join_path(dir, 'check.mjs')
	os.write_file(runner, mjs) or { panic(err) }
	result := os.exec([node, runner, wasm_path])
	assert result.exit_code == 0, result.output
	return result.output
}

// wasm_export_exists reports whether the export section declares `name` of the
// given kind (0 function, 2 memory).
fn wasm_export_exists(bytes []u8, name string, kind u8) bool {
	mut r := WasmReader{
		bytes: wasm_body(bytes, 7)
		pos:   0
	}
	count := r.leb()
	for _ in 0 .. count {
		nm := r.name()
		k := r.bytes[r.pos]
		r.pos++
		r.leb() // index
		if nm == name && k == kind {
			return true
		}
	}
	return false
}

// ---- tests ----

// B1/e2e: a string literal is materialised as a data segment, NUL-terminated,
// and the string struct's stored pointer names its exact offset.
fn test_e2e_string_constant_data_segment() {
	source := '
fn main() {
	println("hello, wasm")
}
'
	bytes := e2e_module_bytes('strdata', source, false)
	assert bytes[..8] == [u8(0), 97, 115, 109, 1, 0, 0, 0]
	off, payload := wasm_first_data_seg(bytes)
	base := 'hello, wasm'.bytes()
	rel := find_bytes(payload, base)
	assert rel >= 0, 'string text not in data segment'
	// NUL terminator immediately after the text.
	assert rel + base.len < payload.len && payload[rel + base.len] == 0, 'missing NUL terminator'
	// The pointer the module stores equals the literal's real memory address.
	text_addr := off + rel
	assert le_present(bytes, text_addr), 'no stored pointer to the string at offset ${text_addr}'
}

// e2e: the exports section names the compiled functions, and each signature is
// the one the source spells: ()->int, (int)->int and ()->i64.
fn test_e2e_exports_and_signatures() {
	source := '
fn make() int {
	return 30 * 10 + 4
}

fn add_const(a int) int {
	return a + 500000
}

fn wide() i64 {
	return i64(2000000000) + i64(2000000000)
}
'
	bytes := e2e_module_bytes('exports', source, false)
	exports := wasm_exports(bytes)
	make_idx := exports['make'] or {
		assert false, 'make not exported: ${exports}'
		-1
	}
	add_idx := exports['add_const'] or {
		assert false, 'add_const not exported: ${exports}'
		-1
	}
	wide_idx := exports['wide'] or {
		assert false, 'wide not exported: ${exports}'
		-1
	}
	// The module also exports its linear memory, the contract a host needs to
	// read the bytes a print writes.
	assert wasm_export_exists(bytes, 'memory', export_mem), 'memory export missing'
	mp, mr := wasm_type_at(bytes, wasm_func_type_idx(bytes, make_idx))
	assert mp.len == 0 && mr == [valtype_i32], 'make sig: ${mp.str()} -> ${mr.str()}'
	mp2, mr2 := wasm_type_at(bytes, wasm_func_type_idx(bytes, add_idx))
	assert mp2 == [valtype_i32] && mr2 == [valtype_i32], 'add_const sig wrong'
	mp3, mr3 := wasm_type_at(bytes, wasm_func_type_idx(bytes, wide_idx))
	assert mp3.len == 0 && mr3 == [valtype_i64], 'wide sig should be ()->i64'
}

// e2e: a program that prints and exits synthesises exactly the WASI fd_write and
// proc_exit imports, with their canonical signatures.
fn test_e2e_wasi_imports_when_printing() {
	source := '
fn main() {
	println("hello, wasm")
	exit(7)
}
'
	bytes := e2e_module_bytes('wasi', source, false)
	imports := wasm_imports(bytes)
	write_idx := imports['wasi_snapshot_preview1.fd_write'] or {
		assert false, 'fd_write missing: ${imports}'
		-1
	}
	exit_idx := imports['wasi_snapshot_preview1.proc_exit'] or {
		assert false, 'proc_exit missing: ${imports}'
		-1
	}
	wp, wr := wasm_type_at(bytes, write_idx)
	assert wp == [valtype_i32, valtype_i32, valtype_i32, valtype_i32], 'fd_write params wrong'
	assert wr == [valtype_i32], 'fd_write result wrong'
	ep, er := wasm_type_at(bytes, exit_idx)
	assert ep == [valtype_i32], 'proc_exit params wrong'
	assert er.len == 0, 'proc_exit must not return'
}

// e2e: `int` arithmetic materialises its literals as i64.const siblings wrapped
// to i32. This asserts the values 500000 and 300000 actually appear in add_const's
// body, not merely that the function compiled.
fn test_e2e_integer_constants_lower_as_i64_with_wrap() {
	source := '
fn add_const(a int) int {
	return a + 500000
}
'
	bytes := e2e_module_bytes('consts', source, false)
	bodies := wasm_func_bodies(bytes)
	// The only non-helper, non-allocator function is the user's; find the one
	// whose body holds the wrapped literal rather than trusting an index.
	mut wrapped := false
	mut want := sleb64_const(500000)
	want << u8(0xa7) // i64.const 500000 ; i32.wrap_i64
	for body in bodies {
		if find_bytes(body, want) >= 0 {
			wrapped = true
			break
		}
	}
	assert wrapped, 'i64.const 500000 followed by i32.wrap_i64 not found in any body'
}

// B matrix: run the matrix (struct literal, i32/i64/8-bit widths, if/else, for
// loop) through node and assert each program's single proof value. Both the
// unoptimised and optimised modules are exercised, matching ssa_gen_test.v.
fn test_e2e_compute_matrix_runs() {
	source := '
struct Pair {
	a int
	b int
}

fn make() int {
	p := Pair{
		a: 30,
		b: 4
	}
	return p.a * 10 + p.b
}

fn widths() int {
	a := u8(255)
	b := i8(255)
	return a * 1000 + (b + 100)
}

fn add_const(a int) int {
	return a + 500000
}

fn wide() i64 {
	return i64(2000000000) + i64(2000000000)
}

fn classify(n int) int {
	if n % 2 == 0 {
		return 100 + n
	} else {
		return 200 + n
	}
}

fn sum_to(n int) int {
	mut total := 0
	for i := 0; i < n; i++ {
		total += i
	}
	return total
}
'
	checks := '
assert.equal(e.make(), 304);
assert.equal(e.widths(), 255099);
assert.equal(e.add_const(250000), 750000);
assert.equal(e.add_const(-600000), -100000);
assert.equal(e.wide(), 4000000000n);
assert.equal(e.classify(6), 106);
assert.equal(e.classify(7), 207);
assert.equal(e.sum_to(5), 10);
assert.equal(e.sum_to(100), 4950);
'
	for production in [false, true] {
		bytes := e2e_module_bytes('matrix', source, production)
		mjs := "import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
const bytes = readFileSync(process.argv[2]);
assert.ok(WebAssembly.validate(bytes), 'module failed WebAssembly.validate');
const { instance } = await WebAssembly.instantiate(bytes, {});
const e = instance.exports;
assert.ok(e.memory instanceof WebAssembly.Memory);
${checks}
console.log('MATRIX_OK');
"
		out := e2e_node('matrix_${production}', bytes, mjs)
		assert out.contains('MATRIX_OK'), out
	}
}

// C: drive a printing, exiting module through node with a host that supplies the
// WASI pair, capturing the exact bytes the module writes and the exit code.
fn test_e2e_stdio_round_trip() {
	source := '
fn main() {
	println("hello, wasm")
	exit(42)
}
'
	bytes := e2e_module_bytes('stdio', source, false)
	expected := 'hello, wasm\n'.bytes()
	mjs := "import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
const bytes = readFileSync(process.argv[2]);
assert.ok(WebAssembly.validate(bytes));
let instance;
const captured = [];
class ProcExit extends Error { constructor(c){ super('proc_exit'); this.code = c; } }
const wasi = { wasi_snapshot_preview1: {
  fd_write: (fd, iovs, iovs_len, nwritten) => {
    const mem = instance.exports.memory.buffer;
    const dv = new DataView(mem);
    let total = 0;
    for (let i = 0; i < iovs_len; i++) {
      const ptr = dv.getUint32(iovs + i * 8, true);
      const len = dv.getUint32(iovs + i * 8 + 4, true);
      captured.push(Buffer.from(new Uint8Array(mem, ptr, len)));
      total += len;
    }
    dv.setUint32(nwritten, total, true);
    return 0;
  },
  proc_exit: (code) => { throw new ProcExit(code); }
}};
({ instance } = await WebAssembly.instantiate(bytes, wasi));
let exitCode = null;
try { instance.exports._start(); }
catch (e) { if (e instanceof ProcExit) { exitCode = e.code; } else { throw e; } }
const out = Buffer.concat(captured);
console.log('CAPTURED=' + out.toString('hex'));
console.log('EXIT=' + exitCode);
console.log('STDIO_OK');
"
	out := e2e_node('stdio', bytes, mjs)
	assert out.contains('CAPTURED=${hex(expected)}'), 'captured bytes differ: ${out}'
	assert out.contains('EXIT=42'), 'exit code not propagated: ${out}'
	assert out.contains('STDIO_OK'), out
}
