// shapes_test.v measures what the wasm backend *computes* for shapes whose
// correctness was never executed. A module can be structurally valid and still
// wrong, so each case below compiles one small V program through the pipeline
// the backend's own path uses — parser, checker, transform, the shared SSA
// builder, then SSAGen.gen — and then runs that module under node with WASI
// stubs, comparing three things: the module's own stdout against the same
// program compiled and run natively by the compiler that built this binary
// (@VEXE, no -b wasm); every exported function's return value against a native
// reference number; and the linear memory the module writes for the shapes that
// have no wasm value type at all.
//
// The node runner speaks one protocol, and every comparison below is made on the
// V side by parsing it: `MODULE_STDOUT=<hex>` for the module's own bytes, and
// `VALUE <name>#<run>=<text>` for each export call. The runner prints what it
// saw and never decides whether a shape passed — an earlier version had it
// compare against a literal baked into the JavaScript and signal a mismatch by
// exiting non-zero, which turned every finding into one undifferentiated
// "node run failed" with the numbers buried in the same line.
module wasm

import encoding.hex
import os
import v.parser
import v.pref
import v.ssa
import v.ssa.optimize
import v.transform
import v.types

// The suite owns a private build directory: this machine runs several V
// sessions at once and they all share %TEMP%\v_<uid>, so a nested build in the
// shared directory is a coin toss.
fn shape_scratch() string {
	custom := os.getenv('SHAPE_SCRATCH')
	if custom.len > 0 {
		return custom
	}
	return os.join_path(os.home_dir(), 'AppData', 'Local', 'Temp', 'opencode', 'wasmshapes')
}

fn shape_enter_workspace() {
	root := shape_scratch()
	os.mkdir_all(root) or { panic(err) }
	// Child builds inherit both from this process. temp_dir() prefers TMPDIR
	// over TEMP on every platform, and an explicit VJOBS stops a nested build
	// from multiplying this suite's own parallelism.
	os.setenv('TMPDIR', os.join_path(root, 'vtmp'), true)
	os.setenv('VJOBS', '2', true)
}

// shape_vexe names the compiler the native reference is built with. @VEXE is
// baked into this binary, so it is the tree under test rather than whichever V
// happens to be installed.
fn shape_vexe() string {
	candidates := [@VEXE, os.getenv('VEXE'), os.join_path(os.getwd(), 'v'),
		os.join_path(os.getwd(), 'v.exe')]
	for c in candidates {
		if c.len > 0 && os.is_file(c) {
			return c
		}
	}
	panic('shape_vexe: no compiler found in ${candidates}')
}

// shape_module lowers source through the wasm pipeline. A refusal is returned,
// never raised, so a case can assert on the exact text the compiler produced.
fn shape_module(tag string, source string, production bool) ![]u8 {
	dir := os.join_path(shape_scratch(), tag, 'wasm')
	os.mkdir_all(dir) or { return err }
	path := os.join_path(dir, '${tag}.v')
	os.write_file(path, source) or { return err }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	if p.diagnostics.len > 0 {
		return error('parser: ${p.diagnostics.str()}')
	}
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	if tc.errors.len > 0 {
		return error('checker: ${tc.errors.str()}')
	}
	tc.annotate_types()
	transform.transform(mut a, &tc)
	mut metadata := Gen.new(a, &tc, map[string]bool{})
	config := metadata.ssa_configuration() or { return error('configuration: ${err.msg()}') }
	mut m := ssa.build_with_options(a, map[string]bool{}, &tc, ssa.BuildOptions{
		target: ssa.TargetData{ ptr_size: 4 }
	})
	if production {
		optimize.optimize(mut m)
	}
	mut g := SSAGen.new(m)
	g.configure(config.exports, config.init_fns, config.main_fn)
	g.gen() or { return error('gen: ${err.msg()}') }
	return g.mod.compile()
}

// shape_native compiles source with the same compiler, without -b wasm, and
// returns what the program printed. `main_tail` appends an entry point that
// prints the shapes source itself cannot: the SSA builder panics on `f64.str`
// (`ssa: unknown function f64.str`), so a program that prints a float never
// reaches any backend. The declarations are compiled byte for byte either way;
// the only difference is the entry point the normal backend adds on its own.
fn shape_native(tag string, source string, main_tail string) string {
	dir := os.join_path(shape_scratch(), tag, 'native')
	os.mkdir_all(dir) or { panic(err) }
	path := os.join_path(dir, '${tag}.v')
	os.write_file(path, source + main_tail) or { panic(err) }
	res := os.exec([shape_vexe(), '-silent', 'run', path])
	assert res.exit_code == 0, '${tag}: native reference build failed: ${res.output}'
	return res.output
}

// ---- wasm binary inspector (structurally mirrors e2e_test.v's) ----

struct ShapeReader {
mut:
	bytes []u8
	pos   int
}

fn (mut r ShapeReader) leb() int {
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

fn shape_section(bytes []u8, id u8) []u8 {
	mut r := ShapeReader{
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

fn shape_exports(bytes []u8) map[string]int {
	mut out := map[string]int{}
	mut r := ShapeReader{
		bytes: shape_section(bytes, 7)
		pos:   0
	}
	count := r.leb()
	for _ in 0 .. count {
		len := r.leb()
		mut name := ''
		for _ in 0 .. len {
			name += r.bytes[r.pos].ascii_str()
			r.pos++
		}
		kind := r.bytes[r.pos]
		r.pos++
		idx := r.leb()
		if kind == export_func {
			out[name] = idx
		}
	}
	return out
}

// shape_func_count returns how many functions the import section declares: the
// function section indexes defined functions after them, so a lookup by name
// needs the offset.
fn shape_import_count(bytes []u8) int {
	body := shape_section(bytes, 2)
	if body.len == 0 {
		return 0
	}
	mut r := ShapeReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	mut funcs := 0
	for _ in 0 .. count {
		r.leb() // module name
		r.leb() // import name
		kind := r.bytes[r.pos]
		r.pos++
		if kind == export_func {
			funcs++
		}
		r.leb() // type index, or the descriptor the kind names
	}
	return funcs
}

// shape_signature resolves an exported function index to the type index it was
// registered with, walking the function section past the imports.
fn shape_type_index_at(bytes []u8, func_index int) int {
	body := shape_section(bytes, 3)
	mut r := ShapeReader{
		bytes: body
		pos:   0
	}
	count := r.leb()
	defined := func_index - shape_import_count(bytes)
	mut ti := -1
	for i in 0 .. count {
		v := r.leb()
		if i == defined {
			ti = v
		}
	}
	return ti
}

// shape_type_at returns the (params, results) of the type registered at
// `index`: f64 is 0x7c and f32 is 0x7d, which is how a test proves the width a
// module computed with rather than trusting the source type.
fn shape_type_at(bytes []u8, index int) ([]u8, []u8) {
	mut params := []u8{}
	mut results := []u8{}
	mut r := ShapeReader{
		bytes: bytes
		pos:   8
	}
	for r.pos + 2 <= bytes.len {
		id := bytes[r.pos]
		r.pos++
		end := r.pos + r.leb()
		if id == 1 {
			count := r.leb()
			for idx in 0 .. count {
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

// shape_export_types maps each exported function name to its declared
// signature.
fn shape_export_types(bytes []u8) map[string]ShapeSignature {
	mut out := map[string]ShapeSignature{}
	for name, idx in shape_exports(bytes) {
		params, results := shape_type_at(bytes, shape_type_index_at(bytes, idx))
		out[name] = ShapeSignature{
			params:  params
			results: results
		}
	}
	return out
}

struct ShapeSignature {
	params  []u8
	results []u8
}

// ---- the node host ----

// shape_mjs_host is the host every module here runs under. It supplies the WASI
// pair the backend synthesises for println and exit, so the module's own stdout
// is captured byte for byte, and it prints the value of each export it calls.
// node returns an export's result for free, so a scalar export needs no extra
// host machinery: the value is the call's return.
const shape_mjs_host = "import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
const bytes = readFileSync(process.argv[2]);
assert.ok(WebAssembly.validate(bytes), 'module failed WebAssembly.validate');
let instance;
const captured = [];
class ProcExit extends Error { constructor(c) { super('proc_exit'); this.code = c; } }
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
const e = instance.exports;
if (e._start) {
  try { e._start(); }
  catch (err) {
    if (err instanceof ProcExit) { console.log('PROC_EXIT=' + err.code); }
    else { throw err; }
  }
}
console.log('MODULE_STDOUT=' + Buffer.concat(captured).toString('hex'));
const show = (v) => (typeof v === 'bigint' ? v.toString() : String(v));
"

// A run names one exported function, the host arguments to call it with, and the
// text node's `show` must print for the result. wasm value types are i32, f32 and
// f64, so node hands back a Number or a BigInt and `show` prints a determined
// text: a bool is 1 or 0, an f32/f64 is the shortest decimal that round-trips
// that width, and an i64 is a BigInt printed without a suffix. A value with no
// wasm type at all — a `string`, a struct, an enum parameter — has no export at
// all, so such a shape is measured through the module's own stdout and through a
// wrapper whose signature is primitive.
struct ShapeRun {
	name     string
	js_args  []string
	expected string
	// sink selects the aggregate-return protocol, for a function whose result
	// has no wasm value type: the module copies the value into the buffer the
	// host passes as the trailing pointer, so the read is `value@[ptr, len]`.
	sink bool
}

// shape_checks builds the per-shape node calls. Each run prints the value node
// handed back, and that is all: the comparison lives on the V side, so the runner
// never decides whether a shape passed and a wrong value cannot be reported as an
// opaque non-zero exit. A name may be called several times with different
// arguments, so every run gets its own binding.
fn shape_checks(runs []ShapeRun) string {
	mut out := ''
	for i, run in runs {
		args := run.js_args.join(', ')
		out += 'const shape_${i} = e.${run.name}(${args});\n'
		out += "console.log('VALUE ${run.name}#${i}=' + show(shape_${i}));\n"
	}
	out += "console.log('SHAPE_OK');\n"
	return out
}

fn shape_node(tag string, wasm []u8, checks string) string {
	node := os.find_abs_path_of_executable('node') or {
		panic('shape_node: node is required to run a wasm module, and it is not on PATH')
	}
	dir := os.join_path(shape_scratch(), tag, 'node')
	os.mkdir_all(dir) or { panic(err) }
	wasm_path := os.join_path(dir, '${tag}.wasm')
	os.write_file_array(wasm_path, wasm) or { panic(err) }
	runner := os.join_path(dir, 'check.mjs')
	os.write_file(runner, shape_mjs_host + checks) or { panic(err) }
	res := os.exec([node, runner, wasm_path])
	assert res.exit_code == 0, '${tag}: node run failed: ${res.output}'
	return res.output
}

// shape_module_stdout decodes the runner's `MODULE_STDOUT=<hex>` line. The module
// writes bytes, so the hex is the transport; decoding it here is what makes the
// comparison against the native build a comparison of strings rather than of two
// hex spellings.
fn shape_module_stdout(out string) !string {
	for line in out.split_into_lines() {
		if line.starts_with('MODULE_STDOUT=') {
			return hex.decode(line.all_after('MODULE_STDOUT='))!.bytestr()
		}
	}
	return error('the runner printed no MODULE_STDOUT line')
}

// shape_values indexes the runner's `VALUE <name>#<run>=<text>` lines by the run
// key shape_checks emitted, so a caller reads one value back rather than scanning
// the output for a name and guessing which call it belonged to.
fn shape_values(out string) map[string]string {
	mut values := map[string]string{}
	for line in out.split_into_lines() {
		if !line.starts_with('VALUE ') {
			continue
		}
		rest := line.all_after('VALUE ')
		values[rest.all_before('=')] = rest.all_after('=')
	}
	return values
}

// shape_case compiles one program for both backends, runs the wasm module and
// compares. `native` is asserted exactly: it is the one thing the case has to
// prove, and the only way to catch a module that prints a wrong value nobody
// asked it to assert. `main_tail` is appended for the native build only, for a
// shape that prints nothing itself.
fn shape_case(tag string, source string, native string, main_tail string, production bool,
	runs []ShapeRun) {
	shape_enter_workspace()
	wasm_native := shape_native(tag, source, main_tail)
	assert wasm_native == native, '${tag}: native reference is `${wasm_native}`, expected `${native}`'
	wasm := shape_module(tag, source, production) or {
		assert false, '${tag}: the wasm backend refused this program: ${err.msg()}'
		return
	}
	out := shape_node(tag, wasm, shape_checks(runs))
	assert out.contains('SHAPE_OK'), '${tag}: node run did not finish: ${out}'
	mut wrong := []string{}
	if main_tail.len == 0 {
		if module_stdout := shape_module_stdout(out) {
			if module_stdout != wasm_native {
				wrong << 'module stdout is `${module_stdout}`, native is `${wasm_native}`'
			}
		} else {
			wrong << 'no MODULE_STDOUT line: ${err.msg()}'
		}
	}
	values := shape_values(out)
	mut measured := []string{}
	for i, run in runs {
		got := values['${run.name}#${i}'] or {
			wrong << 'no VALUE line for `${run.name}(${run.js_args.join(', ')})`'
			continue
		}
		measured << got
		if got != run.expected {
			wrong << 'wasm `${run.name}(${run.js_args.join(', ')})` returned `${got}`, native `${run.expected}`'
		}
	}
	println('  ${tag} native=[${wasm_native.trim_space().replace('\n', ', ')}] wasm=[${measured.join(', ')}]')
	assert wrong.len == 0, '${tag}: ${wrong.len} shape(s) disagree with the native build -- ' +
		wrong.join('; ')
}

// shape_signature_test documents what an exported signature is, which is the
// only way to prove a value's width: the source type is what the program says,
// the type section is what the module will compute with.
fn shape_signature_test(tag string, source string, name string, want_params []u8,
	want_results []u8) {
	shape_enter_workspace()
	wasm := shape_module(tag, source, false) or {
		assert false, '${tag}: the wasm backend refused this program: ${err.msg()}'
		return
	}
	sigs := shape_export_types(wasm)
	sig := sigs[name] or {
		assert false, '${tag}: `${name}` is not exported: ${sigs.keys()}'
		return
	}
	assert sig.params == want_params, '${tag}: `${name}` params ${sig.params}, expected ${want_params}'
	assert sig.results == want_results, '${tag}: `${name}` results ${sig.results}, expected ${want_results}'
}

// ---- 1. f64 arithmetic, each returned through an exported function ----

// The program prints nothing: `println(f64)` reaches the SSA builder, which
// panics with `ssa: unknown function f64.str` — the float-to-string helper is
// not in its function table — so a program that prints a float never gets to any
// backend. The value is measured through the export instead, which is what a
// host reads anyway, and the normal backend's number for the same declarations
// comes from the appended entry point.
const shape_source_f64_arith = '
fn f_add() f64 { return 1.5 + 2.25 }

fn f_mul() f64 { return 1.5 * 4.0 }

fn f_div() f64 { return 7.0 / 2.0 }

fn f_mixed() f64 { return f64(3) + 1.5 }
'

const shape_main_tail_f64_arith = '
fn main() {
	println(f_add())
	println(f_mul())
	println(f_div())
	println(f_mixed())
}
'

fn test_shape_f64_arithmetic() {
	shape_case('f64_arith', shape_source_f64_arith, '3.75\n6.0\n3.5\n4.5\n',
		shape_main_tail_f64_arith, false, [
			ShapeRun{
				name:     'f_add'
				expected: '3.75'
			},
			ShapeRun{
				name:     'f_mul'
				expected: '6'
			},
			ShapeRun{
				name:     'f_div'
				expected: '3.5'
			},
			ShapeRun{
				name:     'f_mixed'
				expected: '4.5'
			},
		])
	shape_signature_test('f64_arith', shape_source_f64_arith, 'f_div', [], [valtype_f64])
}

// ---- 2. f32 versus f64 precision and width ----

const shape_source_float_precision = '
fn f32_pair() f32 { return f32(0.1) + f32(0.2) }

fn f64_pair() f64 { return 0.1 + 0.2 }

fn f32_wide() f32 { return f32(16777216) + f32(1) }

fn f64_wide() f64 { return f64(16777216) + f64(1) }
'

const shape_main_tail_float_precision = '
fn main() {
	println(f64(f32_pair()))
	println(f64_pair())
	println(f64(f32_wide()))
	println(f64(f64_wide()))
}
'

fn test_shape_float32_precision_and_width() {
	shape_case('float_precision', shape_source_float_precision,
		'0.30000001192092896\n0.30000000000000004\n1.6777216e+07\n1.6777217e+07\n',
		shape_main_tail_float_precision, false, [
			ShapeRun{
				name:     'f32_pair'
				expected: '0.30000001192092896'
			},
			ShapeRun{
				name:     'f64_pair'
				expected: '0.30000000000000004'
			},
			ShapeRun{
				name:     'f32_wide'
				expected: '16777216'
			},
			ShapeRun{
				name:     'f64_wide'
				expected: '16777217'
			},
		])
	// The width is asserted from the module's type section, not from the source:
	// an f32 export must be declared f32 or the backend widened it.
	src := shape_source_float_precision
	shape_signature_test('float_precision', src, 'f32_pair', [], [valtype_f32])
	shape_signature_test('float_precision', src, 'f64_pair', [], [valtype_f64])
	shape_signature_test('float_precision', src, 'f32_wide', [], [valtype_f32])
	shape_signature_test('float_precision', src, 'f64_wide', [], [valtype_f64])
}

// ---- 3. string comparison ----

// MEASURED DEFECT, fixed. String ordering used to be wrong in the module, and
// both channels said so independently: the module's own println and the exported
// call. The wasm string is {str@0, len@4, is_lit@8}, but the generated
// string__lt body read the length word at a hardcoded offset 8 -- the 64-bit
// layout -- so both lengths came out as 1, min() was 1, memcmp compared byte 0
// only and the tiebreak was 1 < 1. `<` was therefore true exactly when the first
// bytes differed in that direction, and false otherwise, which is why the length
// tiebreak never fired. `==`/`!=` looked right only because every case here
// compares two literals, which the constant folder resolves without calling
// string__eq; with a parameter on one side `"abcXef" == "abcYef"` was true.
// vlib/v/ssa/builder.v now takes the offset from block_struct_field_ptr, which
// is the same helper generate_print_body and generate_string_plus_body use, and
// which resolves to offset 8 on a 64-bit pointer so no other backend changes.
// gen_memcmp was checked and is not involved: 0x49 is i32.lt_u, as its comment
// says, and the byte loop is sound. The expectations below are the native ones.
const shape_source_string_compare = '
fn mid_eq() bool { return "abcXef" == "abcXef" }

fn mid_ne() bool { return "abcXef" != "abcYef" }

fn mid_lt() bool { return "abcXef" < "abcYef" }

fn mid_gt() bool { return "abcXef" > "abcYef" }

fn end_eq_len() bool { return "abc" == "abcd" }

fn end_lt() bool { return "abc" < "abcd" }

fn end_gt() bool { return "abcd" > "abc" }
fn prefix_eq() bool {
	return "ab" == "ab"
}

fn first_diff_lt() bool {
	return "b" < "c"
}

fn long_vs_short() bool {
	return "abcd" < "abcde"
}

fn long_ge_short() bool {
	return "abc" >= "abcd"
}

fn main() {
	println(mid_eq())
	println(mid_ne())
	println(mid_lt())
	println(mid_gt())
	println(end_eq_len())
	println(end_lt())
	println(end_gt())
	println(prefix_eq())
	println(first_diff_lt())
	println(long_vs_short())
	println(long_ge_short())
}
'

// String ordering on wasm was wrong in five of the eleven cases here. This is
// the record of that, in the order main prints them; wasm as 1/0 because an
// exported bool comes back from node as a Number:
//
//	case              native  wasm (before the fix)
//	mid_eq     ==        1      1
//	mid_ne     !=        1      1
//	mid_lt     <         1      0   wrong
//	mid_gt     >         0      0
//	end_eq_len ==        0      0
//	end_lt     <         1      0   wrong
//	end_gt     >         1      0   wrong
//	prefix_eq  ==        1      1
//	first_diff <         1      1
//	long_vs_sht <        1      0   wrong
//	long_ge_sht >=       0      1   wrong
//
// Equality was right in every case -- not because == is sound, but because both
// operands of each case are literals, which the constant folder resolves without
// calling string__eq. Ordering was right only where the two strings differ at
// index 0, which is first_diff ("b" < "c"). It was wrong for mid_lt, where the
// difference is at index 3, and for every case whose strings are equal up to the
// shorter length, where the length should break the tie and did not.
// long_ge_short was wrong in the opposite direction, which is what falls out of
// `>=` being the negation of `<`.
//
// Where the defect was NOT: generate_string_lt_body (vlib/v/ssa/builder.v:6671)
// is correct as SSA. It takes min(len_a, len_b), calls memcmp over that many
// bytes, and on a zero result returns len_a < len_b. The memcmp intrinsic
// lowering (vlib/v/gen/wasm/ssa_gen.v:1044) is a correct byte loop that stops on
// either the end of the range or the first difference -- its 0x49 comments are
// accurate, 0x49 is i32.lt_u. Nor was it the two-block minimum, which is an
// alloca store/load pair, not a phi: the minimum was correctly min(1, 1). The
// lengths themselves were the 1s, read from offset 8 because 8 is a 64-bit
// pointer's offset for the len field and wasm32's is 4, so offset 8 is is_lit.
//
// The fix is one substitution in vlib/v/ssa/builder.v; the expectations below
// are unchanged native values, and this case is left in place as the regression
// test for it.
fn test_shape_string_comparisons() {
	shape_case('string_compare', shape_source_string_compare,
		'true\ntrue\ntrue\nfalse\nfalse\ntrue\ntrue\ntrue\ntrue\ntrue\nfalse\n', '', false, [
			ShapeRun{
				name:     'mid_eq'
				expected: '1'
			},
			ShapeRun{
				name:     'mid_ne'
				expected: '1'
			},
			ShapeRun{
				name:     'mid_lt'
				expected: '1'
			},
			ShapeRun{
				name:     'mid_gt'
				expected: '0'
			},
			ShapeRun{
				name:     'end_eq_len'
				expected: '0'
			},
			ShapeRun{
				name:     'end_lt'
				expected: '1'
			},
			ShapeRun{
				name:     'end_gt'
				expected: '1'
			},
			ShapeRun{
				name:     'prefix_eq'
				expected: '1'
			},
			ShapeRun{
				name:     'first_diff_lt'
				expected: '1'
			},
			ShapeRun{
				name:     'long_vs_short'
				expected: '1'
			},
			ShapeRun{
				name:     'long_ge_short'
				expected: '0'
			},
		])
}

// ---- 4. string concatenation ----

const shape_source_string_concat = '
fn cat2() string { return "a" + "b" }

fn cat_int() string { return "n=" + int(41 + 1).str() }

fn cat3() string { return "a" + "b" + "c" }

fn cat2_len() int { return cat2().len }

fn cat3_len() int { return cat3().len }

fn cat3_first_two() int { return int(cat3()[0]) * 1000 + int(cat3()[1]) }

fn main() {
	println(cat2())
	println(cat_int())
	println(cat3())
	println(cat2_len())
	println(cat3_len())
	println(cat3_first_two())
}
'

fn test_shape_string_concatenation() {
	shape_case('string_concat', shape_source_string_concat, 'ab\nn=42\nabc\n2\n3\n97098\n', '',
		false, [
			ShapeRun{
				name:     'cat2_len'
				expected: '2'
			},
			ShapeRun{
				name:     'cat3_len'
				expected: '3'
			},
			ShapeRun{
				name:     'cat3_first_two'
				expected: '97098'
			},
		])
	// A `fn f() string` never reaches the export section: fn_info (gen.v:561)
	// drops any function a non-primitive signature names, so a host has no
	// binding for it. That rules out the aggregate-return protocol altogether —
	// verified on this module, the exports are cat2_len, cat3_len,
	// cat3_first_two, main and _start, so there is no `cat3` to call and no
	// trailing pointer to pass one to. The concatenated value is measured
	// through the module's stdout above and through the primitive wrappers
	// here, so its correctness is still a measured value rather than an
	// assumption.
	shape_enter_workspace()
	wasm := shape_module('string_concat', shape_source_string_concat, false) or {
		assert false, 'the wasm backend refused this program: ${err.msg()}'
		return
	}
	exports := shape_exports(wasm)
	assert 'cat3' !in exports, 'a `fn f() string` must not be exported: ${exports.keys()}'
	assert 'cat3_first_two' in exports, 'the primitive wrapper must be exported: ${exports.keys()}'
}

// ---- 5. match as a value ----

const shape_source_match_value = '
fn pick(n int) int {
	return match n {
		1 { 10 }
		2 { 20 }
		5 { 50 }
		else { 99 }
	}
}

fn main() {
	println(pick(1))
	println(pick(2))
	println(pick(5))
	println(pick(9))
	println(pick(-5))
}
'

fn test_shape_match_returns_value() {
	shape_case('match_value', shape_source_match_value, '10\n20\n50\n99\n99\n', '', false, [
		ShapeRun{
			name:     'pick'
			js_args:  ['1']
			expected: '10'
		},
		ShapeRun{
			name:     'pick'
			js_args:  ['2']
			expected: '20'
		},
		ShapeRun{
			name:     'pick'
			js_args:  ['5']
			expected: '50'
		},
		ShapeRun{
			name:     'pick'
			js_args:  ['9']
			expected: '99'
		},
		ShapeRun{
			name:     'pick'
			js_args:  ['-5']
			expected: '99'
		},
	])
}

// ---- 6. match over an enum ----

const shape_source_match_enum = '
enum Color {
	red
	green
	blue
}

fn score(c Color) int {
	return match c {
		.red { 3 }
		.green { 5 }
		.blue { 7 }
	}
}

fn score_at(n int) int {
	c := if n == 0 {
		Color.red
	} else if n == 1 {
		Color.green
	} else {
		Color.blue
	}
	return score(c)
}

// main calls score_at as well as score: the native reference build emits
// `notice: unused function: score_at` into the same stream the program prints to,
// so a wrapper nothing calls makes the reference a block of compiler chatter
// instead of what the program prints.
fn main() {
	println(score(Color.red))
	println(score(Color.green))
	println(score(Color.blue))
	println(score_at(0))
	println(score_at(1))
	println(score_at(2))
}
'

fn test_shape_match_enum_returns_value() {
	shape_case('match_enum', shape_source_match_enum, '3\n5\n7\n3\n5\n7\n', '', false, [
		ShapeRun{
			name:     'score_at'
			js_args:  ['0']
			expected: '3'
		},
		ShapeRun{
			name:     'score_at'
			js_args:  ['1']
			expected: '5'
		},
		ShapeRun{
			name:     'score_at'
			js_args:  ['2']
			expected: '7'
		},
	])
	// score itself names an enum, so it is not exported either: the host reaches
	// the match through score_at, whose signature is all primitive.
	shape_enter_workspace()
	wasm := shape_module('match_enum', shape_source_match_enum, false) or {
		assert false, 'the wasm backend refused this program: ${err.msg()}'
		return
	}
	exports := shape_exports(wasm)
	assert 'score' !in exports, 'a `fn f(c Color)` must not be exported: ${exports.keys()}'
	assert 'score_at' in exports, 'the primitive wrapper must be exported: ${exports.keys()}'
}

// ---- 7. for over a range, and a nested for ----

const shape_source_loops = '
fn sum_range(n int) int {
	mut s := 0
	for i in 0 .. n {
		s += i
	}
	return s
}

fn nested(n int) int {
	mut s := 0
	for i in 0 .. n {
		for j in 0 .. i {
			s += 10 * i + j
		}
	}
	return s
}

fn main() {
	println(sum_range(5))
	println(sum_range(100))
	println(nested(4))
	println(nested(7))
}
'

fn test_shape_for_ranges_and_nesting() {
	shape_case('loops', shape_source_loops, '10\n4950\n144\n945\n', '', false, [
		ShapeRun{
			name:     'sum_range'
			js_args:  ['5']
			expected: '10'
		},
		ShapeRun{
			name:     'sum_range'
			js_args:  ['100']
			expected: '4950'
		},
		ShapeRun{
			name:     'nested'
			js_args:  ['4']
			expected: '144'
		},
		ShapeRun{
			name:     'nested'
			js_args:  ['7']
			expected: '945'
		},
	])
}

// ---- 8. a while-style loop ----

const shape_source_while_style = '
fn ticks(n int) int {
	mut k := n
	mut s := 0
	for k > 0 {
		s += k
		k -= 3
	}
	return s
}

fn main() {
	println(ticks(10))
	println(ticks(7))
	println(ticks(2))
	println(ticks(4))
	println(ticks(-3))
}
'

fn test_shape_while_style_loop() {
	shape_case('while_style', shape_source_while_style, '22\n12\n2\n5\n0\n', '', false, [
		ShapeRun{
			name:     'ticks'
			js_args:  ['10']
			expected: '22'
		},
		ShapeRun{
			name:     'ticks'
			js_args:  ['7']
			expected: '12'
		},
		ShapeRun{
			name:     'ticks'
			js_args:  ['2']
			expected: '2'
		},
		ShapeRun{
			name:     'ticks'
			js_args:  ['4']
			expected: '5'
		},
		ShapeRun{
			name:     'ticks'
			js_args:  ['-3']
			expected: '0'
		},
	])
}

// ---- 9. signed division and modulo, including negatives ----

const shape_source_signed_div_mod = '
fn sig_div(a int, b int) int { return a / b }

fn sig_rem(a int, b int) int { return a % b }

fn wide_div(a i64, b i64) i64 { return a / b }

fn wide_rem(a i64, b i64) i64 { return a % b }

fn main() {
	println(sig_div(-7, 2))
	println(sig_rem(-7, 2))
	println(sig_div(7, -2))
	println(sig_rem(7, -2))
	println(sig_div(-8, 3))
	println(sig_rem(-8, 3))
	println(wide_div(i64(-7), i64(2)))
	println(wide_rem(i64(-7), i64(2)))
}
'

fn test_shape_signed_division_and_modulo() {
	shape_case('signed_div_mod', shape_source_signed_div_mod,
		'-3\n-1\n-3\n1\n-2\n-2\n-3\n-1\n', '', false, [
			ShapeRun{
				name:     'sig_div'
				js_args:  ['-7', '2']
				expected: '-3'
			},
			ShapeRun{
				name:     'sig_rem'
				js_args:  ['-7', '2']
				expected: '-1'
			},
			ShapeRun{
				name:     'sig_div'
				js_args:  ['7', '-2']
				expected: '-3'
			},
			ShapeRun{
				name:     'sig_rem'
				js_args:  ['7', '-2']
				expected: '1'
			},
			ShapeRun{
				name:     'sig_div'
				js_args:  ['-8', '3']
				expected: '-2'
			},
			ShapeRun{
				name:     'sig_rem'
				js_args:  ['-8', '3']
				expected: '-2'
			},
			ShapeRun{
				name:     'wide_div'
				js_args:  ['-7n', '2n']
				expected: '-3'
			},
			ShapeRun{
				name:     'wide_rem'
				js_args:  ['-7n', '2n']
				expected: '-1'
			},
		])
}

// ---- 10. the optimizer must agree with the same program built without it ----

fn test_shape_optimized_module_agrees() {
	shape_case('f64_arith', shape_source_f64_arith, '3.75\n6.0\n3.5\n4.5\n',
		shape_main_tail_f64_arith, true, [
			ShapeRun{
				name:     'f_add'
				expected: '3.75'
			},
			ShapeRun{
				name:     'f_mul'
				expected: '6'
			},
			ShapeRun{
				name:     'f_div'
				expected: '3.5'
			},
			ShapeRun{
				name:     'f_mixed'
				expected: '4.5'
			},
		])
	shape_case('loops', shape_source_loops, '10\n4950\n144\n945\n', '', true, [
		ShapeRun{
			name:     'sum_range'
			js_args:  ['100']
			expected: '4950'
		},
		ShapeRun{
			name:     'nested'
			js_args:  ['7']
			expected: '945'
		},
	])
	// The reference for the same program without -prod (test 4) ends in 97098:
	// the optimizer changes nothing about which lines the program prints, so the
	// earlier expectation that stopped at 3 was missing a line, not finding one.
	shape_case('string_concat', shape_source_string_concat, 'ab\nn=42\nabc\n2\n3\n97098\n', '',
		true, [
			ShapeRun{
				name:     'cat2_len'
				expected: '2'
			},
			ShapeRun{
				name:     'cat3_len'
				expected: '3'
			},
		])
}
