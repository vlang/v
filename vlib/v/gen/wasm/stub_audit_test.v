// stub_audit_test.v measures the compiler-synthesised SSA runtime functions that
// a wasm build runs instead of their real source bodies.
// register_basic_format_stubs (vlib/v/ssa/builder.v:2382) writes synthetic
// bodies for a set of builtins and skip_source_fn_in_module (:2087) drops the
// real ones, so anything wrong in that set is invisible to the checker and to a
// native build: only executing the wasm module shows it. A previous session
// measured `string.int` that way; this file asks what else in the set is wrong.
//
// Each case compiles one small V program through the pipeline the backend's own
// path uses - parser, checker, transform, the shared SSA builder, SSAGen -
// runs the module under node with WASI fd_write/proc_exit stubs, and compares
// the module's stdout against the same source compiled and run natively. The
// assertion is always the VALUE: a wasm value that disagrees with native is a
// finding, not a compile problem. Every audited program also exports primitive
// wrappers and prints them itself, so a wrong value shows up in the module
// stdout and in a channel a host could call.
//
// A build that reaches a runtime function the wasm stub set never registered
// does not return an error - it panics the SSA builder, which kills this
// process. Every case therefore runs inside a child invocation of this same
// test binary (stub_child_build), so one panicking program costs one case
// instead of the whole suite, and the panic text becomes the assertion.
//
// The reference compiler is chosen for its tree, not its speed: `@VEXE` here is
// a copy of another checkout's binary whose baked VROOT points at that checkout,
// so a native reference built with it measures the wrong vlib. The `v1.exe`
// sitting next to it has no baked root and resolves `vlib` from its own
// directory, i.e. this worktree.
module wasm

import encoding.hex
import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn stub_scratch() string {
	custom := os.getenv('STUB_SCRATCH')
	if custom.len > 0 {
		return custom
	}
	return os.join_path(os.home_dir(), 'AppData', 'Local', 'Temp', 'opencode', 'wasmstub')
}

fn stub_enter_workspace() {
	root := stub_scratch()
	os.mkdir_all(root) or { panic(err) }
	os.setenv('TMPDIR', os.join_path(root, 'vtmp'), true)
	os.setenv('VJOBS', '2', true)
}

fn stub_vexe() string {
	candidates := [@VEXE, os.getenv('VEXE'), os.join_path(os.getwd(), 'v'),
		os.join_path(os.getwd(), 'v.exe')]
	for c in candidates {
		if c.len > 0 && os.is_file(c) {
			return c
		}
	}
	panic('stub_vexe: no compiler found in ${candidates}')
}

// stub_ref_vexe returns a compiler whose vlib is this worktree. A binary in this
// tree with no baked VROOT resolves vlib from its own directory, and @VEXE is a
// copy of another checkout's binary, so its baked root points elsewhere.
fn stub_ref_vexe() string {
	dir := os.dir(@VEXE)
	bootstrap := os.join_path(dir, 'v1.exe')
	if os.is_file(bootstrap) {
		return bootstrap
	}
	return stub_vexe()
}

fn stub_module(tag string, source string) ![]u8 {
	dir := os.join_path(stub_scratch(), tag, 'wasm')
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
	mut g := SSAGen.new(m)
	g.configure(config.exports, config.init_fns, config.main_fn)
	g.gen() or { return error('gen: ${err.msg()}') }
	return g.mod.compile()
}

fn stub_native(tag string, source string) string {
	dir := os.join_path(stub_scratch(), tag, 'ref')
	os.mkdir_all(dir) or { panic(err) }
	path := os.join_path(dir, '${tag}.v')
	os.write_file(path, source) or { panic(err) }
	res := os.exec([stub_ref_vexe(), '-silent', 'run', path])
	assert res.exit_code == 0, '${tag}: native reference build failed with ${stub_ref_vexe()}'
	return res.output
}

const stub_mjs_host = "import assert from 'node:assert/strict';
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

fn stub_node(tag string, wasm []u8) string {
	node := os.find_abs_path_of_executable('node') or {
		panic('stub_node: node is required to run a wasm module, and it is not on PATH')
	}
	dir := os.join_path(stub_scratch(), tag, 'node')
	os.mkdir_all(dir) or { panic(err) }
	wasm_path := os.join_path(dir, '${tag}.wasm')
	os.write_file_array(wasm_path, wasm) or { panic(err) }
	runner := os.join_path(dir, 'check.mjs')
	os.write_file(runner, stub_mjs_host) or { panic(err) }
	res := os.exec([node, runner, wasm_path])
	assert res.exit_code == 0, '${tag}: node run failed: ${res.output}'
	return res.output
}

fn stub_stdout(out string) !string {
	for line in out.split_into_lines() {
		if line.starts_with('MODULE_STDOUT=') {
			return hex.decode(line.all_after('MODULE_STDOUT='))!.bytestr()
		}
	}
	return error('the runner printed no MODULE_STDOUT line')
}

// ---- the audited programs ----

const stub_source_int_format = '
fn main() {
	a := int(0).str()
	b := int(7).str()
	c := int(-7).str()
	d := int(2147483647).str()
	e := int(-2147483648).str()
	f := i64(-9223372036854775807).str()
	g := i64(9223372036854775807).str()
	h := i64(-9223372036854775808).str()
	i := i32(-1).str()
	j := i8(-128).str()
	k := u8(255).str()
	l := u64(18446744073709551615).str()
	m := u16(65535).str()
	n := i16(-32768).str()
	println(a)
	println(b)
	println(c)
	println(d)
	println(e)
	println(f)
	println(g)
	println(h)
	println(i)
	println(j)
	println(k)
	println(l)
	println(m)
	println(n)
	println(probe_int(0))
	println(probe_int(-7))
	println(probe_int(2147483647))
	println(probe_int(-2147483648))
	println(probe_i64(-9223372036854775807))
	println(probe_i64(9223372036854775807))
	println(probe_i64(-9223372036854775808))
	println(probe_uint(18446744073709551615))
	println(probe_bool(true))
	println(probe_bool(false))
}

fn probe_int(v int) int {
	s := v.str()
	return s.len * 100 + int(s[0])
}

fn probe_i64(v i64) int {
	s := v.str()
	return s.len * 100 + int(s[0])
}

fn probe_uint(v u64) int {
	s := v.str()
	return s.len * 100 + int(s[0])
}

fn probe_bool(v bool) int {
	s := v.str()
	return s.len * 100 + int(s[0])
}
'

const stub_source_string_int = '
fn main() {
	a := "0".int()
	b := "42".int()
	c := "-42".int()
	d := "+42".int()
	e := "".int()
	f := "0x1f".int()
	g := "0X1F".int()
	h := "0b101".int()
	i := "0o17".int()
	j := "12abc".int()
	k := "1_000".int()
	l := "99999999999999999999".int()
	m := "-99999999999999999999".int()
	n := "  42".int()
	o := "0x".int()
	p := "-0x10".int()
	q := "1__2".int()
	r := "0b".int()
	s := "0o8".int()
	t := "0xzz".int()
	println(a)
	println(b)
	println(c)
	println(d)
	println(e)
	println(f)
	println(g)
	println(h)
	println(i)
	println(j)
	println(k)
	println(l)
	println(m)
	println(n)
	println(o)
	println(p)
	println(q)
	println(r)
	println(s)
	println(t)
	println(probe_dec())
	println(probe_neg())
	println(probe_hex())
	println(probe_bin())
	println(probe_oct())
	println(probe_empty())
	println(probe_bad())
	println(probe_us())
	println(probe_over())
	println(probe_dyn(31))
}

// probe_dyn builds its input at runtime, so the parse cannot have been folded
// into the constant: this is the one string.int probe that can only be right if
// the synthetic body really runs.
fn probe_dyn(n int) int {
	s := "0x" + n.str()
	return s.int()
}

fn probe_dec() int { return "42".int() }

fn probe_neg() int { return "-42".int() }

fn probe_hex() int { return "0x1f".int() }

fn probe_bin() int { return "0b101".int() }

fn probe_oct() int { return "0o17".int() }

fn probe_empty() int { return "".int() }

fn probe_bad() int { return "12abc".int() }

fn probe_us() int { return "1_000".int() }

fn probe_over() int { return "99999999999999999999".int() }
'

const stub_source_bool_str = '
fn main() {
	a := true.str()
	b := false.str()
	println(a)
	println(b)
	println(probe_true())
	println(probe_false())
}

fn probe_true() int {
	s := true.str()
	return s.len * 100 + int(s[0])
}

fn probe_false() int {
	s := false.str()
	return s.len * 100 + int(s[0])
}
'

const stub_source_string_plus = '
fn main() {
	a := "a" + "b"
	b := "x" + "y" + "z"
	c := "n=" + int(42).str()
	d := "" + "end"
	e := "start" + ""
	f := a + b + c + d + e
	println(a)
	println(b)
	println(c)
	println(d)
	println(e)
	println(f)
	println(a.len)
	println(f.len)
	println(probe_two())
	println(probe_many())
}

fn probe_two() int {
	s := "a" + "b"
	return s.len * 100 + int(s[0])
}

fn probe_many() int {
	s := "a" + "b" + "c" + "d" + "e"
	return s.len * 100 + int(s[0])
}
'

const stub_source_interp = '
fn main() {
	n := 42
	s := "ab"
	r := `Z`
	println("[\${n}]")
	println("[\${n:-7}]")
	println("[\${n:7}]")
	println("[\${s:-7}]")
	println("[\${s:7}]")
	println("[\${n:05}]")
	println("[\${`Z`}]")
	println("[\${`Z`:-7}]")
	println("[\${n:08}]")
	println("[\${i64(-7):06}]")
	println("[\${u64(9):05}]")
	println("[\${u8(250):04}]")
	z := "\${n:05}"
	println(z)
	println(z.len)
	y := "\${u64(9):05}"
	println(y)
	println(y.len)
	x := "\${r}"
	println(x)
	println(x.len)
	println(probe_pad_left(42))
	println(probe_pad_right(42))
	println(probe_zpad_i32(42))
	println(probe_zpad_i64(i64(-7)))
	println(probe_zpad_u64(u64(9)))
	println(probe_char(`Z`))
}

// Every probe below takes its value as a parameter: an interpolated literal is
// folded by the transformer, so a probe built from a constant would measure the
// folder rather than the stub.
fn probe_pad_left(v int) int {
	s := "[\${v:-7}]"
	return s.len * 100 + int(s[1])
}

fn probe_pad_right(v int) int {
	s := "[\${v:7}]"
	return s.len * 100 + int(s[1])
}

fn probe_zpad_i32(v int) int {
	s := "[\${v:05}]"
	return s.len * 100 + int(s[1])
}

fn probe_zpad_i64(v i64) int {
	s := "[\${v:06}]"
	return s.len * 100 + int(s[1])
}

fn probe_zpad_u64(v u64) int {
	s := "[\${v:05}]"
	return s.len * 100 + int(s[1])
}

fn probe_char(c rune) int {
	s := "[\${c}]"
	return s.len * 100 + int(s[1])
}
'

const stub_source_interp_float = '
fn main() {
	f := 1.5
	g := 0.125
	h := 1234.5678
	println("[\${f:.2f}]")
	println("[\${f:.0f}]")
	println("[\${f:10.3f}]")
	println("[\${f:-10.3f}]")
	println("[\${g:.4f}]")
	println("[\${h:.1f}]")
	println(probe_fixed(1.5))
	println(probe_fixed_zero(1.5))
	println(probe_fixed_width(1.5))
}

// See the note in stub_source_interp: a literal is folded, so these take the
// value as a parameter.
fn probe_fixed(v f64) int {
	s := "[\${v:.2f}]"
	return s.len * 100 + int(s[1])
}

fn probe_fixed_zero(v f64) int {
	s := "[\${v:.0f}]"
	return s.len * 100 + int(s[1])
}

fn probe_fixed_width(v f64) int {
	s := "[\${v:10.3f}]"
	return s.len * 100 + int(s[1])
}
'

const stub_source_string_builtins = '
fn main() {
	up := "  Hello World  ".to_upper()
	println(up)
	low := "  Hello World  ".to_lower()
	println(low)
	tsp := "  Hello World  ".trim_space()
	println(tsp)
	tl := "  Hello World  ".trim_left(" ")
	println(tl)
	tr := "  Hello World  ".trim_right(" ")
	println(tr)
	c1 := "  Hello World  ".contains("World")
	println(c1)
	c2 := "abc".contains("z")
	println(c2)
	sw := "  Hello World  ".starts_with("  H")
	println(sw)
	ew := "  Hello World  ".ends_with("  ")
	println(ew)
	rp := "aXbXc".replace("X", "-")
	println(rp)
	println("  Hello World  ".len)
	println("abc".len)
	println("".len)
	println(int("  Hello World  "[0]))
	println(int("  Hello World  "["  Hello World  ".len - 1]))
	println(probe_upper())
	println(probe_lower())
	println(probe_trim())
	println(probe_trim_left())
	println(probe_trim_right())
	println(probe_replace())
}

fn probe_upper() int {
	s := "  Hello World  ".to_upper()
	return s.len * 100 + int(s[0])
}

fn probe_lower() int {
	s := "  Hello World  ".to_lower()
	return s.len * 100 + int(s[0])
}

fn probe_trim() int {
	s := "  Hello World  ".trim_space()
	return s.len * 100 + int(s[0])
}

fn probe_trim_left() int {
	s := "  Hello World  ".trim_left(" ")
	return s.len * 100 + int(s[0])
}

fn probe_trim_right() int {
	s := "  Hello World  ".trim_right(" ")
	return s.len * 100 + int(s[0])
}

fn probe_replace() int {
	s := "aXbXc".replace("X", "-")
	return s.len * 100 + int(s[0])
}
'

const stub_source_string_reverse = '
fn main() {
	rev := "abc".reverse()
	println(rev)
	println(probe_reverse())
}

fn probe_reverse() int {
	s := "abc".reverse()
	return s.len * 100 + int(s[0])
}
'

const stub_source_string_index = '
fn main() {
	hit := "hello".index("ll")
	miss := "hello".index("zz")
	println(hit)
	println(miss)
}
'

const stub_source_pointer = '
fn main() {
	mut buf := [u8(104), 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0]
	p := unsafe { &buf[0] }
	a := unsafe { p.vstring() }
	b := unsafe { p.vstring_with_len(5) }
	c := unsafe { tos2(p) }
	d := unsafe { tos_clone(p) }
	e := unsafe { p.vstring_with_len(0) }
	println(a)
	println(b)
	println(c)
	println(d)
	println(e)
	println(probe_vstring())
	println(probe_vstring_len())
	println(probe_tos2())
}

fn probe_vstring() int {
	mut buf := [u8(104), 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0]
	a := unsafe { (&buf[0]).vstring() }
	return a.len * 100 + int(a[0])
}

fn probe_vstring_len() int {
	mut buf := [u8(104), 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0]
	a := unsafe { (&buf[0]).vstring_with_len(5) }
	return a.len * 100 + int(a[0])
}

fn probe_tos2() int {
	mut buf := [u8(104), 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0]
	a := unsafe { tos2(&buf[0]) }
	return a.len * 100 + int(a[0])
}
'

// ---- the case table ----

// The audited set, with the registration site and the body it registers:
//
//	tos2, tos3, tos_clone, u8/char/byteptr/charptr.vstring[_literal][_with_len]
//	  builder.v:2386-2394 and 2493-2517 -> generate_tos2_body (:2989),
//	  generate_tos_clone_body (:3000), generate_vstring_with_len_body (:3012).
//	int_str                      builder.v:2399 -> generate_int_format_body (:2540)
//	bool_str                     builder.v:2405 -> generate_bool_str_body (:2527)
//	string.int / string__int     builder.v:2411, :2414 -> generate_string_int_body (:2661)
//	strconv__format_int          builder.v:2421 -> generate_int_format_body (dead)
//	strconv__format_uint         builder.v:2424 -> generate_int_format_body (unsigned)
//	strconv__f{32,64}_to_str_l[_with_dot] (8 names) builder.v:2430-2436
//	                             -> generate_const_string_body (:2520), the constant "0.0"
//	v3_string_pad                builder.v:2449 -> generate_string_pad_body (:2878)
//	v3_char_string               builder.v:2455 -> a constant "?"
//	v3_f64_fixed                 builder.v:2462 -> a constant "0.0"
//	v3_int_zpad / v3_i64_zpad    builder.v:2469, :2476 -> generate_int_zpad_passthrough_body (:2959)
//	v3_u64_zpad                  builder.v:2483 -> a constant "0"
//	string__plus, string_plus_many  builder.v:3022-3035 -> generate_string_plus_body (:3079),
//	                             generate_string_plus_many_body (:3121)
//	bench runtime stubs          builder.v:2973 -> a constant 0
//
// Each stub gets one case below. A stub whose wasm value is wrong is left red
// with both values in the failure message, and a stub that cannot be built at
// all is asserted against the compiler's own refusal text, so a refusal is
// visible rather than hidden behind a skipped case.

struct StubCase {
	tag    string
	stub   string
	source string
	native string
	// refuses, when non-empty, is the fragment of the compiler's own output this
	// case must see instead of a value. Two shapes end here: a wasm build that
	// reaches a runtime function the stub set never registered panics the SSA
	// builder, and a stub whose body needs a host function wasm has no binding
	// for is rejected by the encoder.
	refuses string
}

fn stub_case_refusals() []StubCase {
	return [
		StubCase{
			tag:     'interp_plain_float'
			stub:    'f64.str - not registered for wasm'
			source:  '
fn main() {
	f := 1.5
	println("[\${f}]")
}
'
			native:  '[1.5]\n'
			refuses: 'ssa: unknown function `f64.str`'
		},
		StubCase{
			tag:     'float_str_method'
			stub:    'f64.str - not registered for wasm'
			source:  '
fn main() {
	a := 1.5.str()
	println(a)
}
'
			native:  '1.5\n'
			refuses: 'ssa: unknown function `f64.str`'
		},
		StubCase{
			tag:     'float_strg'
			stub:    'f64.strg - not registered for wasm'
			source:  '
fn main() {
	a := 1.5.strg()
	println(a)
}
'
			native:  '1.5\n'
			refuses: 'ssa: unknown function `strg`'
		},
		StubCase{
			tag:     'interp_plus_sign'
			stub:    'v3_string_plus_sign - not registered for wasm'
			source:  '
fn main() {
	println("[\${42:+5}]")
}
'
			native:  '[  +42]\n'
			refuses: 'ssa: unknown function `v3_string_plus_sign`'
		},
		StubCase{
			tag:     'interp_float_exp'
			stub:    'v3_f64_exp - not registered for wasm'
			source:  '
fn main() {
	println("[\${1.5:e}]")
}
'
			native:  '[1.5]\n'
			refuses: 'ssa: unknown function `f64.str`'
		},
		StubCase{
			tag:     'interp_float_general'
			stub:    'v3_f64_general - not registered for wasm'
			source:  '
fn main() {
	println("[\${1.5:g}]")
}
'
			native:  '[1.5]\n'
			refuses: 'ssa: unknown function `f64__strg`'
		},
		StubCase{
			tag:     'string_split'
			stub:    'strings.split - no synthetic body, source body panics'
			source:  '
fn main() {
	sp := "a,b,c".split(",")
	println(sp)
	println(sp.len)
}
'
			native:  "['a', 'b', 'c']\n3\n"
			refuses: 'ssa: unknown function `split`'
		},
		StubCase{
			tag:     'string_repeat'
			stub:    'strings.repeat - no synthetic body, source body panics'
			source:  '
fn main() {
	s := "ab".repeat(3)
	println(s)
}
'
			native:  'ababab\n'
			refuses: 'ssa: unknown function `string__repeat`'
		},
		StubCase{
			tag:     'string_index'
			stub:    'strings.index - no synthetic body, source body panics'
			source:  stub_source_string_index
			native:  'Option(2)\nOption(none)\n'
			refuses: 'ssa: unknown function `index`'
		},
		StubCase{
			tag:     'string_reverse'
			stub:    'strings.reverse - no synthetic body, source body panics'
			source:  stub_source_string_reverse
			native:  'cba\n399\n'
			refuses: 'ssa: unknown function `reverse`'
		},
		StubCase{
			tag:     'pointer_strings'
			stub:    'tos2 / vstring / vstring_with_len / tos_clone'
			source:  stub_source_pointer
			native:  'hello world\nhello\nhello world\nhello world\n\n1204\n604\n1204\n'
			refuses: 'wasm: unsupported external function `C.strlen`'
		},
	]
}

fn stub_cases() []StubCase {
	return [
		StubCase{
			tag:    'int_format'
			stub:   'int_str / strconv__format_uint (generate_int_format_body)'
			source: stub_source_int_format
			native: '0\n7\n-7\n2147483647\n-2147483648\n-9223372036854775807\n' +
				'9223372036854775807\n-9223372036854775808\n-1\n-128\n255\n' +
				'18446744073709551615\n65535\n-32768\n148\n245\n1050\n1145\n' +
				'2045\n1957\n2045\n2049\n516\n602\n'
		},
		StubCase{
			tag:    'bool_str'
			stub:   'bool_str (generate_bool_str_body)'
			source: stub_source_bool_str
			native: 'true\nfalse\n516\n602\n'
		},
		StubCase{
			tag:    'string_int'
			stub:   'string.int (generate_string_int_body)'
			source: stub_source_string_int
			native: '0\n42\n-42\n42\n0\n31\n31\n5\n15\n12\n1000\n2147483647\n' +
				'-2147483648\n0\n0\n-16\n0\n0\n0\n0\n42\n-42\n31\n5\n15\n0\n' +
				'12\n1000\n2147483647\n49\n'
		},
		StubCase{
			tag:    'string_plus'
			stub:   'string__plus / string_plus_many'
			source: stub_source_string_plus
			native: 'ab\nxyz\nn=42\nend\nstart\nabxyzn=42endstart\n2\n17\n297\n597\n'
		},
		StubCase{
			tag:    'interp_pad'
			stub:   'v3_string_pad / v3_char_string / v3_int_zpad / v3_i64_zpad / v3_u64_zpad'
			source: stub_source_interp
			native: '[42]\n[42     ]\n[     42]\n[ab     ]\n[     ab]\n[00042]\n[Z]\n' +
				'[Z      ]\n[00000042]\n[-00007]\n[00009]\n[0250]\n00042\n5\n' +
				'00009\n5\nZ\n1\n952\n932\n748\n845\n748\n390\n'
		},
		StubCase{
			tag:    'interp_float_fixed'
			stub:   'v3_f64_fixed'
			source: stub_source_interp_float
			native: '[1.50]\n[2]\n[     1.500]\n[1.500     ]\n[0.1250]\n' +
				'[1234.6]\n649\n350\n1232\n'
		},
		StubCase{
			tag:    'string_builtins'
			stub:   'strings source bodies compiled through the SSA builder'
			source: stub_source_string_builtins
			native: '  HELLO WORLD  \n  hello world  \nHello World\nHello World  \n' +
				'  Hello World\ntrue\nfalse\ntrue\ntrue\na-b-c\n15\n3\n0\n32\n32\n' +
				'1532\n1532\n1172\n1372\n1332\n597\n'
		},
	]
}

fn stub_all_cases() []StubCase {
	mut all := stub_cases()
	for c in stub_case_refusals() {
		all << c
	}
	return all
}

// ---- the child protocol ----

fn stub_child_tag() string {
	for i, arg in os.args {
		if arg == '--stub-child' && i + 1 < os.args.len {
			return os.args[i + 1]
		}
	}
	return ''
}

fn stub_child_case(tag string) ?StubCase {
	for c in stub_all_cases() {
		if c.tag == tag {
			return c
		}
	}
	return none
}

fn stub_child(tag string) {
	c := stub_child_case(tag) or {
		println('CHILD ${tag}: NO SUCH CASE')
		return
	}
	wasm := stub_module(tag, c.source) or {
		println('CHILD ${tag}: REFUSED ${err.msg()}')
		return
	}
	out := stub_node(tag, wasm)
	stdout := stub_stdout(out) or {
		println('CHILD ${tag}: ${err.msg()}')
		return
	}
	println('CHILD ${tag}: BUILT ${hex.encode(stdout.bytes())}')
}

fn stub_child_build(tag string) string {
	res := os.exec([os.executable(), '--stub-child', tag])
	return res.output
}

fn before_each() {
	if stub_child_tag().len > 0 {
		stub_child(stub_child_tag())
		exit(0)
	}
}

// ---- the audit ----

fn stub_show(text string) string {
	return text.trim_space().replace('\n', '|')
}

fn stub_child_refusal(child string) string {
	for line in child.split_into_lines() {
		if line.contains('ssa: unknown function') || line.contains('REFUSED') {
			return line.trim_space()
		}
	}
	return child.trim_space()
}

fn stub_audit_case(c StubCase, mut wrong []string) {
	stub_enter_workspace()
	native := stub_native(c.tag, c.source)
	if native != c.native {
		wrong << '${c.tag}: native printed `${stub_show(native)}`, expected `${stub_show(c.native)}`'
		return
	}
	child := stub_child_build(c.tag)
	if c.refuses.len > 0 {
		if !child.contains(c.refuses) {
			wrong << '${c.tag}: expected a refusal `${c.refuses}`, the child printed `${stub_show(child)}`'
			return
		}
		println('  ${c.tag} REFUSED ${stub_child_refusal(child)}')
		return
	}
	mut built := ''
	for line in child.split_into_lines() {
		if line.starts_with('CHILD ${c.tag}: BUILT ') {
			built = line.all_after(': BUILT ')
			break
		}
	}
	if built.len == 0 {
		wrong << '${c.tag}: the wasm build did not succeed: ${stub_show(child)}'
		return
	}
	decoded := hex.decode(built) or {
		wrong << '${c.tag}: the child printed undecodable hex `${built}`'
		return
	}
	wasm_stdout := decoded.bytestr()
	if wasm_stdout != c.native {
		wrong << '${c.tag}: wasm printed `${stub_show(wasm_stdout)}`, native `${stub_show(c.native)}`'
		return
	}
	println('  ${c.tag} MATCH')
}

fn test_stub_audit() {
	stub_enter_workspace()
	mut wrong := []string{}
	for c in stub_all_cases() {
		stub_audit_case(c, mut wrong)
	}
	total := stub_all_cases().len
	println('STUB_AUDIT_DONE cases=${total} wrong=${wrong.len}')
	assert wrong.len == 0, '${wrong.len} of ${total} cases disagree: ' + wrong.join(' ||| ')
}
