module wasm

import os

// This suite proves the driver path, not just the code generator. Every other
// file here drives SSAGen directly, which cannot catch a regression in file
// selection, in the wasm file family, or in the printing declarations: a
// module that lowers perfectly can still never be emitted by `v -b wasm`.
//
// So each case writes a V program, runs the real compiler on it, and executes
// the module it produces under node with a wasi_snapshot_preview1 host. That
// is the only level at which the whole chain is exercised at once.

const tour_program = '
struct Point {
	x int
	y int
}

fn fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n - 1) + fib(n - 2)
}

fn sum_to(n int) int {
	mut s := 0
	for i in 0 .. n {
		s += i
	}
	return s
}

fn main() {
	a := [1, 2, 3, 4, 5]
	mut t := 0
	for v in a {
		t += v
	}
	println("tour array sum=\${t}")
	println("tour fib10=\${fib(10)}")
	println("tour sum100=\${sum_to(100)}")
	p := Point{
		x: 30
		y: 4
	}
	println("tour point=\${p.x * 100 + p.y}")
	s := "abc"
	println("tour len=\${s.len} upper=\${s.to_upper()}")
	println("tour exit=7")
	exit(7)
}
'

const tour_expected = [
	'tour array sum=15',
	'tour fib10=55',
	'tour sum100=4950',
	'tour point=3004',
	'tour len=3 upper=ABC',
	'tour exit=7',
]

const tour_host_mjs = "
import { readFileSync } from 'node:fs';
const bytes = readFileSync(process.argv[2]);
class ProcExit extends Error {
	constructor(code) {
		super(String(code));
		this.code = code;
	}
}
let memory = null;
function u32at(addr) {
	return new DataView(memory.buffer).getUint32(addr, true);
}
const wasi = {
	wasi_snapshot_preview1: {
		fd_write(fd, iovs, iovsLen, nwritten) {
			memory = instance.exports.memory;
			const dv = new DataView(memory.buffer);
			let total = 0;
			for (let i = 0; i < iovsLen; i++) {
				const ptr = dv.getUint32(iovs + i * 8, true);
				const len = dv.getUint32(iovs + i * 8 + 4, true);
				process.stdout.write(Buffer.from(new Uint8Array(memory.buffer, ptr, len)));
				total += len;
			}
			dv.setUint32(nwritten, total, true);
			return 0;
		},
		proc_exit(code) {
			throw new ProcExit(code);
		},
	},
};
let instance = null;
try {
	const out = await WebAssembly.instantiate(bytes, wasi);
	instance = out.instance;
	instance.exports._start();
} catch (e) {
	if (e instanceof ProcExit) {
		console.error('PROC_EXIT=' + e.code);
	} else {
		console.error('RUN_ERROR ' + e.message);
	}
}
"

fn tour_workspace() string {
	custom := os.getenv('TOUR_SCRATCH')
	if custom.len > 0 {
		return custom
	}
	dir := os.join_path(os.vtmp_dir(), 'wasm_tour_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	return dir
}

// tour_node_bin locates node, without which the module can be compiled and
// validated but not executed. Every case skips rather than fails, because a
// missing node says nothing about the backend.
fn tour_node_bin() ?string {
	node := os.find_abs_path_of_executable('node') or { return none }
	return node
}

// tour_compile runs the real compiler on one program and returns the module.
fn tour_compile(tag string, source string) ![]u8 {
	dir := os.join_path(tour_workspace(), tag)
	os.mkdir_all(dir) or { panic(err) }
	src := os.join_path(dir, '${tag}.v')
	os.write_file(src, source) or { panic(err) }
	// V appends .exe on Windows, but the derive here is the wasm module, so the
	// output name is used exactly as given.
	out := os.join_path(dir, '${tag}.c')
	res := os.exec([@VEXE, '-gc', 'none', '-b', 'wasm', '-o', out, src])
	if res.exit_code != 0 {
		return error('v -b wasm failed: ${res.output}')
	}
	return os.read_bytes(out)!
}

fn tour_run(tag string, module []u8) (string, int) {
	node := tour_node_bin() or { return '', -1 }
	dir := os.join_path(tour_workspace(), tag, 'node')
	os.mkdir_all(dir) or { panic(err) }
	wasm := os.join_path(dir, '${tag}.wasm')
	os.write_file_array(wasm, module) or { panic(err) }
	host := os.join_path(dir, 'host.mjs')
	os.write_file(host, tour_host_mjs) or { panic(err) }
	res := os.exec([node, host, wasm])
	return res.output, res.exit_code
}

// test_tour_driver_path_emits_a_runnable_module compiles a program that uses
// the four gates this branch lifted and executes it. The values are the proof:
// an array with for-in, recursion, a range loop and a struct literal of
// primitives, each interpolated into a line of stdout.
fn test_tour_driver_path_emits_a_runnable_module() {
	module := tour_compile('tour', tour_program) or {
		assert false, 'the wasm build refused a program the gates should admit: ${err.msg()}'
		return
	}
	// A wasm module starts with the magic and a version word. A C file would
	// not, so this also proves the wasm backend ran rather than the C one.
	assert module.len > 8, 'the output is too short to be a wasm module'
	assert module[0] == 0x00 && module[1] == 0x61 && module[2] == 0x73
		&& module[3] == 0x6d, 'the output does not start with the wasm magic'

	node := tour_node_bin() or {
		eprintln('> node unavailable; the module was validated but not executed')
		return
	}
	out, code := tour_run('tour', module)
	// The host prints its own PROC_EXIT marker to stderr, which os.exec merges
	// into the same stream, so it is stripped before the guest's lines are read.
	guest := out.split_into_lines().filter(it.len > 0 && !it.starts_with('PROC_EXIT='))
	exit_seen := out.contains('PROC_EXIT=7')
	assert exit_seen, 'the guest did not report the exit code it was asked for: ${out}'
	assert guest.len == tour_expected.len, 'expected ${tour_expected.len} lines, got ${guest.len}'
	for i, want in tour_expected {
		assert guest[i] == want, 'line ${i}: got `${guest[i]}`, want `${want}`'
	}
	assert code == 0, 'the host reported a non-zero exit: ${out}'
}

// test_tour_module_declares_the_wasi_import_it_prints_through checks the import
// section, because a module that runs by accident on an empty host would hide a
// missing fd_write. The string is present only if the compiler synthesised the
// print path rather than leaving an unresolved external.
fn test_tour_module_declares_the_wasi_import_it_prints_through() {
	module := tour_compile('imports', tour_program) or {
		assert false, 'the wasm build refused the import probe: ${err.msg()}'
		return
	}
	text := module.bytestr()
	assert text.contains('wasi_snapshot_preview1'), 'no wasi namespace in the module'
	assert text.contains('fd_write'), 'no fd_write import in the module'
	assert text.contains('proc_exit'), 'no proc_exit import in the module'
	assert text.contains('_start'), 'no _start export in the module'
}

// test_tour_float_is_refused_not_misanswered is the known gap, pinned. A
// program that prints a float is not supposed to compile today: strconv's
// formatter lives in f64_str.c.v, a C-backend file, so wasm has no body for it
// and the SSA builder panics on an unregistered f64.str. This case exists so
// that the day the gap is closed it fails and has to be rewritten, rather than
// passing silently with a value nobody checked.
fn test_tour_float_is_refused_not_misanswered() {
	source := '
fn main() {
	f := 1.5
	println("f=\${f}")
}
'
	dir := os.join_path(tour_workspace(), 'float')
	os.mkdir_all(dir) or { panic(err) }
	src := os.join_path(dir, 'float.v')
	os.write_file(src, source) or { panic(err) }
	out := os.join_path(dir, 'float.c')
	res := os.exec([@VEXE, '-gc', 'none', '-b', 'wasm', '-o', out, src])
	assert res.exit_code != 0, 'a float program compiled, so the gap may be closed: ${res.output}'
	assert res.output.contains('f64.str'), 'a float program failed for another reason: ${res.output}'
}

// Struct parameters and results use aggregate pointers and hidden sret slots.
// Runtime values verify that both sides of each function boundary agree.
fn test_tour_struct_parameter_lowers_as_the_pointer_it_is() {
	// An aggregate arrives as a pointer, and on a 32-bit target a pointer is the
	// i32 the signature already described, so a struct parameter is admitted the
	// same way a primitive one is. The assertion is on the value, because a
	// parameter that arrives as garbage would compile just as happily.
	param := '
struct P {
	x int
	y int
}

fn use(p P) int {
	return p.x * 100 + p.y
}

fn main() {
	q := P{
		x: 30
		y: 4
	}
	println(use(q).str())
}
'
	dir := os.join_path(tour_workspace(), 'structparam')
	os.mkdir_all(dir) or { panic(err) }
	src := os.join_path(dir, 'structparam.v')
	os.write_file(src, param) or { panic(err) }
	out := os.join_path(dir, 'structparam.c')
	res := os.exec([@VEXE, '-gc', 'none', '-b', 'wasm', '-o', out, src])
	assert res.exit_code == 0, 'a struct parameter was refused: ${res.output}'
	if node := tour_node_bin() {
		wout, _ := tour_run('structparam', os.read_bytes(out)!)
		guest := wout.split_into_lines().filter(it.len > 0 && !it.contains('PROC_EXIT'))
		assert guest.len == 1 && guest[0] == '3004', 'struct parameter gave `${wout}`, want 3004'
	}
}

// A struct return travels through a hidden sret pointer: the caller supplies
// the slot and the function's own result type is void. That is what the
// signature below describes, so the assertion is on the value rather than on
// the build succeeding, because a return that arrived as garbage would compile
// just as happily.
fn test_tour_struct_return_lowers_through_the_sret_slot() {
	ret := '
struct P {
	x int
	y int
}

fn mk() P {
	return P{
		x: 30
		y: 4
	}
}

fn main() {
	p := mk()
	println((p.x * 100 + p.y).str())
}
'
	dir := os.join_path(tour_workspace(), 'structret')
	os.mkdir_all(dir) or { panic(err) }
	src := os.join_path(dir, 'structret.v')
	os.write_file(src, ret) or { panic(err) }
	out := os.join_path(dir, 'structret.c')
	res := os.exec([@VEXE, '-gc', 'none', '-b', 'wasm', '-o', out, src])
	assert res.exit_code == 0, 'a struct return was refused: ${res.output}'
	if node := tour_node_bin() {
		wout, _ := tour_run('structret', os.read_bytes(out)!)
		guest := wout.split_into_lines().filter(it.len > 0 && !it.contains('PROC_EXIT'))
		assert guest.len == 1 && guest[0] == '3004', 'struct return gave `${wout}`, want 3004'
	}
}

// An option return takes the same shape, because an option is a struct_t in SSA
// with a leading `ok`. The value proves the payload survived the sret copy.
fn test_tour_option_return_carries_its_payload() {
	ret := '
fn get() ?int {
	return 42
}

fn main() {
	if v := get() {
		println(v.str())
	}
}
'
	dir := os.join_path(tour_workspace(), 'optret')
	os.mkdir_all(dir) or { panic(err) }
	src := os.join_path(dir, 'optret.v')
	os.write_file(src, ret) or { panic(err) }
	out := os.join_path(dir, 'optret.c')
	res := os.exec([@VEXE, '-gc', 'none', '-b', 'wasm', '-o', out, src])
	assert res.exit_code == 0, 'an option return was refused: ${res.output}'
	if node := tour_node_bin() {
		wout, _ := tour_run('optret', os.read_bytes(out)!)
		guest := wout.split_into_lines().filter(it.len > 0 && !it.contains('PROC_EXIT'))
		assert guest.len == 1 && guest[0] == '42', 'option return gave `${wout}`, want 42'
	}
}

fn test_tour_struct_primitive_pointer_field_roundtrips() {
	source := 'struct Reading {\n\tvalue &int\n}\nfn main() {\n\tvalue := 42\n\treading := Reading{value: &value}\n\tprintln(*reading.value)\n}\n'
	module := tour_compile('primitive_pointer', source) or { panic(err) }
	_ := tour_node_bin() or { return }
	out, code := tour_run('primitive_pointer', module)
	assert code == 0, out
	assert out.trim_space() == '42', out
}

fn test_tour_fixed_float_is_refused_not_misanswered() {
	source := 'fn main() {\n\tf := 1.5\n\tprintln("\${f:.2f}")\n}\n'
	tour_compile('fixed_float', source) or {
		assert err.msg().contains('wasm: unsupported external function'), err.msg()
		assert err.msg().contains('v3_f64_fixed'), err.msg()
		return
	}
	assert false, 'fixed float formatting must be rejected until it is implemented'
}

const fold_print_program = '
fn main() {
	r := "ab".repeat(3)
	println("fold repeat=\${r}")
	r0 := "ab".repeat(0)
	println("fold repeat0=\${r0}")
	a := [1, 2, 3]
	println("fold array=\${a}")
	b := [true, false]
	println("fold bools=\${b}")
}
'

const fold_print_expected = [
	'fold repeat=ababab',
	'fold repeat0=',
	'fold array=[1, 2, 3]',
	'fold bools=[true, false]',
]

fn test_fold_print_wasm_matches_native_text() {
	module := tour_compile('fold_print', fold_print_program) or {
		assert false, 'the wasm build refused a folded print program: ${err.msg()}'
		return
	}
	assert module.len > 8, 'the output is too short to be a wasm module'
	assert module[0] == 0x00 && module[1] == 0x61 && module[2] == 0x73
		&& module[3] == 0x6d, 'the output does not start with the wasm magic'

	node := tour_node_bin() or {
		eprintln('> node unavailable; the module was validated but not executed')
		return
	}
	out, code := tour_run('fold_print', module)
	guest := out.split_into_lines().filter(it.len > 0 && !it.starts_with('PROC_EXIT='))
	assert guest.len == fold_print_expected.len, 'expected ${fold_print_expected.len} lines, got ${guest.len}: ${out}'
	for i, want in fold_print_expected {
		assert guest[i] == want, 'line ${i}: got `${guest[i]}`, want `${want}`'
	}
	assert code == 0, 'the host reported a non-zero exit: ${out}'
}
