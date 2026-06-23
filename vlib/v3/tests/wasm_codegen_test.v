import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn v3_binary() string {
	v3_bin := os.join_path(os.vtmp_dir(), 'v3_wasm_codegen_test')
	build := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn compile_to_wasm(v3_bin string, src string, name string) string {
	src_path := os.join_path(os.vtmp_dir(), '${name}.v')
	out_path := os.join_path(os.vtmp_dir(), '${name}.wasm')
	os.write_file(src_path, src) or { panic(err) }
	os.rm(out_path) or {}
	res := os.execute('${os.quoted_path(v3_bin)} -b wasm -o ${os.quoted_path(out_path)} ${os.quoted_path(src_path)}')
	assert res.exit_code == 0, res.output
	assert os.exists(out_path), 'missing wasm output for ${name}'
	return out_path
}

// assert_valid_wasm checks the module header and that it is non-trivial.
fn assert_valid_wasm(path string) {
	bytes := os.read_bytes(path) or { panic(err) }
	assert bytes.len > 8, 'wasm too small'
	assert bytes[0] == 0x00 && bytes[1] == 0x61 && bytes[2] == 0x73 && bytes[3] == 0x6d, 'bad wasm magic'
	assert bytes[4] == 0x01 && bytes[5] == 0x00 && bytes[6] == 0x00 && bytes[7] == 0x00, 'bad wasm version'
}

fn node_path() ?string {
	path := os.find_abs_path_of_executable('node') or { return none }
	return path
}

// last_line returns the last non-empty line, ignoring any stderr noise such as
// node's WASI ExperimentalWarning that os.execute folds into the output.
fn last_line(out string) string {
	mut result := ''
	for line in out.split_into_lines() {
		if line.trim_space().len > 0 {
			result = line.trim_space()
		}
	}
	return result
}

fn run_node(node string, runner string, wasm string) os.Result {
	return os.execute('${os.quoted_path(node)} --no-warnings ${os.quoted_path(runner)} ${os.quoted_path(wasm)}')
}

fn test_wasm_hello_world() {
	v3_bin := v3_binary()
	wasm := compile_to_wasm(v3_bin, "fn main() {\n\tprintln('hello world')\n}\n", 'wasm_hello')
	assert_valid_wasm(wasm)

	node := node_path() or {
		eprintln('> node not found, skipping wasm execution check')
		return
	}
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	assert last_line(res.output) == 'hello world', res.output
}

fn test_wasm_control_flow_and_int_print() {
	v3_bin := v3_binary()
	src := "fn main() {\n\tmut sum := 0\n\tfor i := 0; i < 5; i++ {\n\t\tsum = sum + i\n\t}\n\tif sum > 5 {\n\t\tprintln(sum)\n\t} else {\n\t\tprintln(-1)\n\t}\n}\n"
	wasm := compile_to_wasm(v3_bin, src, 'wasm_flow')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	assert last_line(res.output) == '10', res.output
}

fn test_wasm_exported_functions() {
	v3_bin := v3_binary()
	src := 'fn add(a int, b int) int {\n\treturn a + b\n}\n\nfn fib(n int) int {\n\tif n < 2 {\n\t\treturn n\n\t}\n\treturn fib(n - 1) + fib(n - 2)\n}\n\nfn gcd(a int, b int) int {\n\tmut x := a\n\tmut y := b\n\tfor y != 0 {\n\t\tt := y\n\t\ty = x % y\n\t\tx = t\n\t}\n\treturn x\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_exports')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_call_exports.mjs')
	os.write_file(runner, exports_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	// runner prints "add fib gcd" results space-separated
	assert last_line(res.output) == '7 55 12', res.output
}

fn test_wasm_unsigned_i64_and_bool() {
	v3_bin := v3_binary()
	src := 'fn main() {\n\ta := u32(4000000000)\n\tprintln(a / u32(3))\n\tc := u64(10000000000)\n\tprintln(c * 2)\n\td := u64(1)\n\tprintln(d << 40)\n\tprintln(a > u32(3))\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_unsigned')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	lines := res.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	expected := ['1333333333', '20000000000', '1099511627776', 'true']
	assert lines.len >= expected.len, res.output
	for i, want in expected {
		got := lines[lines.len - expected.len + i]
		assert got == want, 'line ${i}: got ${got}, want ${want} (full: ${res.output})'
	}
}

fn test_wasm_f32_cast_and_unsigned_u64_print() {
	v3_bin := v3_binary()
	// f32->int/i64 truncation casts and full-range unsigned u64 printing.
	src := 'fn main() {\n\tf := f32(3.9)\n\tprintln(int(f))\n\tprintln(i64(f))\n\tg := f32(-2.7)\n\tprintln(int(g))\n\tprintln(u64(0) - u64(1))\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_f32_u64')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	lines := res.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	expected := ['3', '3', '-2', '18446744073709551615']
	assert lines.len >= expected.len, res.output
	for i, want in expected {
		got := lines[lines.len - expected.len + i]
		assert got == want, 'line ${i}: got ${got}, want ${want} (full: ${res.output})'
	}
}

fn test_wasm_signed_compound_float_postfix_and_u64_literal() {
	v3_bin := v3_binary()
	// signed /= %= >>=, unsigned /=, float ++/--, full-range u64 decimal literal.
	src := 'fn main() {\n\tmut x := -6\n\tx /= 2\n\tprintln(x)\n\tmut y := -7\n\ty %= 3\n\tprintln(y)\n\tmut z := -8\n\tz >>= 1\n\tprintln(z)\n\tmut u := u32(4000000000)\n\tu /= u32(2)\n\tprintln(u)\n\tmut f := 1.5\n\tf++\n\tprintln(int(f))\n\tmut g := f32(5.5)\n\tg--\n\tprintln(int(g))\n\tprintln(u64(18446744073709551615))\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_signed_float_u64')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	lines := res.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	expected := ['-3', '-1', '-4', '2000000000', '2', '4', '18446744073709551615']
	assert lines.len >= expected.len, res.output
	for i, want in expected {
		got := lines[lines.len - expected.len + i]
		assert got == want, 'line ${i}: got ${got}, want ${want} (full: ${res.output})'
	}
}

const wasi_runner_js = "import { WASI } from 'node:wasi';
import { readFile } from 'node:fs/promises';
const wasi = new WASI({ version: 'preview1', args: [], env: {} });
const bytes = await readFile(process.argv[2]);
const mod = await WebAssembly.compile(bytes);
const inst = await WebAssembly.instantiate(mod, wasi.getImportObject());
wasi.start(inst);
"

const exports_runner_js = "import { readFileSync } from 'node:fs';
const { instance: i } = await WebAssembly.instantiate(readFileSync(process.argv[2]));
const e = i.exports;
process.stdout.write(`\${e.add(3, 4)} \${e.fib(10)} \${e.gcd(48, 36)}`);
"
