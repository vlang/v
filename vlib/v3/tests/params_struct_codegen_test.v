import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_params_struct_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// run_good supports run good handling for v3 tests.
fn run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0
	assert !compile.output.contains('C compilation failed')

	run := os.execute(bin)
	assert run.exit_code == 0
	return run.output.trim_space()
}

// A `@[params]` struct argument can be supplied as trailing `key: value` call args.
// Regression: the args used to be dropped, generating `make(1, )` (empty arg, trailing comma).
fn test_params_struct_call_args_codegen() {
	v3_bin := build_v3()
	source := "@[params]\nstruct Cfg {\n\ta int\n\tb string\n\tc bool\n}\n\nfn make(x int, cfg Cfg) string {\n\treturn '\${x}|\${cfg.a}|\${cfg.b}|\${cfg.c}'\n}\n\nfn main() {\n\tprintln(make(1, a: 10, b: 'hi', c: true))\n\tprintln(make(2, b: 'yo'))\n\tprintln(make(3, Cfg{a: 5, b: 'z', c: true}))\n}\n"
	out := run_good(v3_bin, 'params_struct_codegen_input', source)
	assert out == '1|10|hi|true\n2|0|yo|false\n3|5|z|true'
}

// The standard-library call that originally exposed the bug:
// `strconv.atof64(s, allow_extra_chars: true)` reachable via `string.f64()`.
// Previously this generated `strconv__atof64(s, )` (dropped param, trailing comma),
// which failed C compilation. We only assert it compiles and runs here; atof64's
// numeric accuracy is covered elsewhere.
fn test_string_f64_params_struct_codegen() {
	v3_bin := build_v3()
	source := "fn main() {\n\tprintln('\${'3.0'.f64() > 0.0}')\n\tprintln('\${'2.0'.f32() > 0.0}')\n}\n"
	out := run_good(v3_bin, 'string_f64_params_input', source)
	assert out == 'true\ntrue'
}

fn test_pool_config_params_struct_can_be_omitted() {
	v3_bin := build_v3()
	source := "@[params]\nstruct PoolConfig {\n\tmax_open int\n}\n\nfn open_pool(name string, cfg PoolConfig) string {\n\treturn '\${name}|\${cfg.max_open}'\n}\n\nfn main() {\n\tprintln(open_pool('db'))\n\tprintln(open_pool('db', max_open: 4))\n}\n"
	out := run_good(v3_bin, 'pool_config_params_input', source)
	assert out == 'db|0\ndb|4'
}

fn test_interface_field_in_params_struct_codegen() {
	v3_bin := build_v3()
	source := 'interface Reader {\n\tread(mut buf []u8) !int\n}\n\nstruct Conn {}\n\nfn (c Conn) read(mut buf []u8) !int {\n\treturn 0\n}\n\nstruct Config {\n\treader Reader\n\tcap int\n}\n\nfn make(cfg Config) int {\n\treturn cfg.cap\n}\n\nfn main() {\n\tprintln(make(reader: Conn{}, cap: 8))\n}\n'
	out := run_good(v3_bin, 'params_interface_field_input', source)
	assert out == '8'
}

fn test_fixed_array_field_struct_init_codegen() {
	v3_bin := build_v3()
	source := "struct Item {\n\tx int\n}\n\nstruct Header {\n\tdata [2]Item\n\tcur_pos int\n}\n\nfn main() {\n\tmut h := Header{}\n\th.data[0] = Item{x: 4}\n\th.data[1] = Item{x: 9}\n\th.cur_pos = 2\n\tcombined := Header{\n\t\tdata: h.data\n\t\tcur_pos: h.cur_pos\n\t}\n\tprintln('\${combined.cur_pos}|\${combined.data[0].x}|\${combined.data[1].x}')\n}\n"
	out := run_good(v3_bin, 'fixed_array_field_struct_init_input', source)
	assert out == '2|4|9'
}
