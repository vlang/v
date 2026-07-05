import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_fn_value_callee_uses_its_own_parameter_types() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fn_value_call_param_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_fn_value_call_param_input.v')
	os.write_file(src, 'fn replacement(mut name string, mut out []string) {
	out << name
}

fn render(replacement fn (string, mut []string)) string {
	mut out := []string{}
	name := "ok"
	replacement(name, mut out)
	return out.join("")
}

fn main() {
	println(render(fn (name string, mut out []string) {
		out << name
	}))
}
') or {
		panic(err)
	}
	c_path := os.join_path(os.temp_dir(), 'v3_fn_value_call_param_input.c')
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(c_path) or { panic(err) }
	assert !c_code.contains('replacement(&name,'), c_code
	assert c_code.contains('replacement(*(&name),') || c_code.contains('replacement(name,'), c_code
}
