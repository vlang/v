import os
import v.tempname

fn test_fn_value_callee_uses_its_own_parameter_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_fn_value_call_param_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }

	src := os.join_path(root, 'v3_fn_value_call_param_input.v')
	os.write_file(src, 'fn replacement(name string, mut out []string) {
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
	c_path := os.join_path(root, 'v3_fn_value_call_param_input.c')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	c_code := os.read_file(c_path) or { panic(err) }
	assert !c_code.contains('replacement(&name,'), c_code
	assert c_code.contains('replacement(*(&name),') || c_code.contains('replacement(name,'), c_code
}
