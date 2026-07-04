import os

const builtin_const_collision_vexe = @VEXE
const builtin_const_collision_tests_dir = os.dir(@FILE)
const builtin_const_collision_v3_dir = os.dir(builtin_const_collision_tests_dir)
const builtin_const_collision_vlib_dir = os.dir(builtin_const_collision_v3_dir)
const builtin_const_collision_v3_src = os.join_path(builtin_const_collision_v3_dir, 'v3.v')

fn builtin_const_collision_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_builtin_const_collision_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${builtin_const_collision_vexe} -gc none -path "${builtin_const_collision_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${builtin_const_collision_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_builtin_const_macros_do_not_collide_with_locals() {
	v3_bin := builtin_const_collision_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_builtin_const_collision_input_${os.getpid()}.v')
	os.write_file(src, "fn make_degree() map[string]int {
	mut degree := map[string]int{}
	degree['A'] = 1
	return degree
}

fn local_hashbits() int {
	mut hashbits := 5
	hashbits = hashbits + 1
	return hashbits
}

fn main() {
	result := make_degree()
	assert result['A'] == 1
	assert local_hashbits() == 6
	println('ok')
}
") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_builtin_const_collision_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('#define degree ('), c_code
	assert !c_code.contains('#define hashbits ('), c_code
	assert c_code.contains('#define builtin__degree ('), c_code
	assert c_code.contains('#define builtin__hashbits ('), c_code
	assert c_code.contains('map degree ='), c_code
	assert c_code.contains('int hashbits = 5;'), c_code
}
