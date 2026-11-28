import os
import v.tempname

fn test_builtin_const_macros_do_not_collide_with_locals() {
	dir := os.join_path(os.vtmp_dir(), 'v3_builtin_const_collision_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_builtin_const_collision_input')
	src := bin + '.v'
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
	compile := os.exec([@VEXE, '-W', src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('#define degree ('), c_code
	assert !c_code.contains('#define hashbits ('), c_code
	assert c_code.contains('builtin__degree'), c_code
	assert c_code.contains('#define builtin__hashbits ('), c_code
	assert c_code.contains('map degree ='), c_code
	assert c_code.contains('i64 hashbits = 5;'), c_code
}
