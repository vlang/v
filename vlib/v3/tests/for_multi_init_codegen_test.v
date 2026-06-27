import os

const for_multi_init_vexe = @VEXE
const for_multi_init_tests_dir = os.dir(@FILE)
const for_multi_init_v3_dir = os.dir(for_multi_init_tests_dir)
const for_multi_init_vlib_dir = os.dir(for_multi_init_v3_dir)
const for_multi_init_v3_src = os.join_path(for_multi_init_v3_dir, 'v3.v')

fn for_multi_init_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${for_multi_init_vexe} -gc none -path "${for_multi_init_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${for_multi_init_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_c_style_for_multi_init_does_not_swallow_following_fn() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_input_${os.getpid()}.v')
	os.write_file(src, "struct Iter {
	label string
}

fn scan_empty_post() int {
	mut total := 0
	for h, t := 0, 3; h <= t; {
		total += h + t
		h += 1
		t -= 1
	}
	return total
}

fn following_iterator() Iter {
	return Iter{
		label: 'ok'
	}
}

fn scan_with_post() int {
	mut total := 0
	for h, t := 0, 3; h <= t; h += 1 {
		total += h + t
		t -= 1
	}
	return total
}

fn indexed_for_in_score() int {
	xs := [2, 4, 6]
	mut total := 0
	for i, v in xs {
		total += i * v
	}
	return total
}

fn main() {
	assert scan_empty_post() == 6
	assert following_iterator().label == 'ok'
	assert scan_with_post() == 6
	assert indexed_for_in_score() == 16
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
