import os

const numeric_array_literal_vexe = @VEXE
const numeric_array_literal_tests_dir = os.dir(@FILE)
const numeric_array_literal_v3_dir = os.dir(numeric_array_literal_tests_dir)
const numeric_array_literal_vlib_dir = os.dir(numeric_array_literal_v3_dir)
const numeric_array_literal_v3_src = os.join_path(numeric_array_literal_v3_dir, 'v3.v')

fn numeric_array_literal_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_numeric_array_literal_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${numeric_array_literal_vexe} -gc none -path "${numeric_array_literal_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${numeric_array_literal_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_numeric_array_literals_keep_float_common_type() {
	v3_bin := numeric_array_literal_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_numeric_array_literal_input_${os.getpid()}.v')
	os.write_file(src, "
fn main() {
	xs := [-0.3, 0.0, 0.3, 0.6, 1.0, 1.5]
	mut sum := 0.0
	for x in xs {
		sum += x
	}
	assert sum > 3.09 && sum < 3.11

	ys := [1, 2, 3]
	mut isum := 0
	for y in ys {
		isum += y
	}
	assert isum == 6

	zs := [f32(1.25), f32(2.5)]
	mut zsum := f32(0)
	for z in zs {
		zsum += z
	}
	assert zsum > f32(3.74) && zsum < f32(3.76)

	ws := [f32(1.25), 2]
	mut wsum := f32(0)
	for w in ws {
		wsum += w
	}
	assert wsum > f32(3.24) && wsum < f32(3.26)
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_numeric_array_literal_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('array_new(sizeof(double), 0, 6)'), c_code
	assert c_code.contains('double x = *(double*)(array_get(xs,'), c_code
	assert c_code.contains('array_new(sizeof(int), 0, 3)'), c_code
	assert !c_code.contains('int x = *(int*)(array_get(xs,'), c_code
	assert c_code.contains('array_new(sizeof(float), 0, 2)'), c_code
	assert c_code.contains('float z = *(float*)(array_get(zs,'), c_code
	assert c_code.contains('float w = *(float*)(array_get(ws,'), c_code
	assert !c_code.contains('array_new(sizeof(double), 0, 2)'), c_code
}
