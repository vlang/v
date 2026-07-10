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

fn numeric_array_literal_compact_c(c_code string) string {
	return c_code.replace(' ', '').replace('\t', '')
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

	fs := [f32(5.1), 3.1, 1.1, 9.1]
	mut fmin := fs[0]
	for f in fs {
		if f < fmin {
			fmin = f
		}
	}
	assert fmin == f32(1.1)

	gs := [1.0, f32(2.0)]
	mut gsum := f32(0)
	for g in gs {
		gsum += g
	}
	assert gsum > f32(2.99) && gsum < f32(3.01)

	hs := [f32(2.0), 1.0]
	mut hsum := f32(0)
	for h in hs {
		hsum += h
	}
	assert hsum > f32(2.99) && hsum < f32(3.01)

	ds := [1.0, 2.0]
	mut dsum := 0.0
	for d in ds {
		dsum += d
	}
	assert dsum > 2.99 && dsum < 3.01

	es := [f64(1.0), f32(2.0)]
	mut esum := 0.0
	for e in es {
		esum += e
	}
	assert esum > 2.99 && esum < 3.01

	alias_sum := add_all([MyAlias(5), MyAlias(7), MyAlias(3)])
	assert alias_sum == MyAlias((5 * 10 + 7 * 10) * 10 + 3 * 10)
	println('ok')
}

type MyAlias = i64

fn (a MyAlias) + (b MyAlias) MyAlias {
	return MyAlias(i64(a) * 10 + i64(b) * 10)
}

type UnusedAlias = i64

fn (a UnusedAlias) + (b UnusedAlias) UnusedAlias {
	return UnusedAlias(i64(a) + i64(b))
}

fn add_all[T](items []T) T {
	mut head := items[0]
	for item in items[1..] {
		head += item
	}
	return head
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
	c_compact := numeric_array_literal_compact_c(c_code)
	assert c_compact.contains('array_new(sizeof(double),0,6)'), c_code
	assert c_code.contains('double x = *(double*)(array_get(xs,'), c_code
	assert c_compact.contains('array_new(sizeof(int),0,3)'), c_code
	assert !c_code.contains('int x = *(int*)(array_get(xs,'), c_code
	assert c_compact.contains('array_new(sizeof(float),0,2)'), c_code
	assert c_code.contains('float z = *(float*)(array_get(zs,'), c_code
	assert c_code.contains('float w = *(float*)(array_get(ws,'), c_code
	assert c_compact.contains('array_new(sizeof(float),0,4)'), c_code
	assert c_code.contains('float f = *(float*)(array_get(fs,'), c_code
	assert c_code.contains('float g = *(float*)(array_get(gs,'), c_code
	assert c_code.contains('float h = *(float*)(array_get(hs,'), c_code
	assert c_code.contains('double d = *(double*)(array_get(ds,'), c_code
	assert c_code.contains('double e = *(double*)(array_get(es,'), c_code
	assert c_code.contains('add_all_T_MyAlias'), c_code
	assert !c_code.contains('add_all_T_i64'), c_code
	assert c_code.contains('MyAlias__plus'), c_code
	assert c_code.contains('head = MyAlias__plus(head, item);'), c_code
	assert !c_code.contains('head += item;'), c_code
	assert !c_code.contains('UnusedAlias__plus'), c_code
	assert !c_compact.contains('array_new(sizeof(double),0,4)'), c_code
}
