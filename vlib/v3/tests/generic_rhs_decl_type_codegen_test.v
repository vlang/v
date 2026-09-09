import os

const generic_rhs_vexe = @VEXE
const generic_rhs_tests_dir = os.dir(@FILE)
const generic_rhs_v3_dir = os.dir(generic_rhs_tests_dir)
const generic_rhs_vlib_dir = os.dir(generic_rhs_v3_dir)
const generic_rhs_v3_src = os.join_path(generic_rhs_v3_dir, 'v3.v')

fn generic_rhs_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_rhs_decl_type_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_rhs_vexe} -gc none -path "${generic_rhs_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_rhs_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn generic_rhs_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_generic_rhs_decl_type_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'gm')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_rhs_decl_type' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'gm/gm.v'), 'module gm

pub fn pick[T](x T) T {
	return x
}

pub fn max2[T](a T, b T) T {
	if a > b {
		return a
	}
	return b
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import gm

fn pick(a i32) i32 {
	return i32(-100)
}

fn single_decl(x i32, y i32) i32 {
	a := gm.pick(x)
	return gm.max2(a, y)
}

fn multi_decl(x i32, y i32) i32 {
	a, b := gm.pick(x), gm.pick(y)
	return gm.max2(a, b)
}

fn condition_use(x i32, y i32) i32 {
	a, b := gm.pick(x), gm.pick(y)
	if gm.max2(a, b) > i32(0) {
		return gm.max2(a, b)
	}
	return i32(0)
}

fn if_expr_use(x i32, y i32) i32 {
	a, b := gm.pick(x), gm.pick(y)
	return if gm.max2(a, b) > i32(4) {
		gm.max2(a, b)
	} else {
		gm.pick(i32(4))
	}
}

fn local_homonym(x i32) i32 {
	a := gm.pick(x)
	return pick(a)
}

fn f64_decl(f f64, y f64) f64 {
	a, b := gm.pick(f), gm.pick(y)
	return gm.max2(a, b)
}

fn ptr_unrelated(x &u8) &u8 {
	p := gm.pick(x)
	return p
}

fn string_unrelated(f string) string {
	return f
}

fn main() {
	total := single_decl(i32(3), i32(5)) + multi_decl(i32(3), i32(5)) + condition_use(i32(3),
		i32(5)) + if_expr_use(i32(3), i32(5)) + local_homonym(i32(3))
	assert f64_decl(1.25, 2.5) == 2.5
	mut byte := u8(7)
	_ := ptr_unrelated(&byte)
	assert string_unrelated('ok') == 'ok'
	println(int_str(int(total)))
}
") or {
		panic(err)
	}
	return os.join_path(root, 'main.v')
}

fn test_generic_rhs_decl_types_specialize_later_generic_calls() {
	v3_bin := generic_rhs_build_v3()
	main_path := generic_rhs_write_project()
	out := os.join_path(os.temp_dir(), 'v3_generic_rhs_decl_type_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '-80'
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('gm__pick_T_i32'), generated
	assert generated.contains('gm__max2_T_i32'), generated
	assert generated.contains('gm__pick_T_f64'), generated
	assert generated.contains('gm__max2_T_f64'), generated
	assert !generated.contains('gm__pick('), generated
	assert !generated.contains('gm__max2('), generated
	assert !generated.contains('gm__max2_T_ptr_u8(&x)'), generated
	assert !generated.contains('gm__max2_T_string(f)'), generated
	assert !generated.contains('gm__max2_T_v_int(x)'), generated
	assert generated.contains('return pick(a);'), generated
}
