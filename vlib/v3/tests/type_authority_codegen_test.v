import os

const type_authority_vexe = @VEXE
const type_authority_tests_dir = os.dir(@FILE)
const type_authority_v3_dir = os.dir(type_authority_tests_dir)
const type_authority_vlib_dir = os.dir(type_authority_v3_dir)
const type_authority_v3_src = os.join_path(type_authority_v3_dir, 'v3.v')

fn type_authority_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_type_authority_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${type_authority_vexe} -gc none -path "${type_authority_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${type_authority_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn type_authority_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn type_authority_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_type_authority_${os.getpid()}')
	os.rmdir_all(root) or {}
	type_authority_write_file(root, 'dep/dep.v', 'module dep

pub struct Color {
pub:
	r int
}

pub const white = Color{
	r: 9
}

pub struct TextCfg {
pub:
	color Color
}

pub fn choose(flag bool, fallback Color) TextCfg {
	return TextCfg{
		color: if flag { white } else { fallback }
	}
}
')
	type_authority_write_file(root, 'fixture/fixture.v', 'module fixture

pub struct Thing {
pub:
	n int
}

pub const vals = [
	Thing{
		n: 11
	},
	Thing{
		n: 12
	},
	Thing{
		n: 13
	},
]

pub fn pointer_score(idx int) int {
	first := Thing{
		n: 3
	}
	second := Thing{
		n: 4
	}
	third := Thing{
		n: 5
	}
	mut ptrs := [3]&Thing{}
	ptrs[0] = &first
	ptrs[1] = &second
	ptrs[2] = &third
	item := ptrs[idx]
	return item.n
}

pub fn value_score(idx int) int {
	item := vals[idx]
	return item.n
}

pub fn local_fixed_score() int {
	mut fixed := [3]Thing{}
	fixed[1] = Thing{
		n: 17
	}
	return fixed[1].n
}
')
	type_authority_write_file(root, 'main.v', 'module main

import dep
import fixture

struct Color {
	wrong int
}

fn main() {
	picked := dep.choose(false, dep.Color{
		r: 5
	})
	score := picked.color.r + fixture.pointer_score(2) + fixture.value_score(1) + fixture.local_fixed_score() +
		sizeof(Color) - sizeof(Color)
	println(int_str(score))
}
')
	return root
}

fn test_imported_type_authority_for_if_expr_and_fixed_array_index() {
	v3_bin := type_authority_build_v3()
	root := type_authority_write_project()
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '39'

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('dep__Color __if_val_'), generated
	assert !generated.contains('\n\tColor __if_val_'), generated
	assert !generated.contains('\nColor __if_val_'), generated
	assert !generated.contains('main__Color __if_val_'), generated

	assert generated.contains('fixture__Thing* ptrs[3] = {0};'), generated
	assert generated.contains('fixture__Thing* item = ptrs[idx];'), generated
	assert generated.contains('fixture__Thing item = fixture__vals[idx];'), generated
	assert !generated.contains('Array_fixed_fixture__Thingptr_3 item'), generated
	assert !generated.contains('Array_fixed_fixture__Thing_3 item'), generated
	assert generated.contains('fixture__Thing fixed[3] = {0};'), generated
}
