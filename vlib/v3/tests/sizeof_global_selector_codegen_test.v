import os

const sizeof_global_vexe = @VEXE
const sizeof_global_tests_dir = os.dir(@FILE)
const sizeof_global_v3_dir = os.dir(sizeof_global_tests_dir)
const sizeof_global_vlib_dir = os.dir(sizeof_global_v3_dir)
const sizeof_global_v3_src = os.join_path(sizeof_global_v3_dir, 'v3.v')

fn sizeof_global_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_sizeof_global_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${sizeof_global_vexe} -gc none -path "${sizeof_global_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${sizeof_global_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn sizeof_global_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn sizeof_global_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_sizeof_global_${os.getpid()}')
	os.rmdir_all(root) or {}
	sizeof_global_write_file(root, 'moda/moda.v', 'module moda

pub struct State {
pub mut:
	event int
}

__global global_sizeof_state State

pub fn global_field_size() int {
	return int(sizeof(global_sizeof_state.event))
}

pub fn local_field_size() int {
	global_sizeof_state := State{
		event: 3
	}
	return int(sizeof(global_sizeof_state.event))
}

pub fn type_size() int {
	return int(sizeof(State))
}
')
	sizeof_global_write_file(root, 'main.v', 'module main

import moda

struct State {
	event int
}

__global global_sizeof_state State

fn main_size() int {
	return int(sizeof(global_sizeof_state.event))
}

fn main_type_size() int {
	return int(sizeof(State))
}

fn main() {
	mut score := main_size()
	score += main_type_size()
	score += moda.global_field_size()
	score += moda.local_field_size()
	score += moda.type_size()
	score += int(sizeof(moda.State))
	println(int_str(score))
}
')
	return root
}

fn test_sizeof_selector_qualifies_global_without_rewriting_locals_or_types() {
	v3_bin := sizeof_global_build_v3()
	root := sizeof_global_write_project()
	c_path := os.join_path(root, 'out.c')
	compile := os.execute('${v3_bin} ${root} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(c_path) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('intmain_size(void){return(int)(sizeof(global_sizeof_state.event));}'), c_code

	assert compact.contains('sizeof(moda__global_sizeof_state.event)'), c_code
	assert compact.contains('intmoda__global_field_size(void){return(int)(sizeof(moda__global_sizeof_state.event));}'), c_code

	assert compact.contains('sizeof(global_sizeof_state.event)'), c_code
	assert compact.contains('moda__Stateglobal_sizeof_state=(moda__State){.event=3};return(int)(sizeof(global_sizeof_state.event));'), c_code

	assert compact.count('sizeof(global_sizeof_state.event)') == 2, c_code
	assert compact.contains('sizeof(State)'), c_code
	assert compact.contains('sizeof(moda__State)'), c_code
	assert !compact.contains('sizeof(moda.State)'), c_code
	assert !compact.contains('global_sizeof_state__event'), c_code
}
