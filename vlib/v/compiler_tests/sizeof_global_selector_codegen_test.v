import os
import v.tempname

fn sizeof_global_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn sizeof_global_write_project(root string) {
	sizeof_global_write_file(root, 'moda/moda.v', 'module moda

pub struct State {
pub mut:
	event int
}

__global global_sizeof_state State

pub fn global_field_size() int {
	return int(sizeof(global_sizeof_state.event))
}

// The local was named `global_sizeof_state` too, so this pinned down that
// `sizeof` followed the local rather than the global of that name. Reusing the
// name of a global is an error now; the two selectors still have to be told
// apart, which is what the differing names leave in place.
pub fn local_field_size() int {
	local_sizeof_state := State{
		event: 3
	}
	return int(sizeof(local_sizeof_state.event))
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
}

fn test_sizeof_selector_qualifies_global_without_rewriting_locals_or_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_sizeof_global_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	sizeof_global_write_project(root)
	c_path := os.join_path(root, 'out.c')
	compile := os.exec([@VEXE, '-enable-globals', root, '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, compile.output

	c_code := os.read_file(c_path) or { panic(err) }
	compact := c_code.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('i64main_size(void){return(i64)(sizeof(global_sizeof_state.event));}'), c_code

	assert compact.contains('sizeof(moda__global_sizeof_state.event)'), c_code
	assert compact.contains('i64moda__global_field_size(void){return(i64)(sizeof(moda__global_sizeof_state.event));}'), c_code

	assert compact.contains('sizeof(local_sizeof_state.event)'), c_code
	assert compact.contains('moda__Statelocal_sizeof_state=(moda__State){.event=3};return(i64)(sizeof(local_sizeof_state.event));'), c_code

	assert compact.count('sizeof(local_sizeof_state.event)') == 1, c_code
	assert compact.contains('sizeof(main__State)'), c_code
	assert compact.contains('sizeof(moda__State)'), c_code
	assert !compact.contains('sizeof(moda.State)'), c_code
	assert !compact.contains('global_sizeof_state__event'), c_code
}
