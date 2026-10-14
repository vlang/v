module c

import os
import v.cmdexec

fn run_global_symbol_project(name string, sources map[string]string) {
	root := os.join_path(os.vtmp_dir(), 'global_symbol_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for relative, source in sources {
		path := os.join_path(root, relative)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
	result := cmdexec.run(@VEXE, ['-b', 'c', '-cc', 'clang', '-enable-globals', 'run',
		os.join_path(root, 'main.v')])
	assert result.exit_code == 0, '${name}: ${result.output}'
}

fn test_nested_current_module_leaf_reads_the_stored_callback() {
	for with_same_leaf in [false, true] {
		mut sources := {
			'main.v':             'module main
import acme.local
fn main() { assert local.result() == 7 }
'
			'acme/local/local.v': 'module local
fn stored(value int) int { return value + 1 }
__global callback = stored
fn callback(value int) int { return value + 100 }
pub fn result() int {
	saved := local.callback
	return saved(6)
}
'
		}
		if with_same_leaf {
			sources['main.v'] = 'module main
import acme.local
import other.local as another
fn main() {
	assert local.result() == 7
	assert another.result() == 17
}
'
			sources['other/local/local.v'] = sources['acme/local/local.v'].replace('value + 1 }',
				'value + 11 }')
		}
		run_global_symbol_project('canonical_leaf_${with_same_leaf}', sources)
	}
}

fn test_imported_struct_defaults_read_live_global_storage() {
	run_global_symbol_project('struct_defaults', {
		'main.v':              'module main
import settings
fn main() {
	assert settings.Holder{}.value == 17
	settings.update(23)
	assert settings.Holder{}.value == 23
}
'
		'settings/settings.v': "module settings
struct State {
mut:
	value int
}
__global state = State{value: 17}
@[export: 'settings_state_function']
fn state() int { return 99 }
pub struct Holder {
pub:
	value int = state.value
}
pub fn update(value int) { state.value = value }
"
	})
}

fn test_global_storage_is_disjoint_from_source_suffixes_and_internal_names() {
	run_global_symbol_project('storage_names', {
		'main.v': 'module main
__global callback = 17
__global callback__v_global = 23
__global __v3_internal_symbol_global_callback = 31
fn callback(value int) int { return value * 2 }
fn main() {
	assert callback == 17
	assert callback__v_global == 23
	assert __v3_internal_symbol_global_callback == 31
	callback += 1
	assert callback == 18
	assert callback__v_global == 23
	assert __v3_internal_symbol_global_callback == 31
}
'
	})
}

fn test_global_and_function_with_the_same_name_compile_and_run() {
	root := os.join_path(os.vtmp_dir(), 'global_function_symbols_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for fixture, source in {
		'main':   'module main
__global callback = 17
fn callback(value int) int { return value * 2 }
fn main() {
	assert callback == 17
	callback += 2
	assert callback == 19
}
'
		'module': 'module main
import local
fn main() { assert local.result() == 23 }
'
		'extern': 'module main
import local as alias
fn main() {
	assert alias.counter == 17
	assert alias.result() == 17
}
'
	} {
		dir := os.join_path(root, fixture)
		os.mkdir_all(os.join_path(dir, 'local')) or { panic(err) }
		main_file := os.join_path(dir, 'main.v')
		os.write_file(main_file, source) or { panic(err) }
		if fixture == 'module' {
			os.write_file(os.join_path(dir, 'local', 'local.v'), 'module local
__global callback = 17
fn callback(value int) int { return value * 2 }
pub fn result() int { return callback + local.callback(3) }
') or { panic(err) }
		} else if fixture == 'extern' {
			header := os.join_path(dir, 'local', 'counter.h')
			os.write_file(header, 'long long counter = 17;\n') or { panic(err) }
			os.write_file(os.join_path(dir, 'local', 'local.v'), 'module local
#insert "${header}"
@[c_extern]
pub __global counter int
pub fn result() int { return local.counter }
') or { panic(err) }
		}
		result := cmdexec.run(@VEXE, ['-b', 'c', '-cc', 'clang', '-enable-globals', 'run', main_file])
		assert result.exit_code == 0, '${fixture}: ${result.output}'
	}
}
