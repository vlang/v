// vtest build: !windows
module builder

import os
import runtime
import v2.parser
import v2.pref
import v2.ssa
import v2.token
import v2.transformer
import v2.types

fn parallel_ssa_alias_tmp_dir(label string) string {
	return os.join_path(os.temp_dir(), 'v2_parallel_ssa_alias_${label}_${os.getpid()}')
}

struct ParallelSSAAliasTestSource {
	rel_path string
	code     string
}

fn parallel_ssa_module_for_test_sources(label string, sources []ParallelSSAAliasTestSource) &ssa.Module {
	tmp_dir := parallel_ssa_alias_tmp_dir(label)
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{}
	for source in sources {
		path := os.join_path(tmp_dir, source.rel_path)
		os.mkdir_all(os.dir(path)) or { panic('cannot create ${os.dir(path)}') }
		os.write_file(path, source.code) or { panic('cannot write ${path}') }
		paths << path
	}
	prefs := &pref.Preferences{
		backend: .x64
		arch:    .x64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut mod := ssa.Module.new('parallel_alias_test')
	mut ssa_builder := ssa.Builder.new_with_env(mod, env)
	ssa_builder.skip_fn_bodies = true
	ssa_builder.build_all(transformed_files)
	ssa_builder.skip_fn_bodies = false

	old_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '2', true)
	defer {
		if old := old_vjobs {
			os.setenv('VJOBS', old, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	assert runtime.nr_jobs() == 2
	mut outer_builder := new_builder(prefs)
	outer_builder.ssa_build_parallel(mut ssa_builder, transformed_files)
	return mod
}

fn parallel_ssa_func_names(m &ssa.Module) []string {
	mut names := []string{}
	for func in m.funcs {
		names << func.name
	}
	return names
}

fn test_parallel_ssa_worker_restores_module_import_aliases() {
	m := parallel_ssa_module_for_test_sources('module_alias', [
		ParallelSSAAliasTestSource{
			rel_path: 'left/left.v'
			code:     'module left

pub struct Box {
pub:
	value int
}
'
		},
		ParallelSSAAliasTestSource{
			rel_path: 'right/right.v'
			code:     'module right

pub struct Box {
pub:
	value int
}
'
		},
		ParallelSSAAliasTestSource{
			rel_path: 'a.v'
			code:     'module main

import left as dep

fn bump_left() int {
	box := dep.Box{
		value: 1
	}
	return keep[dep.Box](box).value
}
'
		},
		ParallelSSAAliasTestSource{
			rel_path: 'b.v'
			code:     'module main

import right as dep

fn bump_right() int {
	box := dep.Box{
		value: 2
	}
	return keep[dep.Box](box).value
}
'
		},
		ParallelSSAAliasTestSource{
			rel_path: 'main.v'
			code:     'module main

fn keep[T](value T) T {
	return value
}

fn filler0() int { return 0 }
fn filler1() int { return 1 }
fn filler2() int { return 2 }
fn filler3() int { return 3 }

fn main() {
	_ = bump_left()
	_ = bump_right()
}
'
		},
	])

	func_names := parallel_ssa_func_names(m)
	assert 'keep_T_left_Box' in func_names, func_names.str()
	assert 'keep_T_right_Box' in func_names, func_names.str()
	assert 'keep_T_dep_Box' !in func_names, func_names.str()
}
