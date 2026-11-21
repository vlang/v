import os

const openmod_enum_src = 'module openmod

@[non_exhaustive]
pub enum State {
	ready
	done
}
'

const openmod_sum_src = 'module openmod

pub struct Start {
}

pub struct Stop {
}

@[non_exhaustive]
pub type Event = Start | Stop
'

fn write_generic_non_exhaustive_project(root string, mod_src string, main_src string) string {
	os.mkdir_all(os.join_path(root, 'openmod')) or { panic(err) }
	os.write_file(os.join_path(root, 'openmod', 'openmod.v'), mod_src) or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, main_src) or { panic(err) }
	return main_path
}

fn test_generic_non_exhaustive_enum_requires_else() {
	root := os.join_path(os.vtmp_dir(), 'non_exhaustive_generic_enum_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := write_generic_non_exhaustive_project(root, openmod_enum_src, 'module main

import openmod

fn value[T](s T) int {
	return match s {
		openmod.State.ready {
			1
		}
		openmod.State.done {
			2
		}
	}
}

fn main() {
	println(value(openmod.State.ready))
}
')
	result := os.exec([@VEXE, '-check', main_path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('match on non-exhaustive `State` from module `openmod` must include `else {}`'), result.output
}

fn test_generic_non_exhaustive_sum_requires_else() {
	root := os.join_path(os.vtmp_dir(), 'non_exhaustive_generic_sum_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := write_generic_non_exhaustive_project(root, openmod_sum_src, 'module main

import openmod

fn value[T](s T) int {
	return match s {
		openmod.Start {
			1
		}
		openmod.Stop {
			2
		}
	}
}

fn main() {
	println(value(openmod.Event(openmod.Start{})))
}
')
	result := os.exec([@VEXE, '-check', main_path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('match on non-exhaustive `Event` from module `openmod` must include `else {}`'), result.output
}
