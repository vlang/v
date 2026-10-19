// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os

// Regression tests for https://github.com/vlang/v/issues/30632: with `/` writable by everybody,
// `~/.cache` writable by its group and `/tmp` owned by another user, no default tool cache folder
// was usable. A folder above everything that the cache and the V executable have in common is
// tolerated now, because whoever can change it can replace V itself, and `~/.vtools` is tried last.

// HostLikeTree mimics that host below a private test folder: `world` stands for `/`, and holds
// the home folder, with V installed in it, and the temporary folder.
struct HostLikeTree {
	root     string // private, the folders above it belong to the test host
	world    string // mode 0777, like `/` on that host
	home     string // mode 0700
	cache    string // `~/.cache`, mode 0775
	tmp      string // `/tmp`, mode 0700
	compiler string // the V executable, `~/v/v`
}

fn new_host_like_tree(name string) HostLikeTree {
	root := os.join_path(os.vtmp_dir(), 'v_toolcache_common_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.chmod(root, 0o700) or { panic(err) }
	real_root := os.real_path(root)
	world := os.join_path(real_root, 'world')
	home := os.join_path(world, 'home')
	tree := HostLikeTree{
		root:     real_root
		world:    world
		home:     home
		cache:    os.join_path(home, '.cache')
		tmp:      os.join_path(world, 'tmp')
		compiler: os.join_path(home, 'v', 'v')
	}
	for folder, mode in {
		world:                 0o777
		home:                  0o700
		tree.cache:            0o775
		tree.tmp:              0o700
		os.dir(tree.compiler): 0o755
	} {
		os.mkdir(folder) or { panic(err) }
		os.chmod(folder, mode) or { panic(err) }
	}
	os.write_file(tree.compiler, '') or { panic(err) }
	os.chmod(tree.compiler, 0o755) or { panic(err) }
	return tree
}

fn (tree HostLikeTree) remove() {
	os.chmod(tree.world, 0o700) or {}
	os.rmdir_all(tree.root) or {}
}

// with_tool_cache_variables sets environment variables while `body` runs. An empty value unsets
// a variable.
fn with_tool_cache_variables(values map[string]string, body fn () !) ! {
	mut previous := map[string]?string{}
	for name, _ in values {
		previous[name] = os.getenv_opt(name)
	}
	defer {
		for name, value in previous {
			if v := value {
				os.setenv(name, v, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	for name, value in values {
		if value == '' {
			os.unsetenv(name)
		} else {
			os.setenv(name, value, true)
		}
	}
	body()!
}

fn test_only_folders_above_what_the_cache_shares_with_v_are_tolerated() {
	$if !windows {
		tree := new_host_like_tree('rule')
		defer {
			tree.remove()
		}
		if !tool_cache_parents_are_trusted(tree.root) {
			eprintln('> skipping, the folders above `${tree.root}` are not private')
			return
		}
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE': tree.compiler
		}, fn [tree] () ! {
			assert tool_cache_folder_common_with_compiler(tree.home) == tree.home
			assert tool_cache_folder_common_with_compiler(tree.tmp) == tree.world
			// `world` leads to V too: whoever can change it can replace V itself.
			assert tool_cache_parents_are_trusted(tree.home)
			// The temporary folder and V are reached through different entries of `world`, so
			// it could be swapped alone.
			assert !tool_cache_parents_are_trusted(tree.tmp)
			// Below the common folder, nothing is tolerated.
			assert !tool_cache_parents_are_trusted(os.join_path(tree.cache, 'v'))
			// Nor is the common folder itself: the cache and V are in different entries of it.
			os.chmod(tree.home, 0o775)!
			assert !tool_cache_parents_are_trusted(tree.home)
			os.chmod(tree.home, 0o700)!
		})!
		// With V installed somewhere else, `world` is checked like any other folder.
		other := os.join_path(tree.root, 'opt', 'v')
		os.mkdir_all(os.dir(other))!
		os.write_file(other, '')!
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE': other
		}, fn [tree] () ! {
			assert tool_cache_folder_common_with_compiler(tree.home) == tree.root
			assert !tool_cache_parents_are_trusted(tree.home)
		})!
	}
}

fn test_an_unknown_v_executable_tolerates_nothing() {
	$if !windows {
		tree := new_host_like_tree('unknown')
		defer {
			tree.remove()
		}
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE': os.join_path(tree.home, 'v', 'missing')
		}, fn [tree] () ! {
			assert tool_cache_compiler_executable() == ''
			assert tool_cache_folder_common_with_compiler(tree.home) == ''
			assert !tool_cache_parents_are_trusted(tree.home)
		})!
	}
}

fn test_the_home_folder_candidate_is_used_when_the_other_defaults_are_not_safe() {
	$if !windows {
		tree := new_host_like_tree('home_candidate')
		defer {
			tree.remove()
		}
		if !tool_cache_parents_are_trusted(tree.root) {
			eprintln('> skipping, the folders above `${tree.root}` are not private')
			return
		}
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE':      tree.compiler
			'HOME':                 tree.home
			'XDG_CACHE_HOME':       tree.cache
			'VTMP':                 tree.tmp
			tool_cache_dir_env:     ''
			tool_cache_disable_env: ''
			tool_cache_verbose_env: ''
		}, fn [tree] () ! {
			home_cache := os.join_path(tree.home, '.vtools')
			assert tool_cache_home_candidate() == home_cache
			chosen := tool_cache_dir() or { panic('no usable tool cache folder') }
			assert chosen == home_cache
			assert os.stat(chosen)!.mode & 0o777 == 0o700
			// Neither `~/.cache` nor the home folder are changed, and nothing is created below the
			// temporary folder, which could be swapped.
			assert os.stat(tree.cache)!.mode & 0o777 == 0o775
			assert os.stat(tree.home)!.mode & 0o777 == 0o700
			assert !os.exists(os.join_path(tree.tmp, 'tools'))
			// A home folder that its group can write to is not used either.
			os.rmdir(home_cache)!
			os.chmod(tree.home, 0o775)!
			if unsafe_cache := tool_cache_dir() {
				assert false, 'the cache `${unsafe_cache}` is below a folder that others can change'
			}
			os.chmod(tree.home, 0o700)!
		})!
	}
}

fn test_an_explicit_home_folder_cache_is_not_treated_as_a_default_one() {
	$if !windows {
		tree := new_host_like_tree('explicit_home')
		defer {
			tree.remove()
		}
		if !tool_cache_parents_are_trusted(tree.root) {
			eprintln('> skipping, the folders above `${tree.root}` are not private')
			return
		}
		home_cache := os.join_path(tree.home, '.vtools')
		// With V installed outside the home folder, `world` is checked for the default candidates,
		// but VTOOLS_CACHE_DIR is the user's own choice, and the folders above it are not checked.
		other := os.join_path(tree.root, 'opt', 'v')
		os.mkdir_all(os.dir(other))!
		os.write_file(other, '')!
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE':      other
			'HOME':                 tree.home
			'XDG_CACHE_HOME':       tree.cache
			'VTMP':                 tree.tmp
			tool_cache_dir_env:     home_cache
			tool_cache_disable_env: ''
			tool_cache_verbose_env: ''
		}, fn [home_cache] () ! {
			chosen := tool_cache_dir() or { panic('the explicit `${home_cache}` was not used') }
			assert chosen == home_cache
		})!
		os.rmdir(home_cache)!
		// A `~/.vtools` set there that is a symlink to a private folder is used, not moved aside.
		os.chmod(tree.world, 0o755)!
		elsewhere := os.join_path(tree.root, 'elsewhere')
		os.mkdir(elsewhere, mode: 0o700)!
		os.symlink(elsewhere, home_cache)!
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE':      tree.compiler
			'HOME':                 tree.home
			'XDG_CACHE_HOME':       tree.cache
			'VTMP':                 tree.tmp
			tool_cache_dir_env:     home_cache
			tool_cache_disable_env: ''
			tool_cache_verbose_env: ''
		}, fn [tree, home_cache, elsewhere] () ! {
			chosen := tool_cache_dir() or { panic('the explicit `${home_cache}` was not used') }
			assert chosen == elsewhere
			assert os.is_link(home_cache)
			assert os.ls(tree.home)!.filter(it.starts_with('.vtools.')) == []
		})!
	}
}

// rejected_tool_cache_problems maps each folder that the error of `tool_cache_dir` lists to the
// problems listed for it.
fn rejected_tool_cache_problems(message string) map[string][]string {
	mut problems := map[string][]string{}
	mut candidate := ''
	for line in message.split_into_lines() {
		if line.starts_with('  - `') && line.ends_with('`:') {
			candidate = line.all_after('`').all_before_last('`')
			problems[candidate] = []string{}
		} else if candidate != '' && line.starts_with('      ') {
			problems[candidate] << line.trim_space()
		} else {
			candidate = ''
		}
	}
	return problems
}

fn test_a_tolerated_folder_is_not_reported_as_a_problem() {
	$if !windows {
		tree := new_host_like_tree('problems')
		defer {
			tree.remove()
		}
		if !tool_cache_parents_are_trusted(tree.root) {
			eprintln('> skipping, the folders above `${tree.root}` are not private')
			return
		}
		os.chmod(tree.home, 0o775) or { panic(err) }
		with_tool_cache_variables({
			'V3_PRIMARY_VEXE':      tree.compiler
			'HOME':                 tree.home
			'XDG_CACHE_HOME':       tree.cache
			'VTMP':                 tree.tmp
			tool_cache_dir_env:     ''
			tool_cache_disable_env: ''
			tool_cache_verbose_env: ''
		}, fn [tree] () ! {
			home_problem := '`${tree.home}` (mode 0775) is writable by its group without the sticky bit'
			world_problem := '`${tree.world}` (mode 0777) is writable by its group and others without the sticky bit'
			cache_problem := '`${tree.cache}` (mode 0775) is writable by its group without the sticky bit'
			// Only the home folder has to be fixed for a cache in it: `world` leads to V too.
			assert tool_cache_parent_problems(tree.home).filter(it.starts_with('`${tree.root}')) == [
				home_problem,
			]
			// The temporary folder and V are reached through different entries of `world`, so
			// there `world` has to be fixed.
			assert tool_cache_parent_problems(tree.tmp).filter(it.starts_with('`${tree.root}')) == [
				world_problem,
			]
			if chosen := tool_cache_dir() {
				assert false, 'the cache `${chosen}` is below a folder that others can change'
			} else {
				problems := rejected_tool_cache_problems(err.msg())
				assert problems == {
					os.join_path(tree.cache, 'v', 'tools'): [cache_problem, home_problem]
					os.join_path(tree.tmp, 'tools'):        [world_problem]
					os.join_path(tree.home, '.vtools'):     [home_problem]
					os.join_path(tree.home, '.v', 'tools'): [home_problem]
				}, err.msg()
			}
			// Fixing the home folder alone is enough, while `world` stays writable by everybody.
			os.chmod(tree.home, 0o700)!
			chosen := tool_cache_dir() or { panic(err) }
			assert chosen == os.join_path(tree.home, '.vtools')
		})!
	}
}
