import os

// These tests put the V3 module cache inside the project being built, the way
// VTMP-based tests and a project-local V3CACHE do. A warm cached header must be
// judged by the module it stands for, never as the project's own code.

struct SavedEnv {
	name    string
	value   string
	present bool
}

fn save_env(name string) SavedEnv {
	value := os.getenv_opt(name) or {
		return SavedEnv{
			name: name
		}
	}
	return SavedEnv{
		name:    name
		value:   value
		present: true
	}
}

fn (s SavedEnv) restore() {
	if s.present {
		os.setenv(s.name, s.value, true)
	} else {
		os.unsetenv(s.name)
	}
}

// pin_module_cache points the module cache at `dir`, whatever the runner exports:
// V3CACHE would move it elsewhere, and VFLAGS (e.g. `-nocache`) or
// V3_CACHE_FORCE_SOURCE would keep the cached headers from being written or read,
// hiding the regressions. It returns the previous values for restore().
fn pin_module_cache(dir string) []SavedEnv {
	saved := ['VTMP', 'V3CACHE', 'VFLAGS', 'V3_CACHE_FORCE_SOURCE', 'VMODULES'].map(save_env(it))
	os.setenv('VTMP', dir, true)
	os.setenv('V3CACHE', dir, true)
	os.unsetenv('VFLAGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	return saved
}

// The module cache is only used with the platform `cc`, not with tcc.
fn module_cache_is_usable() bool {
	$if windows {
		return false
	}
	os.find_abs_path_of_executable('cc') or { return false }
	return true
}

// build compiles `dir/name.v` to `dir/name.exe`. Every path is built from `dir`
// and a base name, so a `.v` inside VTMP is left alone.
fn build(dir string, name string, flags string) {
	src := os.quoted_path(os.join_path(dir, '${name}.v'))
	exe := os.quoted_path(os.join_path(dir, '${name}.exe'))
	res := os.execute('${os.quoted_path(@VEXE)} -cc cc ${flags} -o ${exe} ${src}')
	if res.exit_code != 0 {
		eprintln(res.output)
	}
	assert res.exit_code == 0
}

fn run_built(dir string, name string) string {
	res := os.execute(os.quoted_path(os.join_path(dir, '${name}.exe')))
	assert res.exit_code == 0
	return res.output.trim_space()
}

// A script (top-level statements, no `fn main`) written into the same directory as
// the module cache must still build once another program has warmed that cache:
// the cached `.vh` module headers are not part of the script's project.
fn test_script_builds_next_to_a_warm_module_cache() {
	if !module_cache_is_usable() {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_script_warm_module_cache_${os.getpid()}')
	os.mkdir_all(dir)!
	saved := pin_module_cache(dir)
	defer {
		for s in saved {
			s.restore()
		}
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'warm.v'), "fn main() {\n\tprintln('warm')\n}\n")!
	os.write_file(os.join_path(dir, 'script.v'), "println('after warm cache')\n")!
	build(dir, 'warm', '')
	build(dir, 'script', '')
	assert run_built(dir, 'script') == 'after warm cache'
}

// A warm header of an installed dependency embeds its generic bodies. With the
// cache inside the project, a local in such a body must not be reported as
// shadowing one of the project's globals: the dependency is not the project's.
fn test_cached_dependency_generic_body_does_not_shadow_project_globals() {
	if !module_cache_is_usable() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_cached_dependency_shadow_${os.getpid()}')
	app := os.join_path(root, 'app')
	vmodules := os.join_path(root, 'vmodules')
	os.mkdir_all(app)!
	os.mkdir_all(os.join_path(vmodules, 'mydep'))!
	saved := pin_module_cache(os.join_path(app, '.cache'))
	os.setenv('VMODULES', vmodules, true)
	defer {
		for s in saved {
			s.restore()
		}
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(vmodules, 'mydep', 'mydep.v'), 'module mydep

// pick returns the first item.
pub fn pick[T](items []T) T {
	x := items[0]
	return x
}
')!
	os.write_file(os.join_path(app, 'v.mod'), "Module {\n\tname: 'app'\n}\n")!
	os.write_file(os.join_path(app, 'warm.v'), "import mydep\n\nfn main() {\n\tprintln(mydep.pick(['a']))\n}\n")!
	os.write_file(os.join_path(app, 'main.v'), 'import mydep

__global x = 7

fn main() {
	println(mydep.pick([3, 4]) + x)
}
')!
	// `-enable-globals` is part of the cache configuration, so warm with it too.
	build(app, 'warm', '-enable-globals')
	if os.walk_ext(os.join_path(app, '.cache'), '.vh').filter(os.file_name(it).starts_with('mydep_')).len == 0 {
		eprintln('> skipping: this platform did not cache the `mydep` header')
		return
	}
	build(app, 'main', '-enable-globals')
	assert run_built(app, 'main') == '10'
}
