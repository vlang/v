import os

// `v -cc clang run` on macOS links the program unit with a dylib of the cached
// module objects. The unit was compiled with `-D__TINYC__`, which pads an empty
// struct such as `Error` to a byte, while the dylib was built without it: every
// field of a struct that embeds an empty struct was read at another offset on the
// other side (`shed none_ 123` instead of `shed pipeline_full 123`).

const shed_module = 'module shed

pub enum Reason {
	none_
	pipeline_full
}

pub struct Shed {
	Error
pub:
	reason Reason
	n      int
}

pub fn (e Shed) msg() string {
	return "shed \${e.reason} \${e.n}"
}

pub fn make() IError {
	return Shed{
		reason: .pipeline_full
		n:      7
	}
}

pub struct Tag {}

pub struct Tagged {
	Tag
pub:
	id int
}

pub fn (t Tagged) text() string {
	return "tagged \${t.id}"
}
'

const main_program = 'import shed

fn fail() ! {
	return shed.Shed{
		reason: .pipeline_full
		n:      123
	}
}

fn main() {
	fail() or { println(err.msg()) }
	e := shed.make()
	if e is shed.Shed {
		println("\${e.reason} \${e.n}")
	}
	println(shed.Tagged{ id: 5 }.text())
}
'

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

fn test_dev_run_with_clang_keeps_the_struct_layout_of_the_cached_modules() {
	$if !macos {
		return
	}
	os.find_abs_path_of_executable('clang') or {
		eprintln('skipping: no clang')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_dev_run_dylib_layout_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'shed'))!
	saved := ['VTMP', 'V3CACHE', 'VFLAGS', 'CFLAGS', 'LDFLAGS'].map(save_env(it))
	defer {
		for env in saved {
			env.restore()
		}
		os.rmdir_all(root) or {}
	}
	cache := os.join_path(root, 'cache')
	os.setenv('VTMP', cache, true)
	os.setenv('V3CACHE', cache, true)
	os.unsetenv('VFLAGS')
	os.unsetenv('CFLAGS')
	os.unsetenv('LDFLAGS')
	os.write_file(os.join_path(root, 'v.mod'), '')!
	os.write_file(os.join_path(root, 'shed', 'shed.v'), shed_module)!
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, main_program)!
	// The first run builds the module objects and the dylib, the second one links
	// the cached dylib.
	for _ in 0 .. 2 {
		res := os.exec([@VEXE, '-cc', 'clang', '-showcc', 'run', main_file])
		assert res.exit_code == 0, res.output
		lines := res.output.split_into_lines()
		commands := lines.filter(it.starts_with('  > '))
		assert commands.any(it.contains('dev_modules_')), 'the run did not link a dev dylib:\n${res.output}'
		assert lines.filter(!it.starts_with('  > ')) == ['shed pipeline_full 123', 'pipeline_full 7',
			'tagged 5'], res.output
	}
}
