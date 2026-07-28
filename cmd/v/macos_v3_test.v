module main

import os
import v.pref

fn test_macos_v3_relevant_command_only_selects_supported_native_c_builds() {
	$if macos {
		mut prefs := &pref.Preferences{
			path:      'main.v'
			backend:   .c
			os:        .macos
			is_script: true
			gc_mode:   .no_gc
		}
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.coverage_dir = '/tmp/vcovdir'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.coverage_dir = ''
		prefs.show_cc = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.output_mode = .silent
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.output_mode = .stdout
		prefs.show_cc = false
		prefs.is_o = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_o = false
		prefs.is_vlines = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = false
		prefs.gc_mode = .boehm_full_opt
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.gc_mode = .no_gc
		prefs.is_run = true
		assert is_macos_v3_relevant_command('run', prefs)
		prefs.is_shared = true
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.is_run = false
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.path = 'script.vsh'
		prefs.is_crun = true
		assert !is_macos_v3_relevant_command('script.vsh', prefs)
		prefs.is_crun = false
		prefs.is_shared = false
		prefs.is_run = true
		prefs.path = 'main.v'
		prefs.os = .linux
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.os = .macos
		prefs.path = 'cmd/v'
		assert !is_macos_v3_relevant_command('cmd/v', prefs)
		prefs.path = 'cmd/tools/vfmt.v'
		assert !is_macos_v3_relevant_command('cmd/tools/vfmt.v', prefs)
		prefs.path = 'main.v'
		prefs.is_cstrict = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_cstrict = false
		prefs.is_test = true
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.is_test = false
		prefs.autofree = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.autofree = false
		prefs.is_prod = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prod = false
		prefs.path = 'version'
		assert !is_macos_v3_relevant_command('version', prefs)
		prefs.path = 'vlib/v3'
		assert !is_macos_v3_relevant_command('vlib/v3', prefs)
		prefs.path = 'fixture.vv'
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.path = 'script.vsh'
		prefs.is_crun = true
		assert is_macos_v3_relevant_command('script.vsh', prefs)
		assert !is_macos_v3_relevant_command('crun', prefs)
		prefs.is_crun = false
		prefs.path = 'main.v'
		prefs.out_name_is_dir = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.out_name_is_dir = false
		for path in ['foo.c.v', 'foo.js.v', 'foo.wasm.v', '.v'] {
			prefs.path = path
			assert !is_macos_v3_relevant_command(path, prefs)
		}
		root := os.join_path(os.vtmp_dir(), 'macos_v3_symlink_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'source.v')
		alias := os.join_path(root, 'alias.v')
		os.write_file(source, 'fn main() {}\n') or { panic(err) }
		os.symlink(source, alias) or { panic(err) }
		prefs.path = alias
		assert !is_macos_v3_relevant_command(alias, prefs)
	}
}

fn test_macos_v3_environment_flags_require_compatibility_compiler() {
	$if macos {
		assert macos_v3_environment_flags_are_supported('', '')
		assert !macos_v3_environment_flags_are_supported('-DMACOS_V3_CFLAGS', '')
		assert !macos_v3_environment_flags_are_supported('', '-framework Cocoa')
	}
}

fn test_macos_v3_forwards_environment_driven_skip_running() {
	$if macos {
		prefs := &pref.Preferences{
			skip_running: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['script.vsh'])
		assert '-skip-running' in forwarded
		assert forwarded.count(it == '-skip-running') == 1
		already_explicit := macos_v3_forwarded_args(prefs, ['-skip-running', 'script.vsh'])
		assert already_explicit.count(it == '-skip-running') == 1
	}
}

fn test_macos_v3_forwards_showcc_with_quiet_benchmarks() {
	$if macos {
		prefs := &pref.Preferences{
			show_cc: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['-showcc', 'main.v'])
		assert '-silent' in forwarded
		assert '-showcc' in forwarded
	}
}

fn test_macos_v3_args_only_accept_options_implemented_by_v3() {
	$if macos {
		assert macos_v3_args_are_supported(['main.v'])
		assert macos_v3_args_are_supported(['-keepc', '-o', 'main', 'build', 'main.v'])
		assert macos_v3_args_are_supported(['run', 'main.v', '--program-option'])
		assert macos_v3_args_are_supported(['script.vsh', '--script-option'])
		assert !macos_v3_args_are_supported(['-ldflags', '-framework Cocoa', 'main.v'])
		assert !macos_v3_args_are_supported(['-path', '@vlib', 'main.v'])
		assert !macos_v3_args_are_supported(['-show-c-output', 'main.v'])
		assert !macos_v3_args_are_supported(['-output', 'main', 'main.v'])
		assert !macos_v3_args_are_supported(['-o', '-', 'main.v'])
		assert !macos_v3_args_are_supported(['-o', '-foo', 'main.v'])
		assert !macos_v3_args_are_supported(['-silent', 'main.v'])
		assert !macos_v3_args_are_supported(['-w', 'main.v'])
		for help_flag in ['-?', '-h', '-help', '--help'] {
			assert !macos_v3_args_are_supported(['-gc', 'none', help_flag, 'main.v'])
		}
	}
}

fn test_macos_v3_bootstrap_clears_argument_environment() {
	$if macos {
		environment := macos_v3_bootstrap_environment()
		assert environment[macos_v3_bootstrap_env] == '1'
		assert environment['VFLAGS'] == ''
		assert environment['VOSARGS'] == ''
	}
}

fn test_macos_v3_default_executable_excludes_temporary_self_hosted_compilers() {
	$if macos {
		assert is_macos_v3_default_executable('/tmp/v')
		assert is_macos_v3_default_executable('/tmp/vnew')
		assert !is_macos_v3_default_executable('/tmp/v2')
		assert !is_macos_v3_default_executable('/tmp/vstrict1')
		assert !is_macos_v3_default_executable('/tmp/vp')
	}
}
