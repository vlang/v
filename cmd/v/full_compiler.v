module main

import os

// full_compiler_is_required checks the selected target, not backends being compiled into V.
fn full_compiler_is_required(args []string) bool {
	match selected_backend(args, false) {
		'fastc' {
			$if skip_fastc ? {
				return true
			}
		}
		'arm64' {
			$if skip_arm64 ? {
				return true
			}
		}
		'amd64', 'x64', 'x86_64', 'x86-64' {
			$if skip_amd64 ?|| skip_x64 ? {
				return true
			}
		}
		'wasm' {
			$if skip_wasm ? {
				return true
			}
		}
		'eval' {
			$if skip_eval ? {
				return true
			}
		}
		else {}
	}
	return false
}

// full_compiler_build_args returns the options that build the full compiler a `-b fastc` (or
// other optional backend) compilation is handed to. A `-prod` compiler builds an optimized one:
// otherwise `vprod -b fastc` would generate C with an unoptimized helper.
fn full_compiler_build_args(ownership bool, is_prod bool) []string {
	mut build_args := ['-all-backends', '-gc', 'none', '-prealloc', '-nocache']
	if is_prod {
		build_args << '-prod'
	}
	// Keep both capabilities together instead of bouncing between helper compilers.
	if ownership {
		build_args << ['-d', 'ownership']
	}
	return build_args
}

// launch_full_compiler builds all optional backends once, then forwards the original request.
@[noreturn]
fn launch_full_compiler(args []string) {
	vexe := os.real_path(os.executable())
	vroot := find_vroot(vexe) or {
		find_vroot(@VEXEROOT) or {
			eprintln('the V source tree could not be found')
			exit(1)
		}
	}
	mut is_prod := false
	$if prod {
		is_prod = true
	}
	build_args := full_compiler_build_args(ownership_checker_is_compiled()
		|| ownership_compiler_is_required(compiler_args_before_program(args)), is_prod)
	// The child merges VFLAGS itself. Forwarding the already merged arguments would
	// apply options such as -cflags twice and change the requested compilation.
	launch_optional_compiler(vexe, vroot, 'v3_full', 'full V3', build_args, os.args[1..])
}
