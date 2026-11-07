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
		'x64', 'amd64', 'x86_64', 'x86-64' {
			$if skip_x64 ? {
				return true
			}
		}
		'riscv64', 'riscv', 'rv64' {
			$if skip_riscv64 ? {
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
	mut build_args := ['-all-backends', '-gc', 'none', '-prealloc', '-nocache']
	// Keep both capabilities together instead of bouncing between helper compilers.
	if ownership_checker_is_compiled() || ownership_compiler_is_required(compiler_args_before_program(args)) {
		build_args << ['-d', 'ownership']
	}
	// The child merges VFLAGS itself. Forwarding the already merged arguments would
	// apply options such as -cflags twice and change the requested compilation.
	launch_optional_compiler(vexe, vroot, 'v3_full', 'full V3', build_args, os.args[1..])
}
