module fastc

import os
import v.cmdexec

// A `match` branch whose value is a `@[noreturn]` call (`.bad { unreachable() }`) gets a zero
// value of the branch type after the call, so the conditional type-checks.
fn test_fastc_noreturn_call_as_match_branch_value() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_noreturn_call_as_match_branch_value_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

enum Kind {
	bad
	gone
	other
}

struct Fail {
	Error
	kind Kind
}

fn (f Fail) msg() string {
	return f.kind.str()
}

@[noreturn]
fn unreachable() {
	panic('reached unreachable code')
}

fn kind_of(err IError) Kind {
	if err is Fail {
		return err.kind
	}
	return .other
}

fn sendto(n int) !u32 {
	if n == 0 {
		return Fail{
			kind: .other
		}
	}
	return u32(n)
}

fn send(n int) !u32 {
	return sendto(n) or {
		match kind_of(err) {
			.bad, .gone {
				unreachable()
			}
			else {
				return err
			}
		}
	}
}

fn main() {
	println(send(3) or { 0 })
	println(send(0) or { 99 })
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3\n99', run.output
}
