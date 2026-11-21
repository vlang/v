module fastc

import os
import v.cmdexec

// A grouped match arm (`A, B { ... }`) keeps a local boxed, so `x is A` inside it tests
// the sum value instead of the first variant's view.
fn test_fastc_grouped_match_arm_is_tests_boxed_local() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_grouped_match_arm_is_tests_boxed_local_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

struct ActionCsi {
	value string
}

struct ActionEsc {
	value string
}

struct ActionOther {
	n int
}

type Action = ActionCsi | ActionEsc | ActionOther

fn send(action Action) string {
	match action {
		ActionCsi, ActionEsc {
			prefix := if action is ActionCsi { '[' } else { '' }
			return prefix + action.value
		}
		ActionOther {
			return 'other \${action.n}'
		}
	}
}

fn main() {
	println(send(ActionCsi{'A'}))
	println(send(ActionEsc{'B'}))
	println(send(ActionOther{3}))
}

fn send_expr(action Action) string {
	return match action {
		ActionCsi, ActionEsc {
			if action is ActionCsi { 'csi ' + action.value } else { 'esc' }
		}
		ActionOther {
			'other'
		}
	}
}

fn init() {
	println(send_expr(ActionCsi{'C'}))
	println(send_expr(ActionEsc{'D'}))
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
	assert run.output.trim_space() == 'csi C\nesc\n[A\nB\nother 3', run.output
}
