module fastc

import os
import v.cmdexec

fn test_fastc_if_expression_smartcast_subjects_are_evaluated_lazily() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_lazy_sum_subjects_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Record { count int }
type Item = Record | int
__global calls int
fn choose() int { calls++; return 0 }
fn guarded(items []Item, enabled bool) int {
	return if enabled && items.len > 0 && items[items.len - 1] is Record { items[items.len - 1].count } else { -1 }
}
fn main() {
	empty := []Item{}
	assert guarded(empty, true) == -1
	assert guarded(empty, false) == -1
	items := [Item(Record{count: 42})]
	assert guarded(items, true) == 42
	assert guarded(items, false) == -1
	variant := items[0]
	count := if true && variant is Record { variant.count } else { -1 }
	assert count == 42
	blocked := if false && items[choose()] is Record { 7 } else { -1 }
	assert blocked == -1
	assert calls == 0
	selected := if true && items[choose()] is Record { 7 } else { -1 }
	assert selected == 7
	assert calls == 1
	println("lazy sum subjects work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none',
		'-enable-globals', '-o', program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'lazy sum subjects work', run.output
}
