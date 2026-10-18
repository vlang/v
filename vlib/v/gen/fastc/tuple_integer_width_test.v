module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_integer_tuple_components_use_declared_storage_width() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_tuple_integer_width_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main
type Word = u64
struct Counter {
mut:
	value int
}
fn unsigned_pair() (u64, u64) { return 0, 22 }
fn aliased_pair() (Word, Word) { return 0, 22 }
fn signed_pair() (i64, i64) { return 7, -1 }
fn plain_pair() (int, int) { return 7, -1 }
fn counted(mut calls Counter) (u64, u64) {
	calls.value++
	return 0, 22
}
fn forwarded_if(flag bool, mut calls Counter) (u64, u64) {
	return if flag { counted(mut calls) } else { 0, 22 }
}
fn forwarded_match(selector int, mut calls Counter) (u64, u64) {
	return match selector {
		0 { counted(mut calls) }
		else { 0, 22 }
	}
}
fn main() {
	plain_first, plain_second := plain_pair()
	assert plain_first == 7 && plain_second == -1, "plain int tuple width"
	first, second := unsigned_pair()
	assert first == 0 && second == 22, "unsigned tuple width"
	alias_first, alias_second := aliased_pair()
	assert alias_first == Word(0) && alias_second == Word(22), "aliased tuple width"
	signed_first, signed_second := signed_pair()
	assert signed_first == 7 && signed_second == -1, "signed tuple width"
	mut calls := Counter{}
	if_first, if_second := forwarded_if(true, mut calls)
	assert if_first == 0 && if_second == 22 && calls.value == 1, "forwarded if tuple"
	else_first, else_second := forwarded_if(false, mut calls)
	assert else_first == 0 && else_second == 22 && calls.value == 1, "literal if tuple"
	match_first, match_second := forwarded_match(0, mut calls)
	assert match_first == 0 && match_second == 22 && calls.value == 2, "forwarded match tuple"
	other_first, other_second := forwarded_match(1, mut calls)
	assert other_first == 0 && other_second == 22 && calls.value == 2, "literal match tuple"
}
') or { panic(err) }
	baseline := os.join_path(root, 'baseline')
	baseline_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-gc', 'none', '-o', baseline,
		source_path], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert baseline_build.exit_code == 0, baseline_build.output
	baseline_run := cmdexec.run(baseline, [])
	assert baseline_run.exit_code == 0, baseline_run.output
	probe_source := os.join_path(root, 'pattern_probe.c')
	probe_object := os.join_path(root, 'pattern_probe.o')
	os.write_file(probe_source, 'int main(void) { return 0; }\n') or { panic(err) }
	pattern_probe := cmdexec.run('cc', ['-Werror', '-ftrivial-auto-var-init=pattern', '-c',
		probe_source, '-o', probe_object])
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target = pref.host_target()
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		c_path := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_path, source) or { panic(err) }
		mut flags := ['-std=gnu11', '-O2']
		if pattern_probe.exit_code == 0 {
			// Expose unwritten bytes when the C compiler supports stack-pattern initialization.
			flags << '-ftrivial-auto-var-init=pattern'
		}
		flags << ['-o', program, c_path]
		build := cmdexec.run('cc', flags)
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, 'target headers ${target_headers}: ${run.output}'
	}
}
