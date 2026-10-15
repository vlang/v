module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_translated_target_checked_division_matches_ordinary_c() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_checked_division_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.c.v'), r'@[translated]
module main
fn C.getenv(&char) &char

type Blocks = u64
struct State {
mut:
	order i32
	calls i32
	targets i32
	value u64
	guard u64 = 0x123456789abcdef0
}
fn next_value(state &State, tag i32, value u64) u64 {
	unsafe { state.order = state.order * 10 + tag; state.calls++ }
	return value
}
fn next_index(state &State) int {
	unsafe { state.targets++ }
	return 0
}
fn storage(state &State) &u64 {
	unsafe { state.targets++; return &state.value }
}
fn divide32(value i32, count i32) i32 { return value / count }
fn modulo32(value i32, count i32) i32 { return value % count }
fn divide64(value i64, count i64) i64 { return value / count }
fn divide_unsigned(value u64, count u64) u64 { return value / count }
fn modulo_unsigned(value u64, count u64) u64 { return value % count }
fn alias_divide(value Blocks, count Blocks) Blocks { return value / count }
fn truncate_divisor(value u8, count u32) u8 { return value / u8(count) }
fn divide_pointer(value &u64, count u64) { unsafe { *value /= count } }
fn modulo_pointer(value &u64, count u64) { unsafe { (*value) %= count } }

fn main() {
	mode := unsafe { C.getenv(c"FASTC_DIVISION_MODE") }
	if mode != unsafe { nil } {
		letter := unsafe { mode[0] }
		if letter == `d` { println(divide32(7, 0)); return }
		if letter == `m` { println(modulo32(7, 0)); return }
		if letter == `t` { println(truncate_divisor(7, 256)); return }
		if letter == `s` { println(divide32(i32(0x80000000), -1)); return }
		if letter == `l` { println(divide64(i64(0x8000000000000000), -1)); return }
		if letter == `b` {
			mut state := State{}
			println(next_value(&state, 1, 7) / next_value(&state, 2, 0))
			return
		}
		if letter == `n` {
			mut state := State{}
			println(next_value(&state, 1, 7) % next_value(&state, 2, 0))
			return
		}
		if letter == `a` {
			mut state := State{}
			values := [u64(7)]
			println(values[next_index(&state)] / next_value(&state, 2, 0))
			return
		}
		mut value := u64(7)
		if letter == `p` { divide_pointer(&value, 0); return }
		if letter == `q` { modulo_pointer(&value, 0); return }
	}
	assert divide32(-7999, 1000) == -7
	assert modulo32(-7999, 1000) == -999
	assert divide32(2147483647, 3) == 715827882
	assert divide64(-7999, 1000) == -7
	assert divide_unsigned(0xffffffffffffffff, 3) == 0x5555555555555555
	assert modulo_unsigned(0xffffffffffffffff, 7) == 1
	assert divide_unsigned(0x8000000000000000, 2) == 0x4000000000000000
	assert alias_divide(Blocks(0xffffffffffffffff), Blocks(3)) == Blocks(0x5555555555555555)
	assert truncate_divisor(200, 258) == 100
	mut state := State{value: 7999}
	assert next_value(&state, 1, 7999) / next_value(&state, 2, 1000) == 7
	assert state.order == 12
	assert state.calls == 2
	state.order = 0
	state.calls = 0
	assert next_value(&state, 1, 7999) % next_value(&state, 2, 1000) == 999
	assert state.order == 12
	assert state.calls == 2
	unsafe { *storage(&state) /= next_value(&state, 3, 1000) }
	assert state.value == 7
	assert state.targets == 1
	assert state.calls == 3
	unsafe { (*storage(&state)) %= next_value(&state, 4, 3) }
	assert state.value == 1
	assert state.targets == 2
	assert state.calls == 4
	mut values := [u64(8999)]
	values[next_index(&state)] /= next_value(&state, 5, 1000)
	assert values[0] == 8
	assert state.targets == 3
	assert state.calls == 5
	values[next_index(&state)] %= next_value(&state, 6, 3)
	assert values[0] == 2
	assert state.targets == 4
	assert state.calls == 6
	mut local := u32(8999)
	count := u32(1000)
	local /= count
	local %= u32(3)
	assert local == 2
	state.value = 8999
	state.value /= count
	state.value %= u64(3)
	assert state.value == 2
	assert state.guard == 0x123456789abcdef0
	println("checked target division works")
}
') or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
		'-o', ordinary, root], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run_in_merged_with_env(ordinary, [], root, {
		'FASTC_DIVISION_MODE': ''
	})
	assert ordinary_run.exit_code == 0, ordinary_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', pref.host_arch()) or { panic(err) }
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = ['@vlib/builtin/backtraces_nix.c.v']
	generated := generate_files([root], prefs) or { panic(err) }
	assert generated.contains('division by zero')
	assert generated.contains('modulo by zero')
	assert generated.contains('__vf_div_target')
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-fno-strict-aliasing', '-D__vinix__', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run_in_merged_with_env(program, [], root, {
		'FASTC_DIVISION_MODE': ''
	})
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'checked target division works\n', run.output
	for mode, message in {
		'd': 'division by zero'
		'm': 'modulo by zero'
		't': 'division by zero'
		'p': 'division by zero'
		'q': 'modulo by zero'
		's': 'integer overflow'
		'l': 'integer overflow'
		'b': 'division by zero'
		'n': 'modulo by zero'
		'a': 'division by zero'
	} {
		failed := cmdexec.run_in_merged_with_env(program, [], root, {
			'FASTC_DIVISION_MODE': mode
		})
		assert failed.exit_code != 0, '${mode}: ${failed.output}'
		assert failed.output.contains(message), '${mode}: ${failed.output}'
		if mode in ['d', 'm', 't', 'b', 'n', 'a'] {
			baseline := cmdexec.run_in_merged_with_env(ordinary, [], root, {
				'FASTC_DIVISION_MODE': mode
			})
			assert baseline.exit_code != 0, '${mode}: ${baseline.output}'
			assert baseline.output.contains(message), '${mode}: ${baseline.output}'
		}
	}
}
