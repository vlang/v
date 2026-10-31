module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_translated_division_rejects_unproven_or_zero_divisors() {
	for selfhost in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = selfhost
		for divisor in ['count', '(count)', 'u32(count)', '0', '(0)', 'u32(0)', 'u8(256)', 'u8(0x100)',
			'u16(65536)', 'u32(4294967296)', 'i32(4294967296)', '-1', 'i8(128)', '(1000 - 1000)',
			'u32(1000 - count)', 'u32(1000)(count)', 'u32(1000)[0]', 'u32(1000).field',
			'(u8(255) + u8(1))', '(u32(4294967295) + u32(1))', '(9223372036854775807 + 1)', '0.5'] {
			for body in ['return value / ${divisor}', 'return value % ${divisor}',
				'value /= ${divisor}; return value', 'value %= ${divisor}; return value',
				'*ptr /= ${divisor}; return value', '(*ptr) %= ${divisor}; return value'] {
				mut message := ''
				_ := generate('@[translated]\nmodule main\nfn divide(value int, count int, ptr &int) int { ${body} }\n', 'translated_division.v', prefs) or {
					message = err.msg()
					''
				}
				assert message.contains('division or modulo expressions'), '${selfhost}: ${body}: ${message}'
			}
		}
		if !selfhost {
			mut message := ''
			_ := generate('@[translated]\nmodule main\nfn divide(value int) int { return value / u32(1000) }\n', 'translated_division.v', prefs) or {
				message = err.msg()
				''
			}
			assert message.contains('division or modulo expressions'), message
		}
	}
}

fn test_fastc_translated_positive_constant_division_matches_ordinary_c() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_translated_division_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'@[translated]
module main
struct Socket { keep_idle u32 }
fn seconds(socket &Socket, output &i32) {
	unsafe { *output = i32(socket.keep_idle / u32(1000)) }
}
fn mutate(initial u32, ptr &u32) u32 {
	mut value := initial
	value /= u32(1000)
	value %= u32(3)
	unsafe {
		*ptr /= 1000
		(*ptr) %= u32(3)
	}
	return value
}
fn frequency(value u64) bool {
	return value > u64(-1) / 4 || value % u64(2) != u64(0)
}
fn main() {
	mut output := i32(0)
	seconds(&Socket{keep_idle: 7999}, &output)
	assert output == 7
	mut pointed := u32(8999)
	value := mutate(7999, &pointed)
	assert value == 1
	assert pointed == 2
	assert !frequency(2)
	assert frequency(3)
	assert frequency(u64(-1))
	assert i32(-7999) / i32(1000) == -7
	assert i32(-7999) % i32(1000) == -999
	assert u32(7999) / (u32(1000)) == 7
	assert u32(7999) / u8(232) == 34
	assert u32(7999) / u8(-1) == 31
	assert u32(7999) / 1000 * 2 == 14
	assert u32(7999) / (900 + 100) == 7
	println("translated constant division works")
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
	ordinary_run := cmdexec.run(ordinary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	generated := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'translated constant division works\n', run.output
}
