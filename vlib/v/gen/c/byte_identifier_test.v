module c

import os
import v.cmdexec

fn test_byte_global_object_and_callback_compile_and_run() {
	test_dir := os.join_path(os.vtmp_dir(), 'cgen_byte_global_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	for fixture, source in {
		'scalar':   'module main
__global byte int
fn update() { byte = 8; byte += 2; byte++; byte-- }
fn read() int { return byte }
fn main() {
	println(byte)
	update()
	println(read())
	byte = 12
	println(byte)
	println(read())
}
'
		'function': 'module main
__global byte fn (int) int
fn increment(value int) int { return value + 1 }
fn main() { byte = increment; println(byte(8)) }
'
	} {
		v_file := os.join_path(test_dir, '${fixture}.v')
		c_file := os.join_path(test_dir, '${fixture}.c')
		bin_file := os.join_path(test_dir, fixture)
		os.write_file(v_file, source) or { panic(err) }
		generated := cmdexec.run(@VEXE, ['-new-compiler', '-gc', 'none', '-enable-globals', '-cc',
			'clang', '-o', c_file, v_file])
		assert generated.exit_code == 0, generated.output
		compiled := cmdexec.run('clang', ['-std=gnu11', '-o', bin_file, c_file, '-lm', '-lpthread'])
		assert compiled.exit_code == 0, compiled.output
		executed := cmdexec.run(bin_file, [])
		assert executed.exit_code == 0, executed.output
		assert executed.output == if fixture == 'scalar' { '0\n10\n12\n12\n' } else { '9\n' }
	}
}

fn test_byte_global_symbol_mapping_preserves_external_names() {
	mut g := FlatGen.new()
	assert g.global_c_name('byte') != 'byte'
	assert g.global_c_name('main.byte') == g.global_c_name('byte')
	assert g.global_c_name('worker.byte') == 'worker__byte'
	assert g.global_c_name('C.byte') == 'byte'
	g.export_global_names['byte'] = 'external_byte'
	assert g.global_c_name('byte') == 'external_byte'
	g.export_global_names.clear()
	g.c_extern_global_names['byte'] = 'external_byte'
	assert g.global_c_name('byte') == 'external_byte'
}
