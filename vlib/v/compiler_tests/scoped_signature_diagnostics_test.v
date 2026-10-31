import os
import rand

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')

// A parallel check runs the top-level signature checks on a fork beside the body
// workers. Besides adding diagnostics they can replace earlier ones: the unknown
// types of a nongeneric declaration give way to its missing generic parameters.
fn test_parallel_signature_checks_merge_the_diagnostics_they_replace() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_scoped_signature_${os.getpid()}_${rand.ulid()}') +
		$if windows { '.exe' } $else { '' }
	build := os.exec([vexe, '-gc', 'none', '-prealloc', '-path', '${vlib_dir}' + '|@vlib|@vmodules',
		'-o', v3_bin, '${v3_src}'])
	assert build.exit_code == 0, build.output
	defer {
		os.rm(v3_bin) or {}
	}
	mut source := 'module main\n\nstruct Box {\n\ta T = T{}\n\tb T = T{}\n}\n'
	// Enough functions for the checker to use the worker pool.
	for i in 0 .. 64 {
		source += '\nfn pad_${i}() int {\n\treturn ${i}\n}\n'
	}
	source += '\nfn main() {}\n'
	base := os.join_path(os.temp_dir(), 'v3_scoped_signature_src_${os.getpid()}_${rand.ulid()}')
	os.write_file(base + '.v', source)!
	defer {
		os.rm(base + '.v') or {}
		os.rm(base + '.c') or {}
	}
	old_vjobs := os.getenv_opt('VJOBS')
	defer {
		if jobs := old_vjobs {
			os.setenv('VJOBS', jobs, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	os.setenv('VJOBS', '4', true)
	res := os.exec([v3_bin, '-nocache', '-o', base + '.c', base + '.v'])
	assert res.exit_code != 0, res.output
	assert !res.output.contains('V panic'), res.output
	errors := res.output.split_into_lines().filter(it.contains(': error: ')).map(it.all_after(': error: '))
	assert errors == ['cannot initialize field `a` with `void`; expected `T`',
		'cannot initialize field `b` with `void`; expected `T`',
		'generic struct `Box` declaration must specify the generic type names, e.g. Box[T]'], res.output
}
