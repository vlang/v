import os

fn slice_consumer_function(source string, name string) string {
	for line in source.split_into_lines() {
		if line.len > 0 && !line[0].is_space() && line.contains('${name}(') && line.ends_with(' {') {
			return source.all_after(line).all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}

fn test_immediate_string_slice_consumers() {
	root := os.join_path(os.vtmp_dir(), 'string_slice_consumers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(os.dir(@FILE), 'testdata', 'string_slice_consumers.vv'), source)!
	generated := os.join_path(root, 'main.c')
	codegen := os.exec([@VEXE, '-new-compiler', '-nocache', '-o', generated, source])
	assert codegen.exit_code == 0, codegen.output
	c := os.read_file(generated)!
	for name in ['slice_equal', 'slice_reversed', 'slice_unequal', 'slice_open', 'slice_empty',
		'slice_unicode', 'emit_range', 'emit_open', 'ordered_equal', 'ordered_write', 'guarded_equal',
		'guarded_or', 'snapshot_header'] {
		body := slice_consumer_function(c, name)
		assert body.contains('string__substr_borrowed('), '${name}: ${body}'
		assert !body.contains('string__substr('), '${name}: ${body}'
	}
	assert slice_consumer_function(c, 'emit_range').contains('write_string_borrowed(')
	// Rewritten propagation expressions lack the source ancestry needed by the
	// immediate-consumer proof and deliberately retain the ordinary slice.
	for name in ['saved_slice', 'stored_slice', 'compare_variable', 'retained_slice', 'deferred_slice',
		'propagated_bound', 'custom_builder'] {
		assert slice_consumer_function(c, name).contains('string__substr('), name
	}
	for mode in ['', '-prod', '-autofree', '-gc none'] {
		binary := os.join_path(root, 'program' + $if windows { '.exe' } $else { '' })
		compiler := if mode == '-prod' && @CCOMPILER in ['tinyc', 'tcc'] {
			'cc'
		} else {
			@CCOMPILER
		}
		mut args := [@VEXE, '-new-compiler', '-nocache', '-no-retry-compilation', '-cc', compiler]
		if mode != '' { args << mode.split(' ') }
		args << ['-o', binary, source]
		built := os.exec(args)
		assert built.exit_code == 0, '${mode}: ${built.output}'
		run_args := if mode == '-autofree' { [binary, 'ownership'] } else { [binary] }
		ran := os.exec(run_args)
		assert ran.exit_code == 0 && ran.output.trim_space() == 'ok', '${mode}: ${ran.output}'
		for consumer in ['equal', 'write'] {
			for bounds in [['-1', '2'], ['0', '4'], ['2', '1'], ['4', '4'], ['0', '-1']] {
				invalid := os.exec([binary, consumer, ...bounds])
				assert invalid.exit_code != 0, '${mode} ${consumer} ${bounds}: ${invalid.output}'
				assert invalid.output.contains('out of bounds'), invalid.output
				assert !invalid.output.contains('after-invalid-range'), invalid.output
			}
		}
		empty := os.exec([binary, 'empty', '4', '4'])
		assert empty.exit_code != 0 && empty.output.contains('out of bounds'), empty.output
	}
	$if !windows {
		// Newly selected private consumers must also survive module-cache reuse.
		compiler := if @CCOMPILER in ['tinyc', 'tcc'] { 'cc' } else { @CCOMPILER }
		binary := os.join_path(root, 'cached')
		for _ in 0 .. 2 {
			built := os.exec(['env', 'V3CACHE=${os.join_path(root, 'cache')}', @VEXE, '-new-compiler',
				'-no-retry-compilation', '-cc', compiler, '-o', binary, source])
			assert built.exit_code == 0, built.output
			ran := os.exec([binary])
			assert ran.exit_code == 0 && ran.output.trim_space() == 'ok', ran.output
		}
	}
}
