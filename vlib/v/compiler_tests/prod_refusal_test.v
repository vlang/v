import os

// The invocations below stop before any C is generated, so they run on every host.
// `-prod` refuses TinyCC, and FastC refuses `-prod` in both `v` and `v self`.

fn prod_refusal_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v_prod_refusal_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main\nfn main() { println(42) }\n') or {
		panic(err)
	}
	return root
}

fn test_prod_refuses_tinycc_before_any_compiler_runs() {
	root := prod_refusal_root('tinycc')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	for args in [['-prod', '-cc', 'tcc'], ['-cc=tinyc', '-prod'],
		['-old-compiler', '-prod', '-cc', 'tcc']] {
		result := os.exec([@VEXE, ...args, '-o', os.join_path(root, 'main'), source])
		assert result.exit_code == 1, '${args}: ${result.output}'
		assert result.output.contains('tcc is not allowed with -prod'), '${args}: ${result.output}'
		assert (os.ls(root) or { []string{} }) == ['main.v'], '${args}'
	}
}

fn test_prod_refuses_fastc_before_it_writes_output() {
	root := prod_refusal_root('fastc')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	for output in ['prod', 'prod.c'] {
		result := os.exec([@VEXE, '-silent', '-prod', '-b', 'fastc', '-o', os.join_path(root, output),
			source])
		assert result.exit_code != 0, result.output
		assert result.output.contains('fastc parser does not support `-prod`'), result.output
		assert (os.ls(root) or { []string{} }) == ['main.v'], output
	}
}

fn test_self_refuses_prod_with_fastc() {
	root := prod_refusal_root('self')
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.exec([@VEXE, 'self', '-silent', '-prod', '-b', 'fastc', '-o',
		os.join_path(root, 'v prod')])
	assert result.exit_code != 0, result.output
	assert result.output.contains('does not support `-prod`'), result.output
	assert (os.ls(root) or { []string{} }) == ['main.v'], result.output
}

fn test_prod_tinycc_flags_allow_information_commands() {
	for command in ['version', 'help'] {
		result := os.exec([@VEXE, '-prod', '-cc', 'tcc', command])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('tcc is not allowed with -prod'), result.output
	}
}

fn test_old_compiler_prod_refuses_a_tinycc_alias() {
	$if windows {
		return
	}
	root := prod_refusal_root('alias')
	defer { os.rmdir_all(root) or {} }
	alias := os.join_path(root, 'cc')
	os.write_file(alias, '#!/bin/sh\nprintf "tcc version 0.9.27\\n"\n')!
	os.chmod(alias, 0o700)!
	result := os.exec([@VEXE, '-old-compiler', '-prod', '-cc', alias, os.join_path(root, 'main.v')])
	assert result.exit_code == 1, result.output
	assert result.output.contains('tcc is not allowed with -prod'), result.output
}
