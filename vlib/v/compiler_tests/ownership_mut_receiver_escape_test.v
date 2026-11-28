import os

const mut_receiver_escape_args = [@VEXE, '-nocache', '-ownership', '-d', 'ownership']
const mut_receiver_escape_error = 'cannot return a reference to local storage'
const mut_receiver_shadow_error = 'redefinition of `builder`'
const mut_receiver_escape_declarations = 'struct Builder implements Drop {
mut:
	value int
	drops &int
}

fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}

fn (mut builder Builder) drop() {
	unsafe { *builder.drops += 1 }
}

'

fn mut_receiver_escape_function(name string, expression string) string {
	binding := if expression == 'reference' { '\treference := builder.set(42)\n' } else { '' }
	return 'fn ${name}(drops &int) &Builder {
	mut builder := Builder{drops: drops}
${binding}\treturn ${expression}
}
'
}

fn test_ownership_mut_receiver_returns_keep_local_storage_checks() {
	root := os.join_path(os.vtmp_dir(), 'ownership_mut_receiver_escape_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	expressions := ['builder.set(42)', 'builder.set(41).set(42)', 'reference',
		'if *drops == 0 { builder.set(42) } else { builder.set(41) }',
		'match *drops { 0 { builder.set(42) } else { builder.set(41) } }', 'unsafe { builder.set(42) }',
		'unsafe { { mut scoped := Builder{drops: drops}; scoped.set(42) } }',
		'unsafe { if *drops == 0 { builder.set(42) } else { builder.set(41) } }',
		'if *drops == 0 { mut scoped := Builder{drops: drops}; scoped.set(42) } else { builder.set(41) }']
	// Custom flags can limit diagnostics or change their format.
	batched := os.getenv('VFLAGS').trim_space() in ['', '-gc none']
	if batched {
		check_mut_receiver_escape_batch(root, expressions)!
	} else {
		source := os.join_path(root, 'main.v')
		for expression in expressions {
			os.write_file(source, mut_receiver_escape_declarations +
				mut_receiver_escape_function('escaped', expression) +
				'\nfn main() { mut drops := 0; _ = escaped(&drops) }\n')!
			result := os.exec([...mut_receiver_escape_args, '-check', source])
			assert result.exit_code != 0, '${expression}: ${result.output}'
			assert result.output.contains(mut_receiver_escape_error), '${expression}: ${result.output}'
		}
		os.write_file(source, mut_receiver_shadow_declarations('Builder') +
			'fn main() { mut builder := Builder{}; _ = builder.reference() }\n')!
		shadow := os.exec([...mut_receiver_escape_args, '-check', source])
		assert shadow.exit_code != 0, shadow.output
		assert shadow.output.contains(mut_receiver_shadow_error), shadow.output
	}
	check_mut_receiver_deep_local_return(os.join_path(root, 'deep'), batched)!
}

fn check_mut_receiver_escape_batch(root string, expressions []string) ! {
	mut calls := []string{cap: expressions.len + 2}
	for i, expression in expressions {
		name := 'escape_case_${i}'
		os.write_file(os.join_path(root, '${name}.v'), 'module main\n\n' +
			mut_receiver_escape_function(name, expression))!
		calls << '\t{\n\t\tmut drops := 0\n\t\t_ = ${name}(&drops)\n\t}'
	}
	os.write_file(os.join_path(root, 'deep_case.v'), 'module main\n\n' +
		mut_receiver_deep_local_alias_declarations('DeepBuilder', 'deep_reference'))!
	calls << '\tassert deep_reference().value == 42'
	os.write_file(os.join_path(root, 'shadow_case.v'), 'module main\n\n' +
		mut_receiver_shadow_declarations('ShadowBuilder'))!
	calls << '\t{ mut builder := ShadowBuilder{}; _ = builder.reference() }'
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\n' +
		mut_receiver_escape_declarations + 'fn main() {\n${calls.join('\n')}\n}\n')!
	result := os.exec([...mut_receiver_escape_args, '-check', root])
	assert result.exit_code != 0, result.output
	diagnostics := result.output.split_into_lines()
	for i, expression in expressions {
		assert diagnostics.any(it.contains('escape_case_${i}.v:')
			&& it.contains('error: ${mut_receiver_escape_error}')), '${expression}: ${result.output}'
	}
	assert diagnostics.any(it.contains('deep_case.v:')
		&& it.contains('error: ${mut_receiver_escape_error} `builder`')), result.output
	assert diagnostics.any(it.contains('shadow_case.v:')
		&& it.contains('error: ${mut_receiver_shadow_error}')), result.output
}

fn mut_receiver_shadow_declarations(typ string) string {
	return 'struct ${typ} { mut: value int }
fn (mut builder ${typ}) reference() &${typ} {
	if true { mut builder := ${typ}{value: 42}; return builder }
	return builder
}
'
}

fn test_ownership_mut_receiver_reference_can_return_caller_or_heap_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_mut_receiver_safe_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Builder {
mut:
	value int
}

fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}

fn caller_reference(mut builder Builder) &Builder {
	return builder.set(42)
}

fn heap_reference() &Builder {
	mut builder := &Builder{}
	return builder.set(41).set(42)
}

fn caller_branch_reference(mut builder Builder, choose bool) &Builder {
	return if choose {
		builder = Builder{}
		builder.set(42)
	} else {
		builder.set(41)
	}
}

fn heap_branch_reference(choose bool) &Builder {
	mut first := &Builder{}
	mut second := &Builder{}
	return match choose {
		true { first.set(42) }
		false { unsafe { second.set(41) } }
	}
}

fn main() {
	mut builder := Builder{}
	reference := caller_reference(mut builder)
	assert voidptr(reference) == voidptr(&builder)
	assert builder.value == 42
	assert heap_reference().value == 42
	assert voidptr(caller_branch_reference(mut builder, true)) == voidptr(&builder)
	assert builder.value == 42
	assert heap_branch_reference(true).value == 42
	assert heap_branch_reference(false).value == 41
	assert deep_heap_reference().value == 42
}
' + mut_receiver_deep_alias_functions('Builder', 'deep_heap_reference', true))!
	for mode in [['-no-parallel'], []string{}] {
		result := os.exec([...mut_receiver_escape_args, '-cc', 'clang', ...mode, 'run', source])
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}

fn mut_receiver_deep_local_alias_declarations(typ string, name string) string {
	return 'struct ${typ} { mut: value int }
fn (mut builder ${typ}) set(value int) &${typ} { builder.value = value; return builder }
' + mut_receiver_deep_alias_functions(typ, name, false)
}

fn mut_receiver_deep_alias_functions(typ string, name string, heap bool) string {
	mut expression := 'builder.set(42)'
	for _ in 0 .. 80 {
		expression = 'identity(${expression})'
	}
	pointer := if heap { '&' } else { '' }
	return 'fn identity(builder &${typ}) &${typ} { return builder }
fn ${name}() &${typ} {
	mut builder := ${pointer}${typ}{}
	return ${expression}
}
'
}

fn check_mut_receiver_deep_local_return(root string, batched bool) ! {
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	os.write_file(source, mut_receiver_deep_local_alias_declarations('Builder', 'reference') +
		'fn main() { assert reference().value == 42 }\n')!
	modes := if batched { [['-no-parallel']] } else { [['-no-parallel'], []string{}] }
	for mode in modes {
		out := os.exec([...mut_receiver_escape_args, ...mode, '-check', source])
		assert out.exit_code != 0, out.output
		assert out.output.contains('${mut_receiver_escape_error} `builder`'), out.output
	}
}
