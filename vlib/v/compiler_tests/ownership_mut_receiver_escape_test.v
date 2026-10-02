import os

fn test_ownership_mut_receiver_reference_cannot_escape_local_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_mut_receiver_escape_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for expression in ['builder.set(42)', 'builder.set(41).set(42)', 'reference',
		'if *drops == 0 { builder.set(42) } else { builder.set(41) }',
		'match *drops { 0 { builder.set(42) } else { builder.set(41) } }', 'unsafe { builder.set(42) }',
		'unsafe { if *drops == 0 { builder.set(42) } else { builder.set(41) } }',
		'if *drops == 0 { mut scoped := Builder{drops: drops}; scoped.set(42) } else { builder.set(41) }'] {
		binding := if expression == 'reference' { '\treference := builder.set(42)\n' } else { '' }
		os.write_file(source, 'struct Builder implements Drop {
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

fn escaped(drops &int) &Builder {
	mut builder := Builder{drops: drops}
${binding}\treturn ${expression}
}

fn main() { mut drops := 0; _ = escaped(&drops) }
')!
		result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -check ${os.quoted_path(source)}')
		assert result.exit_code != 0, '${expression}: ${result.output}'
		assert result.output.contains('cannot return a reference to local storage'), result.output
	}
}

fn test_ownership_mut_receiver_return_does_not_treat_shadowed_binding_as_receiver() {
	root := os.join_path(os.vtmp_dir(), 'ownership_mut_receiver_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Builder { mut: value int }
fn (mut builder Builder) reference() &Builder {
	if true { mut builder := Builder{value: 42}; return builder }
	return builder
}
fn main() { mut builder := Builder{}; _ = builder.reference() }
')!
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -check ${os.quoted_path(source)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('redefinition of `builder`'), result.output
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
}
')!
	for mode in ['-no-parallel', ''] {
		result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}
