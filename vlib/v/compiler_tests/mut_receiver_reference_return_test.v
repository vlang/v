import os

fn run_mut_receiver_reference_return(name string, source string, ownership bool) os.Result {
	root := os.join_path(os.vtmp_dir(),
		'v_mut_receiver_reference_return_${name}_${ownership}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	out := os.join_path(root, 'out')
	os.write_file(path, source) or { panic(err) }
	flags := if ownership { '-d ownership -ownership' } else { '' }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache ${flags} -o ${os.quoted_path(out)} run ${os.quoted_path(path)}')
}

fn test_mut_receiver_reference_return_is_borrowed_in_ownership_mode() {
	source := '
struct Builder {
mut:
	value int
}

fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}

fn (mut builder Builder) increment() &Builder {
	builder.value++
	return builder
}

fn main() {
	mut builder := Builder{}
	builder.set(41).increment()
	assert builder.value == 42
	mut reference := builder.increment()
	assert voidptr(reference) == voidptr(&builder)
	reference.value = 44
	assert builder.value == 44
}
'
	for ownership in [false, true] {
		result := run_mut_receiver_reference_return('builder', source, ownership)
		if ownership {
			assert result.exit_code == 0, result.output
		} else {
			assert result.exit_code != 0, result.output
			assert result.output.contains('might be stored on stack'), result.output
		}
	}
}

fn test_generic_mut_receiver_reference_return_is_borrowed_in_ownership_mode() {
	source := '
struct Builder[T] {
mut:
	value T
}

fn (mut builder Builder[T]) set(value T) &Builder[T] {
	builder.value = value
	return builder
}

fn main() {
	mut builder := Builder[int]{}
	mut reference := builder.set(42)
	assert voidptr(reference) == voidptr(&builder)
	reference.value = 43
	assert builder.value == 43
}
'
	for ownership in [false, true] {
		result := run_mut_receiver_reference_return('generic', source, ownership)
		assert result.exit_code == 0, result.output
	}
}

fn test_mut_receiver_reference_return_keeps_plain_mut_parameter_restriction() {
	source := '
struct Builder {
mut:
	value int
}

fn reference(mut builder Builder) &Builder {
	return builder
}

fn main() {
	mut builder := Builder{}
	_ = reference(mut builder)
}
'
	for ownership in [false, true] {
		result := run_mut_receiver_reference_return('parameter', source, ownership)
		assert result.exit_code != 0, result.output
		assert result.output.contains('might be stored on stack'), result.output
	}
}

fn test_mut_receiver_reference_return_keeps_immutable_receiver_restriction() {
	source := '
struct Builder {
mut:
	value int
}

fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}

fn main() {
	builder := Builder{}
	_ = builder.set(42)
}
'
	for ownership in [false, true] {
		result := run_mut_receiver_reference_return('immutable', source, ownership)
		assert result.exit_code != 0, result.output
		assert result.output.contains('immutable'), result.output
	}
}
