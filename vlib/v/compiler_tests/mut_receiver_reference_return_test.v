// vtest ownership
import os

fn run_mut_receiver_reference_return(name string, source string, ownership bool, compiler_flags ...string) os.Result {
	root := os.join_path(os.vtmp_dir(),
		'v_mut_receiver_reference_return_${name}_${ownership}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	out := os.join_path(root, 'out')
	os.write_file(path, source) or { panic(err) }
	flags := if ownership { ['-d', 'ownership', '-ownership'] } else { []string{} }
	return os.exec([@VEXE, '-no-memory-limit', '-nocache', ...flags, ...compiler_flags, '-o', out,
		'run', path])
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

fn test_mut_receiver_reference_return_keeps_restrictions() {
	source := '
struct ParameterBuilder {
mut:
	value int
}

fn reference(mut builder ParameterBuilder) &ParameterBuilder {
	return builder
}

fn check_parameter() {
	mut builder := ParameterBuilder{}
	_ = reference(mut builder)
}

struct Builder {
mut:
	value int
}

fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}

fn check_immutable() {
	builder := Builder{}
	_ = builder.set(42)
}

fn main() {
	check_parameter()
	check_immutable()
}
'
	for ownership in [false, true] {
		// Keep both diagnostics visible if VFLAGS supplies a lower limit.
		result := run_mut_receiver_reference_return('restrictions', source, ownership, '-message-limit',
			'-1')
		assert result.exit_code != 0, result.output
		assert result.output.contains('might be stored on stack. Consider declaring `ParameterBuilder` as `@[heap]`'), result.output
		assert result.output.contains('`builder` is immutable, declare it with `mut`'), result.output
	}
}
