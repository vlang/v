module transform

import os

fn generic_numeric_field_program(directory string, name string, source string, check bool) os.Result {
	path := os.join_path(directory, name + '.v')
	os.write_file(path, source) or { panic(err) }
	mut args := [@VEXE, '-b', 'c']
	if check { args << '-check' }
	args << ['-o', os.join_path(directory, name), path]
	return os.exec(args)
}

fn test_generic_numeric_field_assignments_reject_incompatible_concrete_values() {
	directory := os.join_path(os.vtmp_dir(), 'generic_numeric_fields_${os.getpid()}')
	os.mkdir_all(directory) or { panic(err) }
	defer { os.rmdir_all(directory) or {} }
	for index, entry in [
		['f64', 'true', 'bool'],
		['f64', "'12'", 'string'],
		['int', 'false', 'bool'],
		['int', '1.5', 'f64'],
	] {
		for capture in [false, true] {
			body := if capture {
				'read := fn [value] [T] () ${entry[0]} { return Number{width: value}.width }\nreturn read()'
			} else {
				'return Number{width: value}.width'
			}
			source := 'module main\nstruct Number { width ${entry[0]} }\nfn number[T](value T) ${entry[0]} { ${body} }\nfn main() { _ = number(${entry[1]}) }\n'
			for check in [false, true] {
				name := 'invalid_${index}_${capture}_${check}'
				result := generic_numeric_field_program(directory, name, source, check)
				assert result.exit_code != 0, 'invalid numeric field compiled: ${source}'
				assert result.output.contains(name + '.v:'), result.output
				assert result.output.contains('cannot assign to field `width`'), result.output
				assert result.output.contains('not `${entry[2]}`'), result.output
			}
		}
	}
}

fn test_generic_numeric_fields_keep_valid_integer_float_and_alias_values() {
	directory := os.join_path(os.vtmp_dir(), 'generic_numeric_values_${os.getpid()}')
	os.mkdir_all(directory) or { panic(err) }
	defer { os.rmdir_all(directory) or {} }
	source := 'module main
type Measure = f64
struct Number { width f64 }
struct Model { width int height f32 }
struct Pair { first f64 second f64 }
struct Trace { mut: values []int }
fn (mut trace Trace) read(value int) !int { trace.values << value return value }
fn pair[M](mut model M) Pair {
    return Pair{first: model.values.len, second: model.read(2) or { panic(err) }}
}
fn direct[T](value T) f64 { return Number{width: value}.width }
fn captured[T](value T) f64 {
	read := fn [value] [T] () f64 { return Number{width: value}.width }
	return read()
}
fn model_width[M](model &M) f64 {
	read := fn [model] [M] () f64 { return Number{width: model.width}.width }
	return read()
}
fn model_height[M](model &M) f64 { return Number{width: model.height}.width }
fn divided[T](value T) f64 { return Number{width: value / 2}.width }
fn fallible[T](value T) !T { return value }
fn fallible_number[T](value T) f64 {
    read := fn [value] [T] () f64 { return Number{width: fallible(value) or { panic(err) }}.width }
    return read()
}
fn main() {
	assert direct(i8(-12)) == -12
	assert direct(i16(-34)) == -34
	assert direct(i32(-56)) == -56
	assert direct(i64(-78)) == -78
	assert direct(u8(12)) == 12
	assert direct(u16(34)) == 34
	assert direct(u32(56)) == 56
	assert direct(u64(78)) == 78
	assert captured(12) == 12
	assert captured(f32(1.25)) == 1.25
	assert captured(f64(2.5)) == 2.5
	assert captured(Measure(3.75)) == 3.75
	assert captured(1) / captured(2) == 0.5
	assert divided(3) == 1
	assert divided(f64(3)) == 1.5
	assert fallible_number(12) == 12
	assert fallible_number(f64(12.5)) == 12.5
	model := Model{width: 12, height: 1.25}
	assert model_width(&model) == 12
	assert model_height(&model) == 1.25
	mut trace := Trace{}
	observed := pair(mut trace)
	assert observed.first == 0
	assert observed.second == 2
	assert trace.values == [2]
}'
	result := generic_numeric_field_program(directory, 'valid', source, false)
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(directory, 'valid')])
	assert run.exit_code == 0, run.output
}

fn test_generic_numeric_fields_validate_fallible_generic_method_results() {
	directory := os.join_path(os.vtmp_dir(), 'generic_numeric_methods_${os.getpid()}')
	os.mkdir_all(directory) or { panic(err) }
	defer { os.rmdir_all(directory) or {} }
	for index, entry in [['bool', 'true'], ['string', "'12'"], ['int', '12'], ['f64', '12.5']] {
		verification := if entry[0] in ['bool', 'string'] {
			'_ = number(&value)'
		} else {
			'read_value := number(&value) assert read_value == ${entry[1]}'
		}
		source := 'module main\nstruct Number { width f64 }\nstruct Value { item ${entry[0]} }\nfn (value Value) get() !${entry[0]} { return value.item }\nfn number[M](model &M) f64 {\nread := fn [model] [M] () f64 { return Number{width: model.get() or { panic(err) }}.width }\nreturn read()\n}\nfn main() { value := Value{item: ${entry[1]}} ${verification} }\n'
		name := 'method_${index}'
		result := generic_numeric_field_program(directory, name, source, false)
		if entry[0] in ['bool', 'string'] {
			assert result.exit_code != 0, 'invalid generic method result compiled: ${source}'
			assert result.output.contains(name + '.v:'), result.output
			assert result.output.contains('cannot assign to field `width`'), result.output
			assert result.output.contains('not `${entry[0]}`'), result.output
		} else {
			assert result.exit_code == 0, result.output
			run := os.exec([os.join_path(directory, name)])
			assert run.exit_code == 0, run.output
		}
	}
}

fn test_struct_initializers_and_call_arguments_evaluate_before_later_or_preludes() {
	directory := os.join_path(os.vtmp_dir(), 'struct_numeric_order_${os.getpid()}')
	os.mkdir_all(directory) or { panic(err) }
	defer { os.rmdir_all(directory) or {} }
	source := 'module main
struct Pair { first f64 second f64 }
struct Trace { mut: values []int }
fn (mut trace Trace) read(value int) !int {
    trace.values << value
    if value < 0 { return error("negative") }
    return value
}
fn pair[M](mut model M) Pair {
    return Pair{first: model.values.len, second: model.read(2) or { panic(err) }}
}
fn combine(first f64, second f64) Pair { return Pair{first: first, second: second} }
fn call_pair[M](mut model M) Pair {
    return combine(model.values.len, model.read(2) or { panic(err) })
}
fn main() {
    mut generic := Trace{}
    generic_pair := pair(mut generic)
    assert generic_pair.first == 0
    assert generic_pair.second == 2
    assert generic.values == [2]
    mut plain := Trace{}
    plain_pair := Pair{first: plain.values.len, second: plain.read(-2) or { 3 }}
    assert plain_pair.first == 0
    assert plain_pair.second == 3
    assert plain.values == [-2]
    mut called := Trace{}
    call_result := call_pair(mut called)
    assert call_result.first == 0
    assert call_result.second == 2
    assert called.values == [2]
}'
	result := generic_numeric_field_program(directory, 'evaluation_order', source, false)
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(directory, 'evaluation_order')])
	assert run.exit_code == 0, run.output
}
