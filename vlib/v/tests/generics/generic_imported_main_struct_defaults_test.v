import os

fn test_imported_generic_main_struct_literal_keeps_implicit_container_defaults() {
	check_imported_generic_main_struct_defaults('', '')
}

fn test_imported_generic_main_struct_literal_keeps_fixed_array_defaults() {
	check_imported_generic_main_struct_defaults('fixed [2]int = [5, 7]!', 'assert value.fixed == [5, 7]!')
}

fn test_imported_generic_main_struct_literal_keeps_shared_defaults() {
	check_imported_generic_main_struct_shared_defaults('')
}

fn test_imported_generic_main_struct_literal_keeps_shared_and_fixed_array_defaults() {
	check_imported_generic_main_struct_shared_defaults('fixed [2]int = [5, 7]!')
}

fn check_imported_generic_main_struct_shared_defaults(fixed_field string) {
	check_imported_generic_main_struct_defaults('shared_values shared []int
	shared_counts shared map[string]int
	shared_count shared int
	${fixed_field}', 'lock value.shared_values {
		assert value.shared_values.len == 0
		value.shared_values << 23
		assert value.shared_values == [23]
	}
	lock value.shared_counts {
		assert value.shared_counts.len == 0
		value.shared_counts["answer"] = 42
		assert value.shared_counts["answer"] == 42
	}
	lock value.shared_count {
		assert value.shared_count == 0
		value.shared_count = 17
		assert value.shared_count == 17
	}' + if fixed_field.len > 0 { '\nassert value.fixed == [5, 7]!' } else { '' })
}

fn check_imported_generic_main_struct_defaults(extra_fields string, extra_checks string) {
	root := os.join_path(os.vtmp_dir(), 'generic_main_defaults_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'factory')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'factory', 'factory.v'), 'module factory
pub struct Payload { other int = 99 }
pub struct Maker {}
pub fn make[T]() T { return T{} }
pub fn make_result[T]() !T { mut value := T{} return value }
pub fn (mut maker Maker) make_result[T]() !T { mut value := T{} return value }
pub fn make_pointer[T]() &T { return &T{} }
') or { panic(err) }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main
import factory
struct Payload {
mut:
	values []string
	numbers []int
	counts map[string]int
	marker int = 42
	${extra_fields}
}
fn check(mut value Payload) {
	assert value.marker == 42
	value.values << "España ñ"
	value.numbers << 17
	value.counts["one"] = 3
	assert value.values == ["España ñ"]
	assert value.numbers == [17]
	assert value.counts == {"one": 3}
	${extra_checks}
}
fn main() {
	mut plain := factory.make[Payload]()
	check(mut plain)
	mut fallible := factory.make_result[Payload]()!
	check(mut fallible)
	mut maker := factory.Maker{}
	mut method := maker.make_result[Payload]()!
	check(mut method)
	mut pointer := factory.make_pointer[Payload]()
	check(mut pointer)
}
') or { panic(err) }
	for flags in [['-b', 'c', '-nocache'], []string{}] {
		mut args := [@VEXE]
		args << flags
		args << ['-o', os.join_path(root, 'app'), source]
		compiled := os.exec(args)
		assert compiled.exit_code == 0, compiled.output
		run := os.exec([os.join_path(root, 'app')])
		assert run.exit_code == 0, run.output
	}
}
