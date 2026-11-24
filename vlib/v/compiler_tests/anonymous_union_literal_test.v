import os

fn check_anonymous_union_source(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'anonymous_union_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	file := os.join_path(dir, 'main.v')
	os.write_file(file, source) or { panic(err) }
	return os.exec([@VEXE, '-o', os.join_path(dir, 'main.c'), file])
}

fn test_anonymous_union_literal_rejects_multiple_active_members() {
	result := check_anonymous_union_source('multiple', 'struct Box {
	value union {
		number int
		flag bool
	}
}
fn main() {
	_ = Box{value: union {number: 1, flag: true}}
}
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('can have only one field initialised'), result.output
}

fn test_anonymous_union_literal_checks_declared_member_types() {
	result := check_anonymous_union_source('type', 'struct Box {
	value union {
		number int
		flag bool
	}
}
fn main() {
	_ = Box{value: union {number: "incorrect"}}
}
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('expected `int`'), result.output
}

fn test_anonymous_union_literal_rejects_unknown_members() {
	result := check_anonymous_union_source('unknown', 'struct Box {
	value union {
		number int
		flag bool
	}
}
fn main() {
	_ = Box{value: union {missing: 1}}
}
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('unknown field `missing`'), result.output
}

fn test_anonymous_union_literal_cannot_initialize_an_anonymous_struct() {
	result := check_anonymous_union_source('struct', 'struct Box {
	value struct { number int }
}
fn main() {
	_ = Box{value: union {number: 1}}
}
')
	assert result.exit_code != 0, result.output
}

fn test_inferred_anonymous_union_rejects_multiple_active_members() {
	result := check_anonymous_union_source('inferred_multiple', 'fn main() {
	_ = union {number: 1, flag: true}
}
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('can have only one field initialised'), result.output
}

fn test_inferred_anonymous_union_reads_require_unsafe() {
	result := check_anonymous_union_source('unsafe', 'fn main() {
	value := union {number: 1}
	println(value.number)
}
')
	assert result.exit_code == 0, result.output
	assert result.output.contains('reading a union field (or its address) requires `unsafe`'), result.output
}

fn test_named_union_with_an_anonymous_prefix_keeps_its_nominal_type() {
	result := check_anonymous_union_source('nominal', 'union AnonUnion_User {number int}
struct Box {value union {number u64, other [32]u8}}
fn main() { _ = Box{value: AnonUnion_User{number: 7}} }
')
	assert result.exit_code != 0, result.output
}
