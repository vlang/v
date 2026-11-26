import os
import v.tempname

fn test_in_fixed_enum_literal_uses_left_operand_context() {
	dir := os.join_path(os.vtmp_dir(), 'v3_fixed_enum_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source_path := os.join_path(dir, 'main.v')
	output_path := os.join_path(dir, 'app')
	os.write_file(source_path, 'enum OperationKind {
	eq
	in
	not_in
}

fn is_array_operation(kind OperationKind) bool {
	return kind in [.in, .not_in]!
}

fn main() {
	assert is_array_operation(.in)
	assert is_array_operation(.not_in)
	assert !is_array_operation(.eq)
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-o', output_path, source_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([output_path])
	assert run.exit_code == 0, run.output
}
