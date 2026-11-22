module main

import os

const nested_fixed_literal_source = 'struct Uniforms {
	lights [2][4]f32
}

fn row() [4]f32 {
	return [f32(1.5), 2.5, 3.5, 4.5]!
}

fn sum(a [2][4]f32) f32 {
	return a[0][0] + a[1][3]
}

@[noinline]
fn nested_rows_sum() f32 {
	u := Uniforms{
		lights: [row(), row()]!
	}
	return u.lights[1][3] + sum([row(), row()]!)
}

fn main() {
	assert nested_rows_sum() == 10.5
}
'

// The rows of a fixed-array literal that C initializers cannot express are filled in a
// statement expression. Its value must not be a block-scoped array: that decays to a
// pointer to storage which ends with the block, before the caller copies from it
// (AddressSanitizer: stack-use-after-scope). A struct rvalue's array member stays alive
// until the end of the full expression.
fn test_nested_fixed_array_literal_storage_outlives_its_statement_expression() {
	path := os.join_path(os.vtmp_dir(), 'nested_fixed_literal_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, nested_fixed_literal_source)!
	// Name a GCC-style compiler: the statement expressions checked below are not
	// written for `-cc msvc`, which the MSVC CI jobs pass in VFLAGS.
	result := os.exec([@VEXE, '-cc', 'gcc', '-o', output, path])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	mut body := generated
	for {
		body = body.all_after(' nested_rows_sum(')
		if body.all_before('\n').ends_with(') {') || body.len == 0 {
			break
		}
	}
	body = body.all_after('{\n').all_before('\n}')
	assert body.count('struct { float arr[2][4]; } ') == 2, body
	assert body.count('; }).arr') == 2, body
	assert !body.contains('float _t'), body
}
