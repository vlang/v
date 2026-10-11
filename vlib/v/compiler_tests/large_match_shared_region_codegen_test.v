import os

const lmsr_vexe = @VEXE

// A worker of the parallel transform appends into a fixed share of the AST, sized
// by the cost of its functions. Lowering a function that is one dense `match`
// appends more than its node count, so two of them overflowed their regions:
// "array with the flag `.nogrow` cannot grow in size".
fn test_dense_matches_fit_their_transform_regions() {
	pid := os.getpid()
	src := os.join_path(os.temp_dir(), 'v3_large_match_shared_region_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_large_match_shared_region_program_${pid}')
	defer {
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	branch_count := 5000
	mut leaves := []string{cap: branch_count}
	mut names := []string{cap: branch_count}
	mut calls := []string{cap: branch_count}
	for i in 0 .. branch_count {
		leaves << 'fn leaf_${i}() {}'
		names << "\t\t${i} { 'leaf_${i}' }"
		calls << '\t\t${i} { leaf_${i}() }'
	}
	source := 'module main

${leaves.join('\n')}

fn leaf_name(index int) string {
\treturn match index {
${names.join('\n')}
\t\telse { "" }
\t}
}

fn call_leaf(index int) {
\tmatch index {
${calls.join('\n')}
\t\telse {}
\t}
}

fn main() {
\tcall_leaf(4999)
\tassert leaf_name(4999) == "leaf_4999"
\tassert leaf_name(${branch_count}) == ""
}
'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([lmsr_vexe, '-nocache', '-no-memory-limit', '${src}', '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
