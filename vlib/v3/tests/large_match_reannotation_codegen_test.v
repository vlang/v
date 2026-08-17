import os

const lmr_vexe = @VEXE
const lmr_tests_dir = os.dir(@FILE)
const lmr_v3_dir = os.dir(lmr_tests_dir)
const lmr_vlib_dir = os.dir(lmr_v3_dir)
const lmr_v3_src = os.join_path(lmr_v3_dir, 'v3.v')

// Match lowering produces an else-if chain. Post-transform type reannotation
// must walk very large generated tables without overflowing the native stack.
fn test_large_match_reannotation_does_not_overflow() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_large_match_reannotation_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_large_match_reannotation_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_large_match_reannotation_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${lmr_vexe} -gc none -d ownership -path "${lmr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${lmr_v3_src}')
	assert build.exit_code == 0, build.output
	mut branches := []string{cap: 1800}
	for i in 0 .. 1800 {
		branches << "\t\t'key_${i}' { return ${i} }"
	}
	source := 'module main

fn identity[T](value T) T {
\treturn value
}

fn classify(value string) int {
\tmatch value {
${branches.join('\n')}
\t\telse { return -1 }
\t}
}

fn main() {
\tassert classify(identity[string]("key_1799")) == 1799
}
'
	os.write_file(src, source) or { panic(err) }
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}
