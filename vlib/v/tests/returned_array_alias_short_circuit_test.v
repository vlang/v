import os

fn test_return_alias_in_short_circuit_result_fallback() {
	for index, condition in [
		'helper is Mapper && (gate() or { return helper(values) })',
		'helper !is Mapper || (gate() or { return helper(values) })',
	] {
		path := os.join_path(os.vtmp_dir(), 'v3_return_alias_short_circuit_${os.getpid()}_${index}.v')
		os.write_file(path, 'type Mapper = fn ([]int) []int\ntype MapperOrInt = Mapper | int\nfn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn gate() !bool { return error("closed") }\nfn nested(values []int, helper MapperOrInt) []int { if ${condition} {} ; return values.clone() }\nfn main() { original := [1, 2]; mut alias := nested(original, MapperOrInt(Mapper(passthrough))); alias[0] = 9 }\n')!
		defer { os.rm(path) or {} }
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('immutable'), result.output
	}
}
