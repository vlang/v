import os
import strings

fn test_repeated_generic_declarations_stay_within_memory_limit() {
	dir := os.join_path(os.vtmp_dir(), 'repeated_generic_declarations_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'main.v')
	for typ in ['&ZList[string]', 'ZList[string]', 'ZList[int]'] {
		for parameters in [false, true] {
			mut source := strings.new_builder(64 * 1024)
			source.writeln('struct ZList[T] { items []T }')
			for i in 0 .. 1000 {
				if parameters {
					source.writeln('fn consume_${i}(c ${typ}) {}')
				} else {
					source.writeln('struct S${i} { c ${typ} }')
				}
			}
			source.writeln('fn main() {}')
			os.write_file(path, source.str())!
			result := os.exec([@VEXE, '-new-compiler', '-nocache', '-check', '-memory-limit', '512M',
				path])
			assert result.exit_code == 0, '${typ}, parameters=${parameters}: ${result.output}'
		}
	}
}
