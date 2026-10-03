module c

import os

fn test_option_rejects_error_state_and_implicit_err() {
	sources := [
		'fn maybe[T]() ?T { return none }\nfn main() { _ := maybe[int]() or { println(err); 0 } }',
		'fn maybe() ?int { return none }\nfn main() { _ := maybe() or { println(err); 0 } }',
		'fn maybe() ?int { return none }\nfn main() { if _ := maybe() {} else { println(err) } }',
		'fn maybe(flag bool) ?int { return if flag { 1 } else { error("failure") } }',
		'fn main() { mut value := ?string(none)\nvalue = error("failure") }',
	]
	expected := ['err', 'err', 'err', 'mismatched types', 'cannot assign']
	path := os.join_path(os.vtmp_dir(), 'option_errors_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for i, source in sources {
		os.write_file(path, source)!
		command := '${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}'
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, source
		assert result.output.contains(expected[i]), result.output
	}
}
