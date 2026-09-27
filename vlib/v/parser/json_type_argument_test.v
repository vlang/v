module parser

import os
import v.pref

fn test_json_array_type_arguments_are_not_array_values() {
	path := os.join_path(os.vtmp_dir(), 'json_type_argument_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for source in [
		'import json\nfn main() { _ := json.decode([]string, "[]") or { []string{} } }',
		'import json as codec\nfn main() { _ := codec.decode([][]int, "[]") or { [][]int{} } }',
		'import json { decode }\nfn main() { _ := decode([]string, "[]") or { []string{} } }',
	] {
		os.write_file(path, source)!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert !p.diagnostics.any(it.message.contains('instead of')), p.diagnostics.str()
	}
	for source in [
		'fn decode(a []int) {}\nfn main() { decode([]int) }',
		'import json\nfn main() { json := Decoder{}\n json.decode([]int) }',
		'import json\nfn main() { _ := json.decode([]int, []string) }',
		'import json { decode }\nfn main() { decode := fn (a []int) {}; decode([]int) }',
	] {
		os.write_file(path, source)!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.any(it.message.contains('instead of')), p.diagnostics.str()
	}
}
