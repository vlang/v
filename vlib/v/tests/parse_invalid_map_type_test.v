import os
import v.parser
import v.pref

fn test_parser_map_type() {
	pref_ := pref.Preferences{
		is_fmt: true
	}
	path := os.join_path(os.temp_dir(), 'v3_invalid_map_type_${os.getpid()}.v')
	os.write_file(path, 'a := map[*Node]bool') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(&pref_)
	result := p.parse_file(path)
	println(result)
	assert result.nodes.len > 0
}
