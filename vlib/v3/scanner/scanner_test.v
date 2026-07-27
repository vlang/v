module scanner

import v3.pref
import v3.token

fn test_power_tokens() {
	source := 'a ** b **= c'
	mut files := token.FileSet.new()
	mut file := files.add_file('power.v', source.len)
	file.index_lines(source)
	preferences := &pref.Preferences{}
	mut scanner := new_scanner(preferences, .normal)
	scanner.init(file, source)
	assert scanner.scan() == .name
	assert scanner.scan() == .power
	assert scanner.scan() == .name
	assert scanner.scan() == .power_assign
	assert scanner.scan() == .name
	assert scanner.scan() == .semicolon
	assert scanner.scan() == .eof
}
