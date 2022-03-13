import os
import flag

pub fn read_file(file string) ?[]string {
	if os.is_file(file) {
		text := os.read_lines(file) or {
			return error(@MOD + '.' + @STRUCT + '.' + @FN + ' Could not read "$file": "$err.msg()"')
		}
		return text
	}
	return ['']
}

pub fn extract_transitions(line string) ?string {
	mut result := '  '
	first_comma := line.index(',') ?
	second_comma := line.index_after(',', first_comma + 1)

	from := line[..first_comma]
	to := line[first_comma + 1..second_comma]
	condition := line[second_comma + 1..]

	return result + from + ' -> ' + to + ' [label=' + condition + '];'
}

pub fn get_transitions(line string) ?string {
	mut raw_text := line[line.index_any('(') + 1..line.index_any(')')]
	raw_text = raw_text.replace("'", '').replace(' ', '')
	return extract_transitions(raw_text)
}

pub fn main() {
	mut fp := flag.new_flag_parser(os.args)
	file := fp.string('file', `f`, '', 'input V file with transitions to generate graph from.')
	lines := read_file(file) ?
	println('digraph fsm {')
	for line in lines {
		if line.contains('add_transition') {
			println(get_transitions(line) ?)
		}
	}

	println('}')
}
