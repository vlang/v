module toml

import (
	os
	strings
)

const (
	single_quote = `\'`
	double_quote = `"`
)

struct Scanner{
mut:
	file_path	string
	text		string
	pos			int
	line_nr 	int
	last_nl_pos	int
	
}