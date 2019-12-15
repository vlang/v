module toml

import (
	os
	strings
)

const (
	err_syntax = ('toml: syntax error.')
)

struct Parser{
mut:
	scanner &Scanner
}