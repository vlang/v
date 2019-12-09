module toml

import (
	os
	strings
)

const (
	err_array
)

struct Parser{
	file_path string
	file_name string
mut:
	scanner &Scanner
}