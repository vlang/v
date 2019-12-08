module protobuf

struct Parser{
	file_path string
	file_name string
mut:
	scanner &Scanner
}