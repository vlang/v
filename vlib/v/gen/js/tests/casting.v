type Type = string | bool

fn main() {
	t := Type('')
	assert t is string && t == ''
}
