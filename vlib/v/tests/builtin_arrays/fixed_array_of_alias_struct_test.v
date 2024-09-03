type Sfxinfo_t = Sfxinfo_struct

struct Sfxinfo_struct {
	name [9]i8
}

fn test_fixed_array_of_alias_struct() {
	a := [5]Sfxinfo_t{}
	println(a)
	assert true
}
