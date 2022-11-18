struct Shared {
	val int
}

struct AA<T> {
	Shared
}

type AAint = AA<int>
type AAbool = AA<bool>

struct BB<T> {
	Shared
}

type BBint = BB<int>
type BBbool = BB<bool>

type CC = AAbool | AAint | BBbool | BBint

fn (c CC) str() string {
	return '${c.val}'
}

fn test_generic_sumtype_of_alias_generic_struct() {
	mut c := []CC{}
	c << AAint{
		val: 1
	}
	c << BBbool{
		val: 2
	}
	println('${c}')
	assert '${c}' == '[1, 2]'
}
