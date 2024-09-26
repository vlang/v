interface ITeste {
mut:
	teste ?&ITeste
}

struct Teste {
pub mut:
	teste ?&ITeste
}

fn test_main() {
	mut t := Teste{}
	t.teste = &t
	z := &ITeste(t)
	dump(z)
	dump(t)
	assert voidptr(t.teste?) == voidptr(z.teste?)
}
