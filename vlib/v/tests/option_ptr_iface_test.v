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
	dump(t)
	assert t.teste? == &ITeste(t.teste?)
}
