struct Teste {
pub mut:
	teste ?&Teste
}

fn test_main() {
	mut a := Teste{}
	a.teste = &a
	dump(a) // circular

	assert a.teste? == &a

	mut t := ?Teste{}
	w := dump(t) // Option(none)
	assert w == none

	mut z := ?Teste{}
	z = Teste{}
	z?.teste = &z
	dump(z) // // circular

	y := z?
	assert y.teste? == &z?
}
