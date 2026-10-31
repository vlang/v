struct CallbackBox[T] {
mut:
	items []T
}

fn (mut box CallbackBox[T]) add(callback T) {
	box.items << callback
}

fn (box CallbackBox[T]) get(index int) T {
	return box.items[index]
}

type AlphaCallback = fn () int
type BetaCallback = fn () int
type GammaCallback = fn () int

type AlphaVoidCallback = fn ()
type BetaVoidCallback = fn ()

fn named_beta_callback() int {
	return 2
}

fn test_generic_methods_for_function_aliases_with_identical_signatures() {
	mut alpha := CallbackBox[AlphaCallback]{}
	mut beta := CallbackBox[BetaCallback]{}
	mut gamma := CallbackBox[GammaCallback]{}
	alpha.add(fn () int { return 1 })
	beta.add(named_beta_callback)
	gamma.add(GammaCallback(fn () int { return 3 }))
	first := alpha.get(0)
	second := beta.get(0)
	third := gamma.get(0)
	assert first() == 1
	assert second() == 2
	assert third() == 3
}

fn test_generic_methods_for_void_function_aliases() {
	mut alpha := CallbackBox[AlphaVoidCallback]{}
	mut beta := CallbackBox[BetaVoidCallback]{}
	alpha.add(fn () { println('alpha') })
	beta.add(fn () { println('beta') })
	assert alpha.items.len == 1
	assert beta.items.len == 1
	first := alpha.get(0)
	second := beta.get(0)
	first()
	second()
}
