module main

type DataFn = fn (name string) string

fn which_lang(name string) string {
	return name
}

fn find_func(name string) ?DataFn {
	return match name {
		'vlang' { which_lang }
		else { none }
	}
}

fn test_main() {
	req := find_func('vlang')?
	fun := req('options')
	println(fun)
	assert fun == 'options'
}

fn test_ifguard() {
	if req2 := find_func('vlang') {
		assert req2('options') == 'options'
	}
}
