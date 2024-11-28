struct Struct {
	a int
}

fn test_str() {
	mut data := map[string]?string{}
	data = {
		'name': ?string('andre')
		'age':  ?string('')
	}
	dump(data)
}

fn test_complex() {
	mut data := map[string]?Struct{}
	data = {
		'a': ?Struct{
			a: 1
		}
		'b': ?Struct{}
	}
	dump(data)
}

fn test_int() {
	mut data := map[string]?int{}
	data = {
		'a': ?int(0)
		'b': ?int(none)
	}
	dump(data)
}

fn test_f64() {
	mut data := map[string]?f64{}
	data = {
		'a': ?f64(0)
		'b': ?f64(none)
	}
	dump(data)
}
