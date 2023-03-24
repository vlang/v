fn f1(s string) fn (string) !string {
	return fn [s] (str string) !string {
		return s + str
	}
}

fn f2(s string) fn (string) ?string {
	return fn [s] (str string) ?string {
		return s + str
	}
}

fn f3(s string) fn (string) string {
	return fn [s] (str string) string {
		return s + str
	}
}

fn f4(s string) fn (string) ?string {
	return fn (str string) ?string {
		return none
	}
}

fn f5(s string) fn (string) !string {
	return fn (str string) !string {
		return error('test')
	}
}

fn test_call_nested_anon() {
	println(main.f1('V')('Lang')!)
	s1 := main.f1('V')('Lang')!
	println(s1)
	s2 := main.f1('V')('Lang') or { 'ErrLang' }
	println(s2)
	s3 := main.f2('V')('Lang')?
	println(s3)
	s4 := main.f2('V')('Lang') or { 'NoneLang' }
	println(s4)
	s := main.f3('V')('Lang')
	println(s)
	assert s == 'VLang'
	assert s1 == 'VLang'
	assert s2 == 'VLang'
	assert s3 == 'VLang'
	assert s4 == 'VLang'

	s5 := main.f4('V')('Lang') or { 'Lang++' }
	println(s5)
	assert s5 == 'Lang++'
	s6 := main.f5('V')('Lang') or { '${err}' }
	println(s6)
	assert s6 == 'test'
}
