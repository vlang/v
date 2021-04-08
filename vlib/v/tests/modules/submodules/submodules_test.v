import submodules
import submodules.test
import submodules.test.test2

fn main_test() {
	println('main')
	main := submodules.Main{
		a: 'main'
	}
	assert 'main' == main.a

	test := test.Test{
		a: 'test'
	}
	assert 'test' == test.a

	test2 := test2.Test{
		a: 'test2'
	}
	assert 'test2' == test2.a
}