import submodules
import submodules.test
import submodules.test.test2

fn test_main() {
	println('main')
	main := submodules.Main{
		a: 'main'
	}
	assert 'main' == main.a

	test := test.Test{
		a: 'test'
	}
	assert 'test' == test.a

	test2 := test2.Test2{
		a: 'test2'
	}
	assert 'test2' == test2.a
}
