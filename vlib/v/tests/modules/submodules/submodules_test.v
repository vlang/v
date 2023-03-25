import submodules
import submodules.test
import submodules.test.test2

fn test_main() {
	println('main')
	main := submodules.Main{
		a: 'main'
	}
	assert 'main' == main.a

	test_ := test.Test{
		a: 'test'
	}
	assert 'test' == test_.a

	test2_ := test2.Test2{
		a: 'test2'
	}
	assert 'test2' == test2_.a
}
