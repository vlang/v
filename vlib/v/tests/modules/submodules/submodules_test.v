import submodules
import submodules.test
import submodules.test.test2

fn test_main() {
	println('main')
	mod := submodules.Main{
		a: 'main'
	}
	assert 'main' == mod.a

	test_ := test.Test{
		a: 'test'
	}
	assert 'test' == test_.a

	test2_ := test2.Test2{
		a: 'test2'
	}
	assert 'test2' == test2_.a
}
