import submodules
import submodules.test
import submodules.test.test2

fn main() {
	println('main')
	main := submodules.Main{
		a: 'main'
	}
	println(main)

	test := test.Test{
		a: 'test'
	}
	println(test)

	test2 := test2.Test{
		a: 'test2'
	}
	println(test2)
}