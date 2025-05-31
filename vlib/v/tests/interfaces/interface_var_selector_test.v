interface Module {
	import_()
}

struct TestModule {}

fn (mod TestModule) import_() {
	println("I'm called!")
}

fn t_func(a voidptr, b &char) bool {
	return true
}

fn import_module[T]() bool {
	$if T !is Module {
		println("Type doesn't implement Module.")
		return
	}

	mod := Module(T{})

	return t_func(mod.import_, &char(T.name.str))
}

fn test_main() {
	assert import_module[TestModule]() == true
}
