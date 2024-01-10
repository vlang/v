struct ABC {
	data int
}

type RABC = &ABC

fn do(abc RABC) {
	println(abc.data)
}

fn test_alias_to_ptr() {
	abc := &ABC{
		data: 5
	}
	do(abc)
}
