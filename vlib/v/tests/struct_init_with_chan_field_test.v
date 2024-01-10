struct Abc {
	def Def
}

struct Def {
	ch chan string
}

fn test_struct_init_with_chan_field() {
	abc1 := Abc{Def{}}
	println('printing abc1')
	println(abc1) // this works

	abc2 := Abc{}
	println('printing abc2')
	println(abc2) // this gives invalid memory access

	assert '${abc1}' == '${abc2}'
}
