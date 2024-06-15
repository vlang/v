module main

import rand

struct MyStruct {
	foo int
}

const my_struct = MyStruct{rand.intn(6) or { panic(err) }}

fn main() {
	dump('hello')
}
