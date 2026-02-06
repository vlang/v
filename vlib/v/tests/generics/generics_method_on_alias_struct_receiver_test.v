struct Base {}

pub fn (base Base) hello() string {
	return 'hello'
}

type Message = Base

fn hello[T](hello_impl T) string {
	return hello_impl.hello()
}

fn test_generic_method_on_alias_struct_receiver() {
	mut message := Message(Base{})
	ret1 := hello(message)
	println(ret1)
	assert ret1 == 'hello'

	ret2 := message.hello()
	println(ret2)
	assert ret2 == 'hello'
}
