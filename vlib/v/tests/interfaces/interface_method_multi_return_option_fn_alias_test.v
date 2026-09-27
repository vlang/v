type Msg = string

type Cmd = fn () ?Msg

interface Model {
	update(msg Msg) (Model, ?Cmd)
}

struct ExampleModel {}

fn (_ ExampleModel) update(msg Msg) (Model, ?Cmd) {
	_ = msg
	panic('not implemented')
}

fn test_interface_method_multi_return_option_fn_alias() {
	_ := Model(ExampleModel{})
}
