import v.examples.submodule.mymodules { add_xy }
import v.examples.submodule.mymodules.submodule { sub_xy }

fn main() {
	println(add_xy(2, 3)) // expected: 5
	println(sub_xy(10, 7)) // expected: 3
}
