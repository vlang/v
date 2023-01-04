import mymodules { add_xy }
import mymodules.submodule { sub_xy }

fn main() {
	println(add_xy(2, 3)) // expected: 5
	println(sub_xy(10, 7)) // expected: 3
}
