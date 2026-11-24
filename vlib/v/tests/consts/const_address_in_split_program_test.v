// vtest build: !musl? // gg needs GL/gl.h, which is not installed on the musl CIs
import gg

// With gg the program is compiled in several units (the cached program prefix and
// the program's own unit). A constant whose address is taken has one definition,
// so `&foo` is the same in the constant's initializer and here.
const foo = u32(1)
const bar = u32(2)
const keys = [&foo, &bar]!

fn test_address_of_a_constant_is_the_same_in_every_unit() {
	_ := gg.HorizontalAlign.left
	assert keys[0] == &foo
	assert keys[1] == &bar
	assert *keys[0] == 1
}
