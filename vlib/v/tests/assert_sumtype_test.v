struct Moon {}
struct Mars {}

type World = Moon | Mars

fn test_assert_sumtype() {
	w := World(Moon{})
	assert w is Moon
	assert w !is Mars
}
