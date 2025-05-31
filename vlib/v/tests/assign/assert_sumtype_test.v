struct Moon {}

struct Mars {}

type World = Mars | Moon

fn test_assert_sumtype() {
	w := World(Moon{})
	assert w is Moon
	assert w !is Mars
}
