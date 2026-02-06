module test

fn C.GC_INIT()

const foo = 1
const bar = (foo << 5) + 9

@[export: Tatltuae]
pub fn test_tatltuae() int {
	return foo + bar
}
