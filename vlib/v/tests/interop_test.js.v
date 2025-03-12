// vtest build: present_node?
// Not real external functions, so we won't call them
// We just want to make sure they compile

struct Foo {}

fn JS.e(a string, b ...string) int
fn JS.f(a &Foo)
fn JS.i(...string)

fn test_compiles() {
	assert true
}
