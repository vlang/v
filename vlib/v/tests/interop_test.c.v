// Not real external functions, so we won't call them
// We just want to make sure they compile

struct Foo {}

fn C.a(a string, b int) f32
fn C.b(a &voidptr)
fn C.c(a string, b ...string) string
fn C.d(a ...int)

// TODO: Should this be allowed?
fn C.g(string, ...int)
fn C.h(&int)

fn test_null() {
	np := C.NULL
	assert typeof(np).name == 'voidptr'
}
