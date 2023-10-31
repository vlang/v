struct Foo {
	s string
mut:
	i int
}

fn (f Foo) get_s() string {
	return f.s
}

fn (f &Foo) get_s_ref() string {
	return f.s
}

fn (f Foo) add(a int) int {
	return a + f.i
}

fn (f &Foo) add_ref(a int) int {
	return a + f.i
}

fn (mut f Foo) set(a int) {
	f.i = a
}

fn (f_ Foo) set_val(a int) int {
	mut f := unsafe { &f_ }
	old := f.i
	f.i = a
	return old
}

fn test_methods_as_fields() {
	mut f := Foo{
		s: 'hello'
		i: 1
	}

	get_s := f.get_s
	get_s_ref := unsafe { f.get_s_ref }
	add := f.add
	add_ref := unsafe { f.add_ref }
	set := unsafe { f.set }
	set_val := f.set_val

	assert typeof(get_s).str() == 'fn () string'
	assert typeof(get_s_ref).str() == 'fn () string'
	assert typeof(add).str() == 'fn (int) int'
	assert typeof(add_ref).str() == 'fn (int) int'

	assert get_s() == 'hello'
	assert get_s_ref() == 'hello'
	assert add(2) == 3
	assert add_ref(2) == 3

	assert f.i == 1
	set(2)
	assert f.i == 2
	old := set_val(3)
	assert f.i == 2
	new := set_val(5)
	assert old == new && old == 1
}

// the difference between these two tests is that here `f` is &Foo
fn test_methods_as_fields_ref() {
	mut f := &Foo{
		s: 'hello'
		i: 1
	}

	get_s := f.get_s
	get_s_ref := unsafe { f.get_s_ref }
	add := f.add
	add_ref := unsafe { f.add_ref }
	set := unsafe { f.set }
	set_val := f.set_val

	assert typeof(get_s).str() == 'fn () string'
	assert typeof(get_s_ref).str() == 'fn () string'
	assert typeof(add).str() == 'fn (int) int'
	assert typeof(add_ref).str() == 'fn (int) int'

	assert get_s() == 'hello'
	assert get_s_ref() == 'hello'
	assert add(2) == 3
	assert add_ref(2) == 3

	assert f.i == 1
	set(2)
	assert f.i == 2
	old := set_val(3)
	assert f.i == 2
	new := set_val(5)
	assert old == new && old == 1
}

struct GG_Ctx {
	frame_fn fn (voidptr) int
}

[heap]
struct App {
	msg string = 'hello'
}

fn (app &App) frame() int {
	return app.msg.len
}

fn test_ctx_arg_expected() {
	mut app := &App{}
	mut ctx := &GG_Ctx{
		frame_fn: app.frame
	}
	assert typeof(ctx.frame_fn).str() == 'fn (voidptr) int'
	assert ctx.frame_fn(app) == 5
}
