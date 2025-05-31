struct Struct {
	f fn (f64) f64 = unsafe { nil }
}

struct App {}

pub fn (mut a App) frame(dt f64) f64 {
	dump(voidptr(a))
	dump(dt)
	return dt
}

fn generic_f[T](mut ctx T) ! {
	s := Struct{
		f: unsafe { ctx.frame }
	}
	assert s.f(1.2) == 1.2
}

fn test_main() {
	mut app := &App{}
	generic_f(mut app)!
	assert true
}
