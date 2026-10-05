module bridge

// outer forwards the receiver and mutable context through a generic container.
pub fn outer[A, C](app A, mut ctx C) {
	middle[[]A, C]([app], mut ctx)
}

fn middle[A, C](apps A, mut ctx C) {
	inner(apps[0], mut ctx)
}

fn inner[A, C](app A, mut user_context C) {
	mut pointer := &user_context
	$for method in A.methods {
		$if method.name == 'index' {
			app.$method(mut pointer)
		}
	}
}

// Context shares its source name with the caller-owned type.
pub struct Context {
pub mut:
	value int
}

pub struct App {}

// index updates the helper-owned context.
pub fn (_ App) index(mut ctx &Context) {
	ctx.value += 10
}
