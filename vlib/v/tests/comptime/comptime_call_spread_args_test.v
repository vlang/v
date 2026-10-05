struct SpreadContext {
mut:
	response string
}

struct SpreadApp {}

fn (app &SpreadApp) index(mut ctx SpreadContext) {
	ctx.response = 'index'
}

fn (app &SpreadApp) one(mut ctx SpreadContext, number int) {
	ctx.response = '${number}'
}

fn (app &SpreadApp) many(mut ctx SpreadContext, name string, number int) {
	ctx.response = '${name}: ${number}'
}

fn dispatch_spread_generic[T](app &T, mut ctx SpreadContext, name string, args []string) {
	$for method in T.methods {
		if method.name == name {
			app.$method(mut ctx, ...args)
			return
		}
	}
}

fn dispatch_spread_concrete(app &SpreadApp, mut ctx SpreadContext, name string, args []string) {
	$for method in SpreadApp.methods {
		if method.name == name {
			app.$method(mut ctx, ...args)
			return
		}
	}
}

fn test_generic_reflected_method_spread_args() {
	app := &SpreadApp{}
	mut ctx := SpreadContext{}
	dispatch_spread_generic(app, mut ctx, 'index', []string{})
	assert ctx.response == 'index'
	dispatch_spread_generic(app, mut ctx, 'one', ['42'])
	assert ctx.response == '42'
	dispatch_spread_generic(app, mut ctx, 'many', ['V', '3'])
	assert ctx.response == 'V: 3'
}

fn test_concrete_reflected_method_spread_args() {
	app := &SpreadApp{}
	mut ctx := SpreadContext{}
	dispatch_spread_concrete(app, mut ctx, 'index', []string{})
	assert ctx.response == 'index'
	dispatch_spread_concrete(app, mut ctx, 'one', ['42'])
	assert ctx.response == '42'
	dispatch_spread_concrete(app, mut ctx, 'many', ['V', '3'])
	assert ctx.response == 'V: 3'
}
