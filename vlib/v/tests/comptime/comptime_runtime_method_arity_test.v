import os
import veb

struct DispatchContext {
mut:
	called []string
}

struct DispatchApp {}

fn (app &DispatchApp) one(mut ctx DispatchContext) {
	ctx.called << 'one'
}

fn (app &DispatchApp) many(mut ctx DispatchContext, value string) {
	ctx.called << value
}

fn dispatch_by_arity[A](app &A, mut ctx DispatchContext, enabled bool, args []string) {
	$for method in A.methods {
		if method.args.len > 1 && enabled {
			app.$method(mut ctx, ...args)
		} else {
			app.$method(mut ctx)
		}
	}
}

fn dispatch_by_reversed_arity(app &DispatchApp, mut ctx DispatchContext, enabled bool,
	args []string) {
	$for method in DispatchApp.methods {
		if enabled && method.params.len > 1 {
			app.$method(mut ctx, ...args)
		} else {
			app.$method(mut ctx)
		}
	}
}

fn test_runtime_method_arity_guards() {
	app := &DispatchApp{}
	mut ctx := DispatchContext{}
	dispatch_by_arity(app, mut ctx, true, ['many'])
	assert ctx.called == ['one', 'many']
	ctx.called.clear()
	dispatch_by_arity(app, mut ctx, false, ['many'])
	assert ctx.called == ['one']
	ctx.called.clear()
	dispatch_by_reversed_arity(app, mut ctx, true, ['many'])
	assert ctx.called == ['one', 'many']
	ctx.called.clear()
	dispatch_by_reversed_arity(app, mut ctx, false, ['many'])
	assert ctx.called == ['one']
}

struct Context {
	veb.Context
mut:
	called string
}

struct ImplicitContextApp {}

fn (app &ImplicitContextApp) index() veb.Result {
	ctx.called = 'index'
	return veb.no_result()
}

fn (app &ImplicitContextApp) route(mut ctx Context, value string) veb.Result {
	ctx.called = value
	return veb.no_result()
}

struct OptionalApp {}

fn (app OptionalApp) optional(value ?string) string {
	return value or { 'optional' }
}

fn test_reflected_method_omits_optional_argument() {
	$for method in OptionalApp.methods {
		assert OptionalApp{}.$method() == 'optional'
	}
}

fn test_reflected_method_requires_optional_alias_argument() {
	root := os.join_path(os.vtmp_dir(), 'optional_alias_method_arity_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for parameter in ['MaybeInt', 'NestedMaybeInt'] {
		path := os.join_path(root, 'arity.v')
		source := 'type MaybeInt = ?int
type NestedMaybeInt = MaybeInt
struct Counter {
mut:
  hits int
}
fn (mut counter Counter) touch(value ${parameter}) { _ = value; counter.hits++ }
fn main() {
  mut counter := Counter{}
  \$for method in Counter.methods {
    counter.\$method()
  }
  assert counter.hits == 1
}
'
		os.write_file(path, source)!
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('expected 1 arguments to method Counter.touch, but got 0'), result.output
		os.write_file(path, source.replace('counter.\$method()', 'counter.\$method(?int(7))'))!
		control := os.exec([@VEXE, '-new-compiler', '-gc', 'none', 'run', path])
		assert control.exit_code == 0, control.output
	}
}

fn dispatch_implicit_context[A](app &A, mut ctx Context, name string, args []string) {
	$for method in A.methods {
		if method.name == name {
			if method.args.len > 1 {
				app.$method(mut ctx, ...args)
			} else {
				app.$method(mut ctx)
			}
		}
	}
}

fn test_runtime_method_dispatch_uses_implicit_veb_context() {
	app := &ImplicitContextApp{}
	mut ctx := Context{}
	dispatch_implicit_context(app, mut ctx, 'index', [])
	assert ctx.called == 'index'
	dispatch_implicit_context(app, mut ctx, 'route', ['route'])
	assert ctx.called == 'route'
}

fn test_runtime_dispatch_keeps_invalid_reflected_call_diagnostics() {
	root := os.join_path(os.vtmp_dir(), 'runtime_method_arity_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for condition in ['', 'if method.name == "zero"', 'if app.\$method() == 1',
		'if app.\$(method.name)() == 1'] {
		path := os.join_path(root, 'arity.v')
		call := 'app.\$method(1)'
		body := if condition == '' { call } else { '${condition} { ${call} }' }
		os.write_file(path, 'struct App {}
fn (app App) zero() int { return 1 }
fn main() {
  app := App{}
  \$for method in App.methods {
    ${body}
  }
}
')!
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('expected 0 arguments to method App.zero, but got 1'), result.output
	}
	or_path := os.join_path(root, 'always_selected.v')
	os.write_file(or_path, 'struct App {}
fn (app App) zero() int { return 1 }
fn (app App) one(value int) int { return value }
fn main() {
  app := App{}
  enabled := false
  \$for method in App.methods {
    if method.name == "zero" || enabled {
      app.\$method(1)
    }
  }
}
')!
	or_result := os.exec([@VEXE, '-new-compiler', '-check', or_path])
	assert or_result.exit_code != 0, or_result.output
	assert or_result.output.contains('expected 0 arguments to method App.zero, but got 1'), or_result.output
	for argument in ['mut value', 'pointer'] {
		path := os.join_path(root, 'pointer.v')
		os.write_file(path, 'struct Counter { value int }
struct App {}
fn (app App) change(mut pointer &Counter) {}
fn dispatch(app App, enabled bool) {
  mut value := Counter{}
  mut pointer := &value
  \$for method in App.methods {
    if method.name == "change" && enabled {
      app.\$method(${argument})
    }
  }
}
fn main() { dispatch(App{}, true) }
')!
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('method `change` parameter `pointer` is `mut'), result.output
	}
	for argument in ['ctx', 'mut number'] {
		path := os.join_path(root, 'context.v')
		os.write_file(path, 'import veb
struct Context {}
struct App {}
fn (app App) index() veb.Result { return veb.no_result() }
fn main() {
  mut ctx := Context{}
  mut number := 1
  app := App{}
  \$for method in App.methods {
    \$if method.name == "index" {
      app.\$method(${argument})
    }
  }
}
')!
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, result.output
		if argument == 'ctx' {
			assert result.output.contains('method `index` parameter `ctx` is `mut'), result.output
		} else {
			assert result.output.contains('cannot use `int`'), result.output
			assert result.output.contains('in argument 1 to `App.index`'), result.output
		}
	}
}
