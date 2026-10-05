import os

const forwarded_pointer_error_prelude = "module main

struct Context {
mut:
	value int
}

struct App {}

fn (_ App) index(mut ctx &Context) {
	ctx.value++
}

fn inner[A](app A, mut user_context Context) {
	\$for method in A.methods {
		\$if method.name == 'index' {
			app.\$method(mut user_context)
		}
	}
}

"

struct ForwardedPointerErrorCase {
	name   string
	source string
}

fn test_reflected_mut_pointer_storage_through_generic_forwarders() {
	root := os.join_path(os.vtmp_dir(), 'reflected_forwarded_pointer_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cases := [
		ForwardedPointerErrorCase{
			name:   'comptime_call_method_forwarded_mut_pointer_err'
			source: 'fn outer[A](app A, mut ctx Context) {
	inner[A](app, mut ctx)
}

fn main() {
	mut ctx := Context{}
	outer(App{}, mut ctx)
}
'
		},
		ForwardedPointerErrorCase{
			name:   'comptime_call_method_deep_forwarded_mut_pointer_err'
			source: 'fn middle[A](apps A, mut ctx Context) {
	inner(apps[0], mut ctx)
}

fn outer[A](app A, mut ctx Context) {
	middle[[]A]([app], mut ctx)
}

fn main() {
	mut ctx := Context{}
	outer(App{}, mut ctx)
}
'
		},
		ForwardedPointerErrorCase{
			name:   'comptime_call_method_cycle_forwarded_mut_pointer_err'
			source: 'fn outer[A](app A, mut ctx Context, depth int) {
	if depth > 0 {
		outer[A](app, mut ctx, depth - 1)
	} else {
		inner[A](app, mut ctx)
	}
}

fn main() {
	mut ctx := Context{}
	outer(App{}, mut ctx, 3)
}
'
		},
		ForwardedPointerErrorCase{
			name:   'comptime_call_method_generic_method_forwarded_mut_pointer_err'
			source: 'fn (app App) forward[X](mut ctx Context) {
	inner[App](app, mut ctx)
}

fn outer[A](app A, mut ctx Context) {
	app.forward[int](mut ctx)
}

struct HealthyApp {}

fn (_ HealthyApp) forward[X](mut ctx Context) {
	ctx.value++
}

fn main() {
	mut ctx := Context{}
	outer(HealthyApp{}, mut ctx)
	outer(App{}, mut ctx)
}
'
		},
	]
	for case in cases {
		path := os.join_path(root, '${case.name}.v')
		os.write_file(path, forwarded_pointer_error_prelude + case.source)!
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-cc',
			'clang', '-check', path])
		assert result.exit_code == 1, result.output
		assert result.output.count('error:') == 1, result.output
		assert result.output.contains('method `index` parameter `ctx` is `mut ctx &Context`'), result.output
		assert result.output.contains('pass `mut` of a `&Context` variable'), result.output
	}
}
