struct Foo {}

fn (f Foo) foo() string {
	return 'foo'
}

fn (f Foo) bar() string {
	return 'bar'
}

struct NamedFoo {
	name string
}

fn (f NamedFoo) foo() string {
	return 'foo ${f.name}'
}

fn (f NamedFoo) bar() string {
	return 'bar ${f.name}'
}

fn test_main() {
	f := Foo{}
	mut results := []string{}
	$for method in Foo.methods {
		x := fn [method, f] () string {
			return f.$method()
		}
		results << x()
	}
	assert results[0] == 'foo'
	assert results[1] == 'bar'
}

fn test_comptime_method_value() {
	f := NamedFoo{'rabbit'}
	mut results := []string{}
	$for method in NamedFoo.methods {
		x := f.$(method)
		results << x()
	}
	assert results[0] == 'foo rabbit'
	assert results[1] == 'bar rabbit'
}

@[heap]
struct MethodMapApp {
	prefix string
}

fn (app &MethodMapApp) alpha() string {
	return '${app.prefix}:alpha'
}

fn (app &MethodMapApp) beta() string {
	return '${app.prefix}:beta'
}

fn build_method_map[T](app &T) map[string]fn () string {
	mut syms := map[string]fn () string{}
	$for method in T.methods {
		syms[method.name] = app.$(method.name)
	}
	return syms
}

fn test_comptime_method_value_map() {
	app := MethodMapApp{'ctx'}
	syms := build_method_map(app)
	assert syms['alpha']() == 'ctx:alpha'
	assert syms['beta']() == 'ctx:beta'
}
