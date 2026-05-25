// Regression test for https://github.com/vlang/v/issues/27076 —
// `$for method in field.methods` inside `$for field in T.fields` evaluated
// `$if method.name == ...` against an empty method name and dropped the body.

struct Comp1 {
mut:
	a int
}

fn (mut c Comp1) on_init() {
	c.a = 42
}

struct Comp2 {
mut:
	b int
}

fn (mut c Comp2) on_init() {
	c.b = 42
}

struct Comp3 {
mut:
	c int
}

fn (mut c Comp3) on_init() {
	c.c = 42
}

struct Container {
mut:
	id   string
	cmp1 Comp1
	cmp2 Comp2
	cmp3 Comp3
	cmp4 Comp1
	cmp5 Comp2
	cmp6 Comp3
	cmp7 Comp1
	cmp8 Comp2
	cmp9 Comp3
}

fn test_comptime_method_dispatch_on_struct_fields() {
	mut app := Container{}
	$for field in Container.fields {
		$if field.is_struct {
			$for method in field.methods {
				$if method.name == 'on_init' {
					app.$(field.name).on_init()
				}
			}
		}
	}
	assert app.cmp1.a == 42
	assert app.cmp2.b == 42
	assert app.cmp3.c == 42
	assert app.cmp4.a == 42
	assert app.cmp5.b == 42
	assert app.cmp6.c == 42
	assert app.cmp7.a == 42
	assert app.cmp8.b == 42
	assert app.cmp9.c == 42
}
