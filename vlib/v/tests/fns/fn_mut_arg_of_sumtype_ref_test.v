module main

pub struct Expr1 {}

pub struct Expr2 {}

pub type Expr = Expr1 | Expr2

pub struct Gen {
}

fn (mut g Gen) foo(mut expr Expr1) string {
	return '${expr}'
}

pub fn (mut g Gen) bar(mut expr Expr) string {
	return match mut expr {
		Expr1 { g.foo(mut &expr) }
		Expr2 { '' }
	}
}

fn test_fn_mut_arg_of_sumtype_ref() {
	mut expr := &Expr1{}
	mut g := Gen{}
	ret := g.bar(mut expr)
	println(ret)
	assert ret == 'Expr1{}'
}
