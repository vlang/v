module main

fn test_generics_method_with_sumtype_args() {
	mut me := CatDad{}
	ret := me.adopt<CatType, Cat>(CatType.black, BlackCat{})
	println(ret)
	assert ret == 22
}

[flag]
enum CatType {
	black
	white
}

type Cat = BlackCat | WhiteCat

struct BlackCat {}

struct WhiteCat {}

struct CatDad {}

fn (mut foo CatDad) adopt<D, T>(d D, t T) int {
	return 22
}
